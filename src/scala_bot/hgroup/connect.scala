package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.utils._
import scala_bot.logger.Log

import scala.util.chaining.scalaUtilChainingOps
import scala_bot.logger.Logger
import scala_bot.logger.LogLevel

case class ConnectContext(
	looksDirect: Boolean,
	thinksStall: Set[Int],
	connected: List[Int],
	ignore: Set[Int] = Set()
)

case class ConnectOpts(
	knownOnly: Set[Int] = Set(),
	assumeTruth: Boolean = false,
	bluff: Boolean = false,
	findOwn: Option[Int] = None,
	nonTargetFinessed: Boolean = false,
	noLayer: Boolean = false,
)

def findKnownConn(ctx: ClueContext, giver: Int, id: Identity, ignore: Set[Int], findOwn: Boolean) =
	val game = ctx.game
	val state = game.state

	def validKnown(order: Int) =
		!ignore.contains(order) &&
		game.state.deck(order).matches(id, assume = true) &&
		game.common.thoughts(order).matches(id, infer = true) &&
		!game.meta(order).hidden &&
		!game.xmeta(order).maybeFinessed &&
		!game.common.linkedOrders(state).contains(order)

	def validLink(link: Link, order: Int) =
		link.matches {
			case Link.Promised(orders, i, _) => i == id && orders.contains(order)
			case Link.Sarcastic(orders, i) => i == id && orders.contains(order)
		}

	def validPlayable(playerIndex: Int, order: Int) =
		!ignore.contains(order) &&
		game.common.thoughts(order).inferred.contains(id) &&
		!(ctx.action.giver == state.ourPlayerIndex && game.xmeta(order).maybeFinessed) &&
		game.common.orderPlayable(game, order, excludeTrash = true) &&
		!{	// Don't connect on our unknown playables for an id matching the focus:
			// giver would be bad touching
			playerIndex == state.ourPlayerIndex &&
			state.deck(ctx.focusResult.focus).id().contains(id)
		}

	// Globally known
	val knownConns = for
		playerIndex <- (0 until state.numPlayers).view
		order <- state.hands(playerIndex) if validKnown(order)
	yield
		KnownConn(playerIndex, order, id)

	// Promised
	val linkedConns = for
		playerIndex <- (0 until state.numPlayers).view
		order <- state.hands(playerIndex)
		link <- game.common.links if validLink(link, order)
	yield
		PlayableConn(playerIndex, order, id, linked = link.getOrders.toList)

	// Log.info(s"finding known ${state.logId(id)} $ignore ${game.common.linkedOrders(state)} $findOwn")

	// Visible and going to be played (excludes giver)
	val playableConns = for
		playerIndex <- (0 until state.numPlayers) if playerIndex != giver
		playables = state.hands(playerIndex).filter(validPlayable(playerIndex, _))
		order <- playables if game.state.deck(order).matches(id, assume = game.allowFindOwn && findOwn) && game.isTouched(order)
	yield
		PlayableConn(playerIndex, order, id, linked = playables.toList)

	knownConns.headOption
	.orElse(linkedConns.headOption)
	.orElse(playableConns.headOption)

def findUnknownConnecting(ctx: ClueContext, reacting: Int, id: Identity, connected: List[Int], ignore: Set[Int], opts: ConnectOpts): Option[Connection] =
	val ClueContext(prev, game, action) = ctx
	val (state, level) = (game.state, game.level)
	val ClueAction(giver, target, _, _) = action
	val FocusResult(focus, _, _) = game.determineFocus(prev, action)

	// Log.info(s"finding unknown connecting for ${state.logId(id)} (${state.names(reacting)}), $connected, own? ${opts.findOwn} noLayer? ${opts.noLayer}")

	if opts.bluff then
		val clued = prev.common.findClued(prev, reacting, id, ignore ++ connected)
		val matched = clued.find(state.deck(_).matches(id, assume = opts.findOwn.isDefined))

		if matched.isDefined then
			return Some(PlayableConn(matched.get, reacting, id, linked = clued.toList))

	def tryPrompt(order: Int) =
		Option.when(!rainbowMismatch(prev, action, id, order, focus)) {
			opts.findOwn.map(game.players(_).thoughts(order).id()).getOrElse(state.deck(order).id()) match {
				case None =>
					Option.when(opts.findOwn.exists(game.players(_).thoughts(order).possible.contains(id)))
						(PromptConn(reacting, order, id))

				case Some(promptId) =>
					if promptId == id then
						Some(PromptConn(reacting, order, id))

					else if level >= Level.IntermediateFinesses && state.isPlayable(promptId) then
						Some(PromptConn(reacting, order, promptId, hidden = true))

					else
						// Log.warn(s"wrong prompt on ${state.logId(order)} $order, stacks ${state.playStacks}")
						None
			}
		}.flatten

	val prompt = prev.common.findPrompt(prev, reacting, id, connected, ignore)
	// Need to use 'game' to exclude newly clued cards
	val finesse = game.findFinesse(reacting, connected, ignore)

	if prompt.isDefined then
		return tryPrompt(prompt.get)

	// Try prompting a wrongly-ranked pink card
	val tryPinkPrompt = state.includesVariant(PINKISH) &&
		(level < Level.Bluffs || finesse.forall(game.common.thoughts(_).possible.forall(!state.isPlayable(_))))

	if tryPinkPrompt then
		val pinkPrompt = prev.common.findPrompt(prev, reacting, id, connected, ignore, forcePink = true)
		val pinkPromptConn = pinkPrompt.filter(!prompt.contains(_)).flatMap(tryPrompt)

		if pinkPromptConn.isDefined then
			return pinkPromptConn

	val cluedDupe = state.hands.zipWithIndex.exists { (hand, i) =>
		i != giver && hand.exists(o => prev.state.deck(o).clued && state.deck(o).matches(id))
	}

	if cluedDupe then
		Log.warn(s"disallowed finesse, ${state.logId(id)} already clued")
		return None

	// We'll remove an information channel if we finesse a maybe-finessed card
	val disallowFinesse = giver == state.ourPlayerIndex &&
		finesse.exists(prev.xmeta(_).maybeFinessed)

	if disallowFinesse then
		return None

	val nextFinesse = finesse.flatMap(f => prev.findFinesse(reacting, f +: connected, ignore))

	// Try to insert into an earlier finesse
	val insertable = opts.findOwn.isDefined && nextFinesse.isDefined && finesse.exists { f =>
		!game.common.thoughts(f).inferred.contains(id) &&
		game.meta(f).status == CardStatus.Finessed &&
		game.xmeta(f).finesseIds.exists(_.contains(id))
	}

	val knownLayeredIds = finesse.flatMap { f =>
		val future = game.future(f)
		val playableIds = future.intersect(game.state.playableSet)

		Option.when(!future.contains(id) && playableIds.nonEmpty)(playableIds)
	}

	finesse.flatMap(state.deck(_).id()) match {
		case None if finesse.exists(game.future(_).length == 1) =>
			val actualId = game.future(finesse.get).head

			Option.when(state.isPlayable(actualId))
				(FinesseConn(reacting, finesse.get, List(actualId), hidden = actualId != id))

		case None if knownLayeredIds.isDefined =>
			Some(FinesseConn(reacting, finesse.get, knownLayeredIds.get.toList, hidden = true))

		case None if insertable =>
			Some(FinesseConn(reacting, finesse.get, ids = List(id), inserted = true))

		case None if opts.findOwn.isDefined && finesse.isDefined =>
			val thought = game.players(opts.findOwn.get).thoughts(finesse.get)
			val bluffableIds = thought.inferred.filter { i =>
				validBluff(prev, action, i, id, reacting, connected)
			}
			val possiblyBluff = !opts.assumeTruth &&
				bluffableIds.nonEmpty &&
				thought.possible.contains(id)

			val trueFinesse = thought.infoLock.getOrElse(thought.possible).contains(id) &&
				thought.matches(id, assume = true)

			Option.when(trueFinesse || bluffableIds.nonEmpty) {
				val certain = state.hands(giver).exists { o =>
					val card = state.deck(o)
					card.matches(id) && card.clued
				}

				FinesseConn(
					reacting,
					finesse.get,
					ids = if trueFinesse then List(thought.id(infer = true).getOrElse(id)) else bluffableIds.toList,
					hidden = !thought.inferred.contains(id),
					bluff = !opts.assumeTruth && !thought.possible.contains(id),
					possiblyBluff = possiblyBluff,
					certain = certain)
			}

		case None => None

		case Some(finesseId) =>
			val possiblyBluff = !opts.assumeTruth &&
				validBluff(prev, action, finesseId, id, reacting, connected, symmetric = true)

			if finesseId == id || opts.findOwn.exists(i => i != state.ourPlayerIndex && game.players(i).thoughts(finesse.get).possible.contains(id)) then
				// At level 1, only forward finesses are allowed.
				if level == 1 && finesseId == id && !inBetween(state.numPlayers, reacting, giver, target) then
					Log.warn(s"found non-forward finesse ${state.logId(id)} in ${state.names(reacting)}'s hand at lv 1!")
					None
				else
					Some(FinesseConn(reacting, finesse.get, List(id), bluff = false, possiblyBluff = possiblyBluff))

			else if !opts.noLayer && level >= Level.IntermediateFinesses && state.isPlayable(finesseId) then
				val bluff = validBluff(prev, action, finesseId, id, reacting, connected)

				// TODO: Check in resolver that we don't give uncertain finesses or likely dupes
				val uncertainFinesse = !bluff &&
					state.hands(giver).exists(o => state.deck(o).clued && state.deck(o).matches(finesseId))

				Option.when(!uncertainFinesse)
					(FinesseConn(reacting, finesse.get, List(finesseId), hidden = !bluff, bluff = bluff, possiblyBluff = possiblyBluff))

			else
				None
	}

def findSingleConn(ctx: ClueContext, reacting: Int, id: Identity, connCtx: ConnectContext, opts: ConnectOpts, connections: List[Connection] = Nil): Option[List[Connection]] =
	val ClueContext(prev, game, action) = ctx
	val state = game.state
	val ClueAction(giver, target, _, _) = action

	lazy val allVisible = {
		val remaining = state.cardCount(id.toOrd) - state.baseCount(id.toOrd)

		// Everyone between giver and reacting must be able to see them,
		// otherwise they will try to react
		val visible = playersUntil(state.numPlayers, reacting, giver)
			.map(state.hands(_).count(state.deck(_).matches(id)))
			.sum

		remaining == visible
	}

	val skip = reacting == giver ||
		opts.knownOnly.contains(reacting) ||
		(reacting == target && connCtx.looksDirect && !allVisible) ||
		connCtx.thinksStall.contains(reacting)

	// TODO: When resolving, disallow prompting/finessing a player that may need to prove sth to us
	if skip then
		// Log.info(s"skipping ${state.names(reacting)}, giver $giver target $target $connCtx ")
		None
	else
		findUnknownConnecting(ctx, reacting, id, connCtx.connected, connCtx.ignore, opts) match {
			case None => None

			// Try again
			case Some(conn) if conn.hidden =>
				val selfClandestine = reacting == action.target &&
					conn.ids.head.next.exists(game.common.thoughts(ctx.focusResult.focus).possible.contains) &&
					// Someone else finessing will prove this is a clandestine self.
					!opts.nonTargetFinessed

				if selfClandestine then
					Log.warn("illegal clandestine self-finesse!")
					None
				else
					val hypo = state.deck(conn.order).id().orElse(Option.when(conn.ids.length == 1)(conn.ids.head))
						.fold(game)(id => game.withState(_.withPlay(id)).elim(goodTouch = true))

					val newConnCtx = connCtx.copy(connected = conn.order +: connCtx.connected)
					findSingleConn(ctx.copy(game = hypo), reacting, id, newConnCtx, opts, conn +: connections)

			case Some(conn) =>
				Some((conn +: connections).reverse)
		}

def findConnecting(ctx: ClueContext, id: Identity, connCtx: ConnectContext, opts: ConnectOpts): Option[List[Connection]] =
	val ClueContext(prev, game, action) = ctx
	val state = game.state
	val ClueAction(giver, target, _, _) = action

	// Log.highlight(Console.YELLOW, s"find connecting ${state.logId(id)}")

	if state.baseCount(id.toOrd) == state.cardCount(id.toOrd) then
		Log.info(s"all ${state.logId(id)} in trash!")
		return None

	val known = findKnownConn(ctx, giver, id, connCtx.ignore ++ connCtx.connected, opts.findOwn.isDefined)

	if known.isDefined then
		return Some(known.toList)

	val connPlayerOrder = List(
		List(target),	// first, try without layered
		(0 until state.numPlayers)
			.map(i => (giver - i - 1 + state.numPlayers) % state.numPlayers)
			.filterNot(_ == target),
		List(target),	// allow self-layered if no one will fix a self-clandestine
	).flatten

	connPlayerOrder.zipWithIndex.view.map { (reacting, i) =>
		findSingleConn(ctx, reacting, id, connCtx, opts.copy(noLayer = i == 0))
	}
	.find(_.isDefined)
	.flatten

def validBluff(game: HGroup, action: ClueAction, blind: Identity, truth: Identity, reacting: Int, connected: List[Int], symmetric: Boolean = false) =
	val state = game.state
	val ClueAction(giver, target, _, clue) = action
	val focus = connected.head

	lazy val disconnect = symmetric ||
		(clue.kind == ClueKind.Rank && clue.value != blind.rank + 1) ||
		blind.next.forall(!game.common.thoughts(focus).possible.contains(_))

	lazy val interferes = state.hands(reacting).exists { o =>
		game.players(reacting).thoughts(o).possible.contains(truth) &&
		(game.isBlindPlaying(o) || game.meta(o).status == CardStatus.GentlemansDiscard)
	}

	game.level >= Level.Bluffs &&
	state.nextPlayerIndex(giver) == reacting &&
	connected.length == 1 &&
	disconnect &&
	!(clue.kind == ClueKind.Colour && reacting == target) &&	// not self-colour bluff
	!interferes

/** Returns whether the colour clue could be a save on the given identity. */
def colourSave(prev: HGroup, action: ClueAction, id: Identity, focus: Int): Boolean =
	val state = prev.state
	val ClueAction(giver, target, list, clue) = action
	val Identity(suitIndex, rank) = id

	val thought = prev.common.thoughts(focus)
	val suit = state.variant.suits(suitIndex)

	if !state.variant.cardTouched(id, clue) || !thought.possible.contains(id) || state.isBasicTrash(id) then
		return false

	if rank == 5 && suit != "Black" && !BROWNISH.matches(suit) then
		return false

	if suit == "Black" && (rank == 2 || rank == 5) then
		// Newly touched or fill-in cards
		val fillIns = list.count { o =>
			!state.deck(o).clued ||
			prev.common.thoughts(o).possible.exists(!state.variant.idTouched(_, clue))
		}

		// Trash that would be picked up by a rank clue
		val trash = state.hands(target).count { o =>
			val card = state.deck(o)
			!card.clued && card.id().exists(i => i.rank == rank && state.isBasicTrash(i))
		}

		if fillIns < 2 && trash == 0 then
			return false

	if BROWNISH.matches(suit) && prev.common.thinksLoaded(prev, giver) then
		return false

	if "Dark Rainbow|Dark Prism".r.matches(suit) then
		val completed = prev.common.hypoStacks(suitIndex) == state.maxRanks(suitIndex)
		val savedCrit = list.exists { o =>
			val card = state.deck(o)
			!card.clued && card.id().exists { i =>
				state.isCritical(i) && i.rank != 5 &&
				"Dark Rainbow|Dark Prism".r.matches(state.variant.suits(i.suitIndex))
			}
		}

		if !completed && !savedCrit then
			return false

	// If there is a dark colour, save with that, otherwise red.
	val muddySaveColour = if !state.includesVariant("Black|Dark Brown|Dark Pink".r) then 0 else
		state.variant.suits.length - 2

	// Note that critical 2,3,4 can be saved with anything.
	if suit.contains("Muddy") && clue.value != muddySaveColour && !(state.isCritical(id) && Set(2,3,4).contains(rank)) then
		return false

	if suit.contains("Cocoa") && clue.value != muddySaveColour then
		return false

	state.isCritical(id) || (BROWNISH.matches(suit) && rank == 2)

def rankSave(prev: HGroup, action: ClueAction, id: Identity, focus: Int): Boolean =
	val state = prev.state
	val ClueAction(giver, target, list, clue) = action
	val Identity(suitIndex, rank) = id
	val thought = prev.common.thoughts(focus)

	if !thought.possible.contains(id) || state.isBasicTrash(id) then
		return false

	// Don't consider save on k3,k4 (or dark i3,i4) with rank
	// TODO: Florrat Save
	if "Black|Dark Pink".r.matches(state.variant.suits(suitIndex)) && (rank == 3 || rank == 4) then
		return false

	val loaded34 = prev.common.thinksLoaded(prev, giver) &&
		(state.includesVariant(WHITISH) || state.includesVariant("Dark Rainbow|Dark Prism".r)) &&
		(rank == 3 || rank == 4)

	if loaded34 then
		return false

	state.isCritical(id) || rank == 2

def connect(ctx: ClueContext, id: Identity, looksDirect: Boolean, thinksStall: Set[Int], assumeTruth: Boolean = false, findOwn: Option[Int] = None): Option[FocusPossibility] =
	val ClueContext(prev, game, action) = ctx
	val state = game.state
	val FocusResult(focus, _, positional) = ctx.focusResult
	val Identity(suitIndex, rank) = id
	var attemptedBluff = false

	// Log.info(s"trying to connect ${state.logId(id)}${if (looksDirect) " (looks direct)" else ""} $findOwn")

	@annotation.tailrec
	def loop(hypo: HGroup, nextRank: Int, connections: List[Connection]): Option[List[Connection]] =
		if nextRank >= rank then
			return Some(connections)

		val nextId = Identity(suitIndex, nextRank)
		val connected = focus +: connections.map(_.order)
		val direct = looksDirect && {
				(action.clue.kind == ClueKind.Colour &&
					game.common.thoughts(focus).possible.contains(nextId)) ||
				!connections.existsM {
					case f: FinesseConn => f.reacting != action.target && !f.hidden
				}
			}
		val bluffed = connections.existsM { case f: FinesseConn => f.bluff }
		attemptedBluff ||= bluffed

		val nonTargetFinessed = connections.exists(c => c.isInstanceOf[FinesseConn] && c.reacting != action.target)
		val opts = ConnectOpts(assumeTruth = assumeTruth, bluff = bluffed, nonTargetFinessed = nonTargetFinessed)
		val connCtx = ConnectContext(direct, thinksStall, connected)
		val newCtx = ctx.copy(game = hypo)

		val conn = findConnecting(newCtx, nextId, connCtx, opts)
			.orElse(findOwn.flatMap { i =>
				val known = findKnownConn(newCtx, ctx.action.giver, nextId, connCtx.ignore ++ connCtx.connected, findOwn = true)

				// See if we need to correct based on future information
				val actualKnown = known match {
					case Some(c @ PlayableConn(reacting, order, id, linked, layered)) =>
						if linked.forall(!game.future(_).contains(id)) then
							Log.highlight(Console.CYAN, "playable conn is known to not match in the future, finding again")
							val unknown = findSingleConn(newCtx, i, nextId, connCtx.copy(connected = order +: connCtx.connected), opts.copy(findOwn = findOwn))
							unknown.map(c.copy(id = game.future(order).intersect(game.state.playableSet).head) +: _)
						else
							Some(List(c))
					case k => k.map(List(_))
				}

				actualKnown.orElse {
					findSingleConn(newCtx, i, nextId, connCtx, opts.copy(findOwn = findOwn))
				}
			})

		conn match {
			case None =>
				// Log.info(s"failed connection to ${state.logId(id)}: ${state.logConns(connections, nextId)} $findOwn")
				None
			case Some(conns) =>
				// Log.info(s"found conns ${state.logConns(conns)} $findOwn")
				val newGame = conns.foldLeft(hypo) { (acc, conn) =>
					acc.state.deck(conn.order).id()
						.orElse(Option.when(conn.ids.length == 1)(conn.ids.head))
						.fold(acc) { i =>
							val action = PlayAction(state.holderOf(conn.order), conn.order, i.suitIndex, i.rank)
							val level = Logger.level

							Logger.setLevel(LogLevel.Error)

							// Resolve wcs after playing the card
							val res = acc.onPlay(action)
								.pipe(refreshWCs(acc, _, action))
								.elim(goodTouch = true)
							Logger.setLevel(level)

							res
						}
				}
				loop(newGame, nextRank + 1, connections ++ conns)
		}

	loop(game, state.playStacks(suitIndex) + 1, List()).flatMap { conns =>
		val invalidInsert = conns.existsM { case f: FinesseConn =>
			f.inserted &&
			game.findFinesse(f.reacting, focus +: conns.map(_.order)).isEmpty
		}

		if invalidInsert then
			// Log.warn("illegal insert, no space for displaced connection!")
			None
		else
			val symmetric = !state.deck(focus).matches(id, assume = true) ||
				!game.players(action.target).thoughts(focus).possible.contains(id) ||
				conns.exists(c => state.deck(c.order).id().exists(!c.ids.contains(_)))

			Log.info(s"found connections: ${state.logConns(conns, id)}${if symmetric then " (symmetric)" else ""}${if conns.existsM { case f: FinesseConn => f.inserted } then " (inserted)" else ""}")
			Some(FocusPossibility(id, conns, ClueInterp.Play, symmetric))
	}
	.orElse {
		Option.when (game.level > Level.Bluffs && !assumeTruth && attemptedBluff) {
			Log.highlight(Console.YELLOW, "bluff connection failed, retrying true finesse")

			connect(ctx, id, looksDirect, thinksStall, assumeTruth = true, findOwn)
		}.flatten
	}
