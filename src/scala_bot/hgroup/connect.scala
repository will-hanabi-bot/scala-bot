package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.lib.FastBitSet
import scala_bot.utils._

import scala_bot.logger.{Log, Logger, LogLevel}

case class ConnectContext(
	looksDirect: Boolean,
	thinksStall: FastBitSet,
	connected: FastBitSet,
	ignore: FastBitSet = FastBitSet.empty
)

case class ConnectOpts(
	knownOnly: FastBitSet = FastBitSet.empty,
	assumeTruth: Boolean = false,
	bluff: Boolean = false,
	findOwn: Option[Int] = None,
	nonTargetFinessed: Boolean = false,
	noLayer: Boolean = false,
	preferOwn: Boolean = false,
	insertingInto: Option[Seq[Int]] = None
)

def findKnownConn(ctx: ClueContext, id: Identity, finalId: Identity, ignore: FastBitSet, findOwn: Boolean, preferOwn: Boolean = false): Option[Connection] =
	val ClueContext(prev, game, action, _) = ctx
	val (common, state) = (game.common, game.state)
	val giver = action.giver

	// Try looking for a [r1, r2] card that can be disproved if someone else doesn't play
	val ownPlay = if !preferOwn then None else
		var wcExists = false

		state.ourHand.find: o =>
			!ignore.contains(o) &&
			common.thoughts(o).possible.contains(id) &&
			common.thoughts(o).inferred.forall: i =>
				state.isPlayable(i) ||
				game.waiting.exists: wc =>
					val conn = wc.connections.find(!_.hidden).get
					val valid =
						i.prev.exists(conn.ids.contains) &&
						conn.reacting != state.ourPlayerIndex &&
						game.xmeta(conn.order).fStatus.contains(FStatus.PossiblyOn(state.ourPlayerIndex))

					if valid then
						wcExists = true

					valid
			&& wcExists
		.map: o =>
			KnownConn(state.ourPlayerIndex, o, id)

	if ownPlay.isDefined then
		return ownPlay

	val globallyKnown = state.heldOrders.findSome: o =>
		// Check whether the giver knows their card isn't a crit that they are currently cluing
		lazy val asymmetricKnown =
			state.holderOf(o) == giver &&
			state.isCritical(finalId) &&
			common.thoughts(o).inferred.forall: i =>
				i == id || i == finalId

		val validKnown =
			!ignore.contains(o) &&
			game.state.deck(o).matches(id, assume = !game.common.thoughts(o).reset) &&
			(common.thoughts(o).matches(id, infer = true) || asymmetricKnown) &&
			!(!prev.state.deck(o).clued && common.thoughts(o).possible.length > 1 && common.thoughts(ctx.focusResult.focus).inferred.contains(id)) &&
			!game.meta(o).hidden &&
			!game.xmeta(o).fStatus.contains(FStatus.PossiblyOn(giver)) &&
			(game.assumePlays || !game.xmeta(o).fStatus.contains(FStatus.PossiblyOn(state.ourPlayerIndex))) &&
			!common.isLinked(state, o)

		Option.when(validKnown)(KnownConn(state.holderOf(o), o, id))

	if globallyKnown.isDefined then
		return globallyKnown

	val promised = state.heldOrders.findSome: o =>
		if ignore.contains(o) || common.thoughts(o).inferred.difference(state.playableSet).nonEmpty then
			None
		else
			val link = common.links.find: link =>
				link.matchesP:
					case Link.Promised(orders, i, _) => i == id && orders.contains(o)
					case Link.Sarcastic(orders, i)   => i == id && orders.contains(o)

			link.map: l =>
				PlayableConn(state.holderOf(o), o, id, linked = l.getOrders.toList)

	if promised.isDefined then
		return promised

	// Log.info(s"finding known ${state.logId(id)} ${ignore.fmt} ${common.linkedOrders(state)} $findOwn")

	def validPlayable(playerIndex: Int, order: Int) =
		!ignore.contains(order) &&
		common.thoughts(order).inferred.contains(id) &&
		!game.xmeta(order).fStatus.contains(FStatus.PossiblyOn(giver)) &&
		(game.assumePlays || !game.xmeta(order).fStatus.contains(FStatus.PossiblyOn(state.ourPlayerIndex))) && {
			common.orderKp(game, order, excludeTrash = true) || {
				// Mimic common.orderPlayable, except we can also try playing previously-known unrelated plays
				!game.meta(order).trash &&
				!common.isLinked(state, order, unpromisedOnly = true) && {
					val poss = common.thoughts(order).possibilities.difference(state.trashSet)

					poss.nonEmpty && poss.forall: i =>
						state.isPlayable(i) ||
						(state.playStacks(i.suitIndex) + 1 until i.rank).forall: rank =>
							val id = Identity(i.suitIndex, rank)
							common.hypoPlays.exists(state.deck(_).matches(id))
				}
			}
		} &&
		!(	// Don't connect on our unknown playables for an id matching the focus:
			// giver would be bad touching
			playerIndex == state.ourPlayerIndex &&
			state.deck(ctx.focusResult.focus).id().contains(id)
		)

	// Visible and going to be played (excludes giver)
	val playable = state.hands.zipWithIndex.findSome: (hand, playerIndex) =>
		if playerIndex == giver then None else
			val playables = hand.filter(validPlayable(playerIndex, _))
			val playableOrder =
				playables.filter: p =>
					state.deck(p).matches(id, assume = game.allowFindOwn && findOwn) && game.isTouched(p)
				.minByOption: p =>
					// Prefer playing the option with the most info first
					val thought = game.common.thoughts(p)
					thought.infoLock.getOrElse(thought.possibilities).length

			playableOrder.map(PlayableConn(playerIndex, _, id, linked = playables.toList))

	if playable.isDefined then
		return playable

	val playLinked = state.heldOrders.findSome: o =>
		val playerIndex = state.holderOf(o)
		val play =
			!ignore.contains(o) &&
			playerIndex != giver &&
			state.deck(o).clued &&
			common.playLinks.exists(_.target == o) &&
			common.thoughts(o).inferred.contains(id) &&
			state.deck(o).matches(id, assume = game.allowFindOwn && findOwn)

		Option.when(play):
			PlayableConn(playerIndex, o, id, linked = List(o))

	if playLinked.isDefined then
		return playLinked

	val assumeKnown = state.hands(giver).findSome: o =>
		// Maybe someone clued 2 to us and the giver knows their 1s must connect
		val playLink = common.playLinks.find(pl => pl.orders.contains(o) && state.ourHand.contains(pl.target))

		// Maybe someone clued a colour to us (in an omni variant) and the giver knows their 1 must connect
		val waitingConn = game.waiting.find: wc =>
			val giverConn = wc.connections.indexWhere(c => c.order == o && c.ids.length == 1 && c.ids.contains(id))

			giverConn != -1 &&
			wc.giver != giver &&
			(wc.connections.drop(giverConn + 1).exists(_.reacting == state.ourPlayerIndex) || wc.target == state.ourPlayerIndex)

		val play =
			!ignore.contains(o) &&
			state.deck(o).clued &&
			(playLink.isDefined || waitingConn.isDefined) &&
			common.thoughts(o).inferred.contains(id) &&
			state.deck(o).matches(id)

		Option.when(play):
			PlayableConn(giver, o, id, linked = playLink.map(_.orders).getOrElse(Nil))

	assumeKnown

def findUnknownConnecting(ctx: ClueContext, reacting: Int, id: Identity, connected: FastBitSet, ignore: FastBitSet, opts: ConnectOpts): Option[Connection] =
	val ClueContext(prev, game, action, _) = ctx
	val (state, level) = (game.state, game.level)
	val ClueAction(giver, target, _, _) = action

	// val flags = List(
	// 	opts.findOwn.map(own => s"own ($own)"),
	// 	Option.when(opts.noLayer)("no layer"),
	// 	opts.insertingInto.map(i => s"inserting into $i"),
	// 	Option.when(opts.bluff)("bluff")
	// ).flatten.mkString(", ")
	// Log.info(s"finding unknown connecting for ${state.logId(id)} (${state.names(reacting)}), ${connected.fmt}, flags: [$flags]")

	if opts.bluff then
		val clued = prev.common.findClued(prev, reacting, id, ignore.union(connected))
		val matched = clued.find(state.deck(_).matches(id, assume = opts.findOwn.isDefined))

		return matched.map: order =>
			PlayableConn(reacting, order, id, linked = clued.toList)

	def tryPrompt(order: Int) =
		opts.findOwn.map(game.players(_).thoughts(order).id()).getOrElse(state.deck(order).id()) match
			case None =>
				Option.when(opts.findOwn.exists(game.players(_).thoughts(order).possible.contains(id)))
					(PromptConn(reacting, order, id))

			case Some(promptId) =>
				if promptId == id then
					Some(PromptConn(reacting, order, id))

				else if level >= Level.IntermediateFinesses && state.isPlayable(promptId) then
					if game.common.orderPlayable(game, order, excludeTrash = true) then
						Some(PlayableConn(reacting, order, promptId, hidden = true))
					else
						Some(PromptConn(reacting, order, promptId, hidden = true))

				else
					// Log.warn(s"wrong prompt on ${state.logId(order)} $order, stacks ${state.playStacks}")
					None

	// Need to use 'game' to exclude non-omni cards after a colour clue
	// But we also can't use newly clued cards
	val prompt = game.common.findPrompt(game, reacting, id, connected, ignore.union(state.hands(reacting).filter(!prev.state.deck(_).clued)))
	// Need to use 'game' to exclude newly clued cards
	val potentialFinesse = game.findFinesseId(
		reacting,
		id,
		connected,
		ignore,
		overrideLayer = opts.insertingInto.exists(_.exists(prev.state.hands(reacting).contains))
	)

	if prompt.exists(!rainbowMismatch(prev, game, action, id, _)) then
		return tryPrompt(prompt.get)

	// Try prompting a wrongly-ranked pink card
	val tryPinkPrompt = state.variant.pinkish &&
		potentialFinesse.forall: f =>
			if level < Level.Bluffs then
				!game.common.thoughts(f).possible.intersect(state.playableSet).exists: i =>
					state.variant.suits(i.suitIndex).suitType.pinkish
			else
				game.common.thoughts(f).possible.intersect(state.playableSet).isEmpty

	if tryPinkPrompt then
		val pinkPrompt = prev.common.findPrompt(prev, reacting, id, connected, ignore, forcePink = true)
		val pinkPromptConn = pinkPrompt.filter(!prompt.contains(_)).flatMap(tryPrompt)

		if pinkPromptConn.isDefined then
			return pinkPromptConn

	val cluedDupe = state.hands.zipWithIndex.exists: (hand, i) =>
		i != giver && hand.exists(o => prev.state.deck(o).clued && state.deck(o).matches(id))

	if cluedDupe then
		// Log.warn(s"disallowed finesse, ${state.logId(id)} already clued")
		return None

	// Try to insert into an earlier finesse
	val cantInsert = game.level < Level.IntermediateFinesses ||
		!opts.findOwn.contains(reacting) ||
		state.isInverted(id) ||
		potentialFinesse.exists(game.common.thinksInverted(state, _))

	val insertingInto = if cantInsert then None else
		val prevHand = state.hands(reacting).dropWhile: f =>
			game.meta(f).status != CardStatus.Finessed ||
			game.xmeta(f).finesseIds.isEmpty ||
			game.future(f).length == 1

		if prevHand.isEmpty then None else
			// Checks whether all inserting orders can be placed
			def insertable(hypo: HGroup, insertOrders: Seq[Int]): Boolean =
				insertOrders.foldLeftOpt(connected): (newConnected, o) =>
					val finesseIds = hypo.xmeta(o).finesseIds.get
					val nextFinesse = hypo.findFinesse(reacting, newConnected, ignore)

					// Log.info(s"checking that $o (${finesseIds.fmt(state)}) can be shifted to $nextFinesse (${nextFinesse.map(hypo.common.strPoss(hypo.state, _))})")

					// At least one id is possible on the next card on finesse position
					val possible = nextFinesse.exists(o => hypo.future(o).intersect(hypo.common.thoughts(o).possible).intersect(finesseIds).nonEmpty)

					Either.cond(possible, newConnected.incl(nextFinesse.get), FastBitSet.empty)
				.nonEmpty

			def canInsert(prevHand: Vector[Int], insertOrders: Seq[Int], firstInsert: Boolean): Boolean =
				prevHand.nonEmpty && insertOrders.nonEmpty && {
					val replacement = prevHand.head
					val possibleIds = if firstInsert then game.xmeta(replacement).finesseIds.get else game.common.thoughts(replacement).possible

					// Log.info(s"trying to insert ${state.logId(id)} at $replacement with prev hand $prevHand, shifting $insertOrders ${possibleIds.fmt(state)}")

					possibleIds.contains(id) &&
					insertable(game.withState(s => s.copy(hands = s.hands.updated(reacting, prevHand.tail))), insertOrders)
				}

			opts.insertingInto match
				case None =>
					val insertOrders = prevHand.filter(o => game.meta(o).status == CardStatus.Finessed && !game.meta(o).hidden)
					Option.when(canInsert(prevHand, insertOrders, firstInsert = true)):
						(prevHand.head, insertOrders)

				case Some(insertOrders) =>
					val prevHand2 = state.hands(reacting).dropWhile(_ > insertOrders.head)
					Option.when(canInsert(prevHand2, insertOrders, firstInsert = false)):
						(prevHand2.head, insertOrders)

	val knownLayeredIds = potentialFinesse.flatMap: f =>
		val future = game.future(f)
		val playableIds = future.intersect(game.state.playableSet)

		Option.when(!future.contains(id) && playableIds.nonEmpty)(playableIds)

	// We previously inserted, but we can't insert any more: leave a spot for the replacements
	val finesse =
		if opts.insertingInto.isDefined && insertingInto.isEmpty then
			val insertOrders = opts.insertingInto.get
			val prevHand = state.hands(reacting).dropWhile(_ > insertOrders.head)
			val hypo = prev.withState(s => s.copy(hands = s.hands.updated(reacting, prevHand)))

			val newConnected = (0 until insertOrders.length).foldLeftOpt(connected): (newConnected, _) =>
				hypo.findFinesse(reacting, newConnected, ignore) match
					case None => Left(FastBitSet.empty)
					case Some(f) => Right(newConnected.incl(f))

			if newConnected.isEmpty then None else
				game.findFinesseId(reacting, id, newConnected, ignore, overrideLayer = opts.insertingInto.exists(_.exists(prev.state.hands(reacting).contains)))
		else
			potentialFinesse

	finesse.flatMap(state.deck(_).id()) match
		case _ if finesse.exists(o => game.future(o).length < game.common.thoughts(o).possible.length) =>
			val inverted = state.isInverted(id)
			val possibleIds = if inverted then state.trashSet.union(state.playableSet.filter(state.isInverted)) else state.playableSet
			val futureIds = game.future(finesse.get).intersect(possibleIds)

			if futureIds.isEmpty then
				// Log.warn(s"future knowledge of ${finesse.get} is [${game.future(finesse.get).fmt(state)}], but all unplayable!")
				None
			else
				val fKind =
					if !opts.assumeTruth && futureIds.exists(validBluff(game, action, _, reacting, connected)) then
						FinesseKind.Bluff
					else if futureIds.isExactly(id) then
						FinesseKind.True
					else
						FinesseKind.Hidden

				Some(FinesseConn(reacting, finesse.get, futureIds.toList, fKind, inverted = inverted))

		case None if finesse.isDefined && knownLayeredIds.isDefined =>
			Some(FinesseConn(reacting, finesse.get, knownLayeredIds.get.toList, FinesseKind.Hidden))

		case _ if insertingInto.isDefined =>
			val (order, inserts) = insertingInto.get
			Some(PlayableConn(reacting, order, id, insertingInto = Some(inserts)))

		case None if opts.findOwn.isDefined && finesse.isDefined =>
			if prev.meta(finesse.get).status == CardStatus.Finessed && !prev.common.thoughts(finesse.get).reset && game.common.thoughts(finesse.get).reset then
				// Attempted to replace this finessed card with a new finessed card: this player cannot be asked to play any cards
				None
			else
				val thought = game.players(opts.findOwn.get).thoughts(finesse.get)
				val finesseIds = game.xmeta(finesse.get).finesseIds

				if finesseIds.exists(!_.contains(id)) then
					Some(FinesseConn(reacting, finesse.get, thought.inferred.toList, FinesseKind.Hidden))
				else
					val bluffableIds = thought.inferred.filter: i =>
						state.isPlayable(i) &&
						validBluff(game, action, i, reacting, connected)

					val trueFinesse = thought.infoLock.getOrElse(thought.possible).contains(id) &&
						thought.matches(id, assume = true)

					Option.when(trueFinesse || bluffableIds.nonEmpty):
						val certain = state.hands(giver).exists: o =>
							val card = state.deck(o)
							card.matches(id) && card.clued

						val fKind =
							if !opts.assumeTruth && bluffableIds.nonEmpty then
								// if !thought.possible.contains(id) then FinesseKind.Bluff else FinesseKind.PossiblyBluff
								FinesseKind.Bluff
							else if certain then
								FinesseKind.Certain
							else if !thought.inferred.contains(id) then
								FinesseKind.Hidden
							else
								FinesseKind.True

						val ids = if trueFinesse then
							List(thought.id(infer = true).getOrElse(id))
						else
							game.common.thoughts(finesse.get).inferred.filter: i =>
								state.isPlayable(i) &&
								validBluff(game, action, i, reacting, connected)
							.union(id)
							.toList

						FinesseConn(reacting, finesse.get, ids, fKind)

		case None => None

		case Some(finesseId) if finesseId == id =>
			if level == 1 && !inBetween(state.numPlayers, reacting, giver, target) then
				Log.warn(s"found non-forward finesse ${state.logId(id)} in ${state.names(reacting)}'s hand at lv 1!")
				None
			else
				val possiblyBluff = !opts.assumeTruth &&
					validBluff(game, action, finesseId, reacting, connected)

				Some(FinesseConn(reacting, finesse.get, List(id), fKind = if possiblyBluff then FinesseKind.Bluff else FinesseKind.True))

		case Some(finesseId) =>
			if Vector(CardStatus.GentlemansDiscard, CardStatus.Sarcastic).contains(game.meta(finesse.get).status) && state.isPlayable(finesseId) && !state.isInverted(finesseId) && !state.isInverted(id) then
				Some(PlayableConn(reacting, finesse.get, finesseId, hidden = true))
			else
				val possiblyBluff = !opts.assumeTruth &&
					validBluff(game, action, finesseId, reacting, connected)

				if opts.findOwn.exists(i => i != state.ourPlayerIndex && game.players(i).thoughts(finesse.get).possible.contains(id)) then
					Some(FinesseConn(reacting, finesse.get, List(id), fKind = if possiblyBluff then FinesseKind.Bluff else FinesseKind.True))

				else if !opts.noLayer && level >= Level.IntermediateFinesses && state.isPlayable(finesseId) && !state.isInverted(finesseId) && !state.isInverted(id) then
					if game.meta(finesse.get).status == CardStatus.Finessed && game.isDefinite(finesse.get) then
						Some(PlayableConn(reacting, finesse.get, finesseId, hidden = true))
					else
						// TODO: Check in resolver that we don't give uncertain finesses or likely dupes
						// Giver cannot give a layered finesse when they have a card matching the desired id
						val uncertainFinesse = !possiblyBluff &&
							state.hands(giver).exists(o => state.deck(o).clued && game.players(giver).thoughts(o).inferred.contains(id))

						val orangeFib = state.isInverted(id) && !state.isInverted(finesseId)

						if uncertainFinesse then
							Log.warn(s"disallowed hidden finesse on ${state.names(reacting)}, ${state.logId(id)} could be duplicated in giver's hand")
							None
						else if orangeFib then
							Log.warn("disallowed bluff when the focus is an orange card!")
							None
						else
							Some(FinesseConn(reacting, finesse.get, List(finesseId), fKind = if possiblyBluff then FinesseKind.Bluff else FinesseKind.Hidden))

				else
					None

def findSingleConn(ctx: ClueContext, reacting: Int, id: Identity, connCtx: ConnectContext, opts: ConnectOpts, connections: List[Connection] = Nil): Option[List[Connection]] =
	val ClueContext(prev, game, action, _) = ctx
	val state = game.state
	val ClueAction(giver, target, _, _) = action

	lazy val allVisible =
		val remaining = state.cardCount(id.toOrd) - state.baseCount(id.toOrd)

		// Everyone between giver and reacting must be able to see them,
		// otherwise they will try to react
		val visible = playersUntil(state.numPlayers, state.nextPlayerIndex(reacting), giver)
			.summing(state.hands(_).count(state.deck(_).matches(id)))

		remaining == visible

	val skip = reacting == giver ||
		opts.knownOnly.contains(reacting) ||
		(reacting == target && connCtx.looksDirect && !allVisible) ||
		(connCtx.thinksStall.contains(reacting) && {	// might not think stall if a clue exists in our hand
			giver == state.ourPlayerIndex ||
			state.ourHand.forall: o =>
				state.deck(o).clued || game.me.orderKt(game, o)
		})

	// TODO: When resolving, disallow prompting/finessing a player that may need to prove sth to us
	if skip then
		// Log.info(s"skipping ${state.names(reacting)}, giver $giver target $target $connCtx ${opts.knownOnly.fmt}")
		None
	else
		findUnknownConnecting(ctx, reacting, id, connCtx.connected, connCtx.ignore, opts) match
			case None => None

			// Try again
			case Some(conn) if conn.hidden =>
				val selfClandestine = reacting == action.target &&
					conn.ids.head.next.exists(game.common.thoughts(ctx.focusResult.focus).possible.contains) &&
					// Someone else finessing will prove this is a clandestine self.
					!opts.nonTargetFinessed

				if selfClandestine then
					// Log.warn("illegal clandestine self-finesse!")
					None
				else
					val hypo = state.deck(conn.order).id().orElse(Option.when(conn.ids.length == 1)(conn.ids.head))
						.fold(game)(id => game.withState(_.withPlay(id)).elim(skipHypoStacks = true))

					val newConnCtx = connCtx.copy(connected = connCtx.connected.incl(conn.order))
					findSingleConn(ctx.copy(game = hypo), reacting, id, newConnCtx, opts, conn +: connections)

			case Some(conn) =>
				Some((conn +: connections).reverse)

def findConnecting(ctx: ClueContext, id: Identity, finalId: Identity, connCtx: ConnectContext, opts: ConnectOpts): Option[List[Connection]] =
	val ClueContext(prev, game, action, _) = ctx
	val state = game.state
	val ClueAction(giver, target, _, _) = action

	// Log.highlight(Console.GREEN, s"find connecting ${state.logId(id)}")

	if state.baseCount(id.toOrd) == state.cardCount(id.toOrd) then
		Log.info(s"all ${state.logId(id)} in trash!")
		return None

	val known = findKnownConn(ctx, id, finalId, connCtx.ignore.union(connCtx.connected), opts.findOwn.isDefined)

	if known.isDefined then
		return Some(known.toList)

	val mustPassback = game.common.thoughts(ctx.focusResult.focus).inferred.exists: i =>
		i.suitIndex != id.suitIndex &&
		state.playStacks(i.suitIndex) <= state.playStacks(id.suitIndex)

	val connPlayerOrder =
		if mustPassback then
			List(
				List(target),	// first, try without layered
				(0 until state.numPlayers)
					.map(i => (giver - i - 1 + state.numPlayers) % state.numPlayers)
					.filterNot(_ == target),
				List(target),	// allow self-layered if no one will fix a self-clandestine
			).flatten
		else
			(1 until state.numPlayers).map(i => (giver - i + state.numPlayers) % state.numPlayers)

	connPlayerOrder.zipWithIndex.findSome: (reacting, i) =>
		findSingleConn(ctx, reacting, id, connCtx, opts.copy(noLayer = mustPassback && i == 0))

def connect(ctx: ClueContext, id: Identity, looksDirect: Boolean, thinksStall: FastBitSet, assumeTruth: Boolean = false, ignoreKnown: FastBitSet = FastBitSet.empty, findOwn: Option[Int] = None, preferOwn: Boolean = false, forTrashFinesse: Boolean = false, immediate: Boolean = false): List[FocusPossibility] =
	val ClueContext(prev, game, action, _) = ctx
	val ClueAction(giver, target, _, clue) = action
	val state = game.state
	val FocusResult(focus, _, positional) = ctx.focusResult
	val isTrashPush = positional.contains(Positional.TrashPush)

	// Log.highlight(Console.MAGENTA, s"trying to connect ${state.logId(id)}${if (looksDirect) " (looks direct)" else ""} $findOwn")

	def loop(hypo: HGroup, nextRank: Int, connections: List[Connection], connCtx: ConnectContext, opts: ConnectOpts): List[(Either[List[Connection], List[Connection]], Boolean)] =
		if nextRank >= id.rank then
			return List((Right(connections), opts.bluff))

		val nextId = Identity(id.suitIndex, nextRank)
		val newCtx = ctx.copy(game = hypo)

		val lastPlayer = connections.lastOption.map(_.reacting).getOrElse(giver)
		val disallowed = (0 until state.numPlayers).filter: p =>
			if isTrashPush then
				!inBetween(state.numPlayers, p, state.nextPlayerIndex(lastPlayer), target)
			else if forTrashFinesse && (findOwn.isDefined || preferOwn) then
				p == target || (immediate && p != state.ourPlayerIndex && prev.state.hands(p).exists(prev.isBlindPlaying))
			else
				false
		val stepConnCtx = connCtx.when(_ => isTrashPush):
			_.copy(ignore = connCtx.ignore.union(disallowed.flatMap(state.hands(_))))
		val stepOpts = opts.when(_ => disallowed.nonEmpty || isTrashPush || immediate):
			_.copy(knownOnly = opts.knownOnly.union(disallowed), noLayer = opts.noLayer || isTrashPush || immediate)

		def seekOwn(playerIndex: Int) =
			val known = findKnownConn(newCtx, nextId, id, stepConnCtx.ignore.union(stepConnCtx.connected), findOwn = true, preferOwn = preferOwn)

			// See if we need to correct based on future information
			val actualKnown = known match
				case Some(c @ PlayableConn(reacting, order, id, linked, hidden, _)) =>
					if linked.forall(!game.future(_).contains(id)) then
						val playableIds = game.future(order).intersect(state.playableSet)

						if playableIds.isEmpty then None else
							Log.highlight(Console.CYAN, s"playable conn ($order) is known to not match in the future, finding again")
							val unknown = findSingleConn(newCtx, playerIndex, nextId, stepConnCtx.copy(connected = stepConnCtx.connected.incl(order)), stepOpts.copy(findOwn = Some(playerIndex)))
							unknown.map(c.copy(id = playableIds.head) +: _)
					else
						Some(List(c))
				case k => k.map(List(_))

			actualKnown.orElse:
				findSingleConn(newCtx, playerIndex, nextId, stepConnCtx, stepOpts.copy(findOwn = Some(playerIndex)))

		val rawConns =
			if preferOwn then
				List(seekOwn(state.ourPlayerIndex), findConnecting(newCtx, nextId, id, stepConnCtx, stepOpts)).flatten
			else
				findConnecting(newCtx, nextId, id, stepConnCtx, stepOpts)
				.orElse(findOwn.flatMap(seekOwn))
				.toList

		val possibleConns =
			if !isTrashPush && !immediate then rawConns
			else rawConns.filter:
				_.forall: conn =>
					!conn.hidden &&
					!prev.state.hands(conn.reacting).exists(o => o != conn.order && prev.isBlindPlaying(o))

		if possibleConns.isEmpty then
			// Log.info(s"failed connection to ${state.logId(id)}: ${state.logConns(connections, nextId)} $findOwn")
			return List((Left(connections), opts.bluff))

		else
			possibleConns.flatMap: conns =>
				// Log.info(s"found conns ${state.logConns(conns)} $findOwn")

				val newGame = conns.foldLeft(hypo): (acc, conn) =>
					acc.state.deck(conn.order).id()
						.orElse(Option.when(conn.ids.length == 1)(conn.ids.head))
						.fold(acc): i =>
							val playAction = PlayAction(state.holderOf(conn.order), conn.order, i.suitIndex, i.rank)
							val level = Logger.level

							Logger.setLevel(LogLevel.Error.min(level))

							// Resolve wcs after playing the card
							val res = acc.onPlay(playAction)
								.pipe(refreshWCs(acc, _, playAction, hypo = Some(giver)))

							Logger.setLevel(level)
							res

				val newConnCtx = connCtx.copy(
					looksDirect = connCtx.looksDirect && {
						(clue.kind == ClueKind.Colour && nextId.next.exists(game.common.thoughts(focus).possible.contains)) ||
						positional.isDefined ||
						!conns.existsM:
							case f: FinesseConn => f.reacting != target && !f.hidden
							case f: PromptConn  => f.reacting != target && !f.hidden
					},
					connected = connCtx.connected.union(conns.map(_.order))
				)

				val newOpts = opts.copy(
					bluff = opts.bluff || conns.exists(_.isPossiblyBluff),
					nonTargetFinessed = opts.nonTargetFinessed || conns.exists(c => c.isInstanceOf[FinesseConn] && c.reacting != target),
					insertingInto = opts.insertingInto.orElse(conns.collectFirst { case c: PlayableConn if c.insertingInto.isDefined => c.insertingInto.get })
				)

				if !forTrashFinesse && connections.isEmpty && ignoreKnown.isEmpty && conns.exists(_.isPossiblyBluff) && isIntermediateBluffTarget(prev, game, action, id, focus) then
					List((Right(conns), true))
				else
					loop(newGame, nextRank + 1, connections ++ conns, newConnCtx, newOpts)

	val ignore =
		// Assume mud clues aren't selfish
		if ctx.focusResult.positional.contains(Positional.Mud) then
			ignoreKnown.union(state.hands(giver))
		else
			ignoreKnown

	val unconfirmedTrash = state.heldOrders.filter(unconfirmedTrashPlay(prev, giver, _, findOwn))
	val actualAssumeTruth = assumeTruth || isTrashPush
	val initialConnCtx = ConnectContext(looksDirect = looksDirect, thinksStall = thinksStall, connected = FastBitSet.single(focus), ignore = ignore.union(unconfirmedTrash))
	val initialOpts = ConnectOpts(assumeTruth = actualAssumeTruth)

	loop(game, state.playStacks(id.suitIndex) + 1, Nil, initialConnCtx, initialOpts).flatMap:
		case (Left(conns), bluffed) =>
			val truthConns =
				if game.level >= Level.Bluffs && !actualAssumeTruth && bluffed then
					Log.highlight(Console.MAGENTA, "bluff connection failed, retrying true finesse")
					connect(ctx, id, looksDirect, thinksStall, assumeTruth = true, findOwn = findOwn, ignoreKnown = ignore, preferOwn = preferOwn, forTrashFinesse = forTrashFinesse, immediate = immediate)
				else
					List.empty

			if truthConns.nonEmpty then
				truthConns
			else if conns.existsM { case c: KnownConn => !unconfirmedTrashPlay(prev, giver, c.order, findOwn) } then
				val newIgnore = ignore.incl(conns.collectFirst { case c: KnownConn if !unconfirmedTrashPlay(prev, giver, c.order, findOwn) => c.order }.get)
				connect(ctx, id, looksDirect, thinksStall, ignoreKnown = newIgnore, findOwn = findOwn, assumeTruth = actualAssumeTruth, preferOwn = preferOwn, forTrashFinesse = forTrashFinesse, immediate = immediate)
			else
				List.empty

		case (Right(conns), _) if conns.exists(c => unconfirmedTrashPlay(prev, giver, c.order, findOwn)) =>
			List.empty

		case (Right(conns), _) if forTrashFinesse =>
			findTrashFinesses(ctx, conns, id, ambiguous = preferOwn)

		case (Right(conns), _) =>
			val symmetric = !state.deck(focus).matches(id, assume = target == state.ourPlayerIndex) ||
				!game.players(target).thoughts(focus).possible.contains(id) ||
				// Newly touching a visible dupe
				(!prev.isTouched(focus) && visibleFind(state, prev.players(giver), id, infer = true, excludeOrder = focus).exists(prev.isTouched)) ||
				symmetricConns(state, conns) ||
				conns.exists(c => unconfirmedTrashPlay(prev, giver, c.order))

			val finalizedConns = finalizeConns(conns, givenByUs = giver == state.ourPlayerIndex)

			Log.info(s"found connections: ${state.logConns(finalizedConns, id)}${if symmetric then " (symmetric)" else ""}${if preferOwn then " (ambiguous)" else ""}")
			Some(FocusPossibility(id, finalizedConns, ClueInterp.Play, symmetric, ambiguous = preferOwn))

/** Returns true if the given order is currently expected to blind play to prove an unconfirmed Trash Finesse. */
def unconfirmedTrashPlay(prev: HGroup, giver: Int, order: Int, observer: Option[Int] = None): Boolean =
	val state = prev.state
	prev.waiting.exists: wc =>
		!wc.symmetric && !wc.ambiguousSelf &&
		wc.trashFinesse && wc.needsBlindPlay &&
		wc.target != state.ourPlayerIndex &&
		wc.target != giver &&
		!observer.contains(wc.target) &&
		{
			var lastPlayer = giver
			var found = false
			var ci = 0
			while ci < wc.connections.length && !found &&
				inBetween(state.numPlayers, wc.connections(ci).reacting, state.nextPlayerIndex(lastPlayer), wc.target)
			do
				wc.connections(ci) match
					case f: FinesseConn if f.order == order => found = true
					case _ => ()
				lastPlayer = wc.connections(ci).reacting
				ci += 1
			found
		}

def symmetricConns(state: State, conns: Seq[Connection]): Boolean =
	conns.exists:
		case conn: PlayableConn =>
			if conn.linked.isEmpty then
				state.deck(conn.order).id().exists(!conn.ids.contains(_))
			else
				conn.linked.forall(state.deck(_).id().exists(!conn.ids.contains(_)))
		case conn: FinesseConn if conn.isBluff =>
			// A bluff doesn't have to match the actual id
			false
		case conn =>
			state.deck(conn.order).id().exists(!conn.ids.contains(_))

def findTrashFinesses(ctx: ClueContext, connections: List[Connection], nextId: Identity, ambiguous: Boolean = false): List[FocusPossibility] =
	val ClueContext(prev, game, action, _) = ctx
	val (common, state) = (game.common, game.state)
	val ClueAction(giver, target, _, _) = action
	val focus = ctx.focusResult.focus
	val focusThoughts = common.thoughts(focus)

	if game.level < Level.TrashMoves || isTrashClue(prev, game, action, focus) || !connections.exists(_.isInstanceOf[FinesseConn]) then
		return Nil

	// The first unknown play must come from another player.
	if connections.find(c => c.isInstanceOf[FinesseConn] || c.isInstanceOf[PromptConn]).exists(_.reacting == target) then
		return Nil

	var lastPlayer = giver
	var ci = 0
	while ci < connections.length &&
		inBetween(state.numPlayers, connections(ci).reacting, state.nextPlayerIndex(lastPlayer), target) &&
		!prev.state.hands(connections(ci).reacting).exists(prev.isBlindPlaying)
	do
		lastPlayer = connections(ci).reacting
		ci += 1

	val isMismatchBluff = connections.head.isBluff && !connections.head.ids.forall(_.suitIndex == nextId.suitIndex)

	// If there are plays before the target, check if the play would prove it trash.
	val playablesRemaining =
		val base = focusThoughts.possible.filter(!state.isBasicTrash(_))
		if ci == 0 then
			base
		else if isMismatchBluff then
			// If we have a bluff connection, we can only eliminate immediate playables or connecting matching ranks.
			base.filter: id =>
				!state.isPlayable(id) &&
				!connections(ci - 1).ids.forall(cid => cid.suitIndex == id.suitIndex && cid.rank >= id.rank)
		else
			// Otherwise, we can rule out anything that doesn't follow the connection.
			base.filter: id =>
				!connections(ci - 1).ids.forall(cid => cid.suitIndex != id.suitIndex || cid.rank >= id.rank)

	val noConnectingPlay = playablesRemaining.isEmpty

	val tfConnections = finalizeConns(connections, givenByUs = giver == state.ourPlayerIndex) match
		case (f: FinesseConn) :: tail if !isMismatchBluff && f.isPossiblyBluff =>
			f.copy(fKind = FinesseKind.True) :: tail
		case finalized =>
			finalized

	def trashWithReverse(revConns: List[Connection]) =
		val revFinesses = revConns.collect { case f: FinesseConn => f }
		focusThoughts.possible.filter: id =>
			state.isBasicTrash(id) ||
			revFinesses.exists(_.ids.contains(id))

	val validReverse = !tfConnections.exists(_.isPossiblyBluff) && {
		val revTrash = trashWithReverse(tfConnections)

		// 2. The target knows their card must either be trash or match a reverse finesse play, or
		val cardMatchesLastPlay = focusThoughts.possible.forall(revTrash.contains)

		// 3. There is only one playable card and we might have it in our hand.
		lazy val playableIds = focusThoughts.inferred.filter: id =>
			!revTrash.contains(id) &&
			!tfConnections.exists(conn => conn.ids.length == 1 && conn.ids.head == id)

		lazy val maybeInOurHand = playableIds.length == 1 &&
			state.isCritical(playableIds.head) &&
			giver != state.ourPlayerIndex &&
			target != state.ourPlayerIndex &&
			state.ourHand.exists(o => game.me.thoughts(o).possible.contains(playableIds.head))

		cardMatchesLastPlay || maybeInOurHand
	}

	// Can only reverse trash finesse before end game.
	if !noConnectingPlay && (game.inEndgame || !validReverse) then
		return Nil

	val possibleTrash = trashWithReverse(if game.inEndgame then Nil else tfConnections.drop(ci))

	val symmetric = !state.deck(focus).id().forall(possibleTrash.contains) || symmetricConns(state, tfConnections)

	Log.info(s"found possible trash finesse: ${state.logConns(tfConnections)} -> [${possibleTrash.fmt(state)}]${if symmetric then " (symmetric)" else ""}${if ambiguous then " (ambiguous)" else ""}")
	possibleTrash.toList.map: id =>
		FocusPossibility(id, tfConnections, ClueInterp.Play, symmetric = symmetric, ambiguous = ambiguous, trashFinesse = true, needsBlindPlay = !validReverse)

def connectTrashFinesses(ctx: ClueContext, looksDirect: Boolean, thinksStall: FastBitSet, findOwn: Option[Int] = None, preferOwn: Boolean = false): List[FocusPossibility] =
	val ClueContext(prev, game, action, _) = ctx
	val (common, state) = (game.common, game.state)
	val ClueAction(giver, target, _, clue) = action
	val FocusResult(focus, _, positional) = ctx.focusResult

	if game.level < Level.TrashMoves || positional.contains(Positional.TrashPush) || isTrashClue(prev, game, action, focus) then
		return Nil

	val targetIds = (0 until state.variant.suits.length).toList.flatMap: suitIndex =>
		val rank = if clue.kind == ClueKind.Rank then clue.value else state.playStacks(suitIndex) + 1
		Option.when(state.playStacks(suitIndex) < rank && state.variant.idTouched(Identity(suitIndex, rank), clue)):
			Identity(suitIndex, rank + 1)

	if findOwn.isEmpty && !preferOwn then
		targetIds.flatMap:
			connect(ctx, _, looksDirect, thinksStall, forTrashFinesse = true)
	else if giver != state.ourPlayerIndex && target != state.ourPlayerIndex && state.deck(focus).id().exists(state.isBasicTrash) && common.thoughts(focus).possible.exists(!state.isBasicTrash(_)) then
		game.findFinesse(state.ourPlayerIndex) match
			case None => Nil
			case Some(ownFinesse) =>
				val possibleBluff = !preferOwn &&
					state.nextPlayerIndex(giver) == state.ourPlayerIndex &&
					!prev.isBlindPlaying(ownFinesse) &&
					!common.thoughts(focus).possible.exists(id => !state.isBasicTrash(id) && !state.isPlayable(id))

				if possibleBluff then
					val bluffableIds = common.thoughts(ownFinesse).possible.filter(state.isPlayable)
					val trashIds = common.thoughts(focus).possible.filter(state.isBasicTrash)
					if bluffableIds.nonEmpty then
						val conn = FinesseConn(state.ourPlayerIndex, ownFinesse, bluffableIds.toList, FinesseKind.Bluff)
						trashIds.toList.map(FocusPossibility(_, List(conn), ClueInterp.Play, trashFinesse = true, needsBlindPlay = true))
					else
						Nil
				else
					val direct = common.thoughts(focus).possible.exists(state.isPlayable)
					val immediate = common.thoughts(focus).inferred.length > 1
					targetIds.flatMap:
						connect(ctx, _, direct, thinksStall, assumeTruth = true, findOwn = findOwn, preferOwn = preferOwn, forTrashFinesse = true, immediate = immediate)
	else
		Nil

/** Corrects 'possibly bluff' connections to true finesses if necessary. */
def finalizeConns(conns: List[Connection], givenByUs: Boolean = false) =
	if conns.isEmpty then conns else
		val newHead = conns.head match
			case f: FinesseConn if f.possiblyBluff =>
				if conns.count(_.isInstanceOf[FinesseConn]) > 1 then
					Log.info("rewriting to true finesse")
					f.copy(fKind = FinesseKind.True)
				else if givenByUs then
					f.copy(fKind = FinesseKind.Bluff)
				else
					f

			case x => x

		newHead +: conns.tail
