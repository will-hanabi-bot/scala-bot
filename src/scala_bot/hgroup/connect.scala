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

def findKnownConn(ctx: ClueContext, id: Identity, ignore: FastBitSet, findOwn: Boolean, preferOwn: Boolean = false): Option[Connection] =
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
		val validKnown =
			!ignore.contains(o) &&
			game.state.deck(o).matches(id, assume = true) &&
			common.thoughts(o).matches(id, infer = true) &&
			!game.meta(o).hidden &&
			!game.xmeta(o).fStatus.contains(FStatus.PossiblyOn(giver)) &&
			(game.assumePlays || !game.xmeta(o).fStatus.contains(FStatus.PossiblyOn(state.ourPlayerIndex))) &&
			!common.isLinked(state, o)

		Option.when(validKnown)(KnownConn(state.holderOf(o), o, id))

	if globallyKnown.isDefined then
		return globallyKnown

	val promised = state.heldOrders.findSome: o =>
		if common.thoughts(o).inferred.difference(state.playableSet).nonEmpty then
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

	// Log.info(s"finding known ${state.logId(id)} $ignore ${common.linkedOrders(state)} $findOwn")

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
			playerIndex != giver &&
			state.deck(o).clued &&
			common.playLinks.find(_.target == o).isDefined &&
			common.thoughts(o).inferred.contains(id) &&
			state.deck(o).matches(id, assume = game.allowFindOwn && findOwn)

		Option.when(play):
			PlayableConn(playerIndex, o, id, linked = List(o))

	playLinked

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

	if prompt.exists(!rainbowMismatch(prev, action, id, _)) then
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
				val fKind = if futureIds.isExactly(id) then
					FinesseKind.True
				else if opts.assumeTruth || !futureIds.exists(validBluff(game, action, _, reacting, connected)) then
					FinesseKind.Hidden
				else
					FinesseKind.Bluff

				Some(FinesseConn(reacting, finesse.get, futureIds.toList, fKind, inverted = inverted))

		case None if finesse.isDefined && knownLayeredIds.isDefined =>
			Some(FinesseConn(reacting, finesse.get, knownLayeredIds.get.toList, FinesseKind.Hidden))

		case _ if insertingInto.isDefined =>
			val (order, inserts) = insertingInto.get
			Some(PlayableConn(reacting, order, id, insertingInto = Some(inserts)))

		case None if opts.findOwn.isDefined && finesse.isDefined =>
			val thought = game.players(opts.findOwn.get).thoughts(finesse.get)
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

def findConnecting(ctx: ClueContext, id: Identity, connCtx: ConnectContext, opts: ConnectOpts): Option[List[Connection]] =
	val ClueContext(prev, game, action, _) = ctx
	val state = game.state
	val ClueAction(giver, target, _, _) = action

	// Log.highlight(Console.GREEN, s"find connecting ${state.logId(id)}")

	if state.baseCount(id.toOrd) == state.cardCount(id.toOrd) then
		Log.info(s"all ${state.logId(id)} in trash!")
		return None

	val known = findKnownConn(ctx, id, connCtx.ignore.union(connCtx.connected), opts.findOwn.isDefined)

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

def connect(ctx: ClueContext, id: Identity, looksDirect: Boolean, thinksStall: FastBitSet, assumeTruth: Boolean = false, ignoreKnown: FastBitSet = FastBitSet.empty, findOwn: Option[Int] = None, preferOwn: Boolean = false): Option[FocusPossibility] =
	val ClueContext(prev, game, action, _) = ctx
	val ClueAction(giver, target, _, clue) = action
	val state = game.state
	val FocusResult(focus, _, positional) = ctx.focusResult

	// Log.highlight(Console.MAGENTA, s"trying to connect ${state.logId(id)}${if (looksDirect) " (looks direct)" else ""} $findOwn")

	@annotation.tailrec
	def loop(hypo: HGroup, nextRank: Int, connections: List[Connection], connCtx: ConnectContext, opts: ConnectOpts): (Either[List[Connection], List[Connection]], Boolean) =
		if nextRank >= id.rank then
			return (Right(connections), opts.bluff)

		val nextId = Identity(id.suitIndex, nextRank)
		val newCtx = ctx.copy(game = hypo)

		def seekOwn(playerIndex: Int) =
			val known = findKnownConn(newCtx, nextId, connCtx.ignore.union(connCtx.connected), findOwn = true, preferOwn = preferOwn)

			// See if we need to correct based on future information
			val actualKnown = known match
				case Some(c @ PlayableConn(reacting, order, id, linked, hidden, _)) =>
					if linked.forall(!game.future(_).contains(id)) then
						val playableIds = game.future(order).intersect(state.playableSet)

						if playableIds.isEmpty then None else
							Log.highlight(Console.CYAN, "playable conn is known to not match in the future, finding again")
							val unknown = findSingleConn(newCtx, playerIndex, nextId, connCtx.copy(connected = connCtx.connected.incl(order)), opts.copy(findOwn = Some(playerIndex)))
							unknown.map(c.copy(id = playableIds.head) +: _)
					else
						Some(List(c))
				case k => k.map(List(_))

			actualKnown.orElse:
				findSingleConn(newCtx, playerIndex, nextId, connCtx, opts.copy(findOwn = Some(playerIndex)))

		val conn =
			if preferOwn then
				seekOwn(state.ourPlayerIndex)
				.orElse(findConnecting(newCtx, nextId, connCtx, opts))
			else
				findConnecting(newCtx, nextId, connCtx, opts)
				.orElse(findOwn.flatMap(seekOwn))

		conn match
			case None =>
				// Log.info(s"failed connection to ${state.logId(id)}: ${state.logConns(connections, nextId)} $findOwn")
				(Left(connections), opts.bluff)

			case Some(conns) =>
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
						!conns.existsM { case f: FinesseConn => f.reacting != target && !f.hidden }
					},
					connected = connCtx.connected.union(conns.map(_.order))
				)

				val newOpts = opts.copy(
					bluff = opts.bluff || conns.exists(_.isPossiblyBluff),
					nonTargetFinessed = opts.nonTargetFinessed || conns.exists(c => c.isInstanceOf[FinesseConn] && c.reacting != target),
					insertingInto = opts.insertingInto.orElse(conns.collectFirst { case c: PlayableConn if c.insertingInto.isDefined => c.insertingInto.get })
				)

				loop(newGame, nextRank + 1, connections ++ conns, newConnCtx, newOpts)

	val ignore =
		// Assume mud clues aren't selfish
		if ctx.focusResult.positional.contains(Positional.Mud) then
			ignoreKnown.union(state.hands(giver))
		else
			ignoreKnown

	val initialConnCtx = ConnectContext(looksDirect = looksDirect, thinksStall = thinksStall, connected = FastBitSet.single(focus), ignore = ignore)
	val initialOpts = ConnectOpts(assumeTruth = assumeTruth)

	loop(game, state.playStacks(id.suitIndex) + 1, Nil, initialConnCtx, initialOpts) match
		case (Left(conns), _) if conns.existsM { case _: KnownConn => true } =>
			val newIgnore = ignore.incl(conns.collectFirst { case c: KnownConn => c.order }.get)
			connect(ctx, id, looksDirect, thinksStall, ignoreKnown = newIgnore, findOwn = findOwn, assumeTruth = assumeTruth, preferOwn = preferOwn)

		case (Left(_), bluffed) if game.level >= Level.Bluffs && !assumeTruth && bluffed =>
			// Log.highlight(Console.MAGENTA, "bluff connection failed, retrying true finesse")

			connect(ctx, id, looksDirect, thinksStall, assumeTruth = true, findOwn = findOwn, ignoreKnown = ignore, preferOwn = preferOwn)

		case (Right(conns), _) =>
			val symmetric = !state.deck(focus).matches(id, assume = true) ||
				!game.players(target).thoughts(focus).possible.contains(id) ||
				// Newly touching a visible dupe
				(!prev.isTouched(focus) && visibleFind(state, prev.players(giver), id, infer = true, excludeOrder = focus).exists(prev.isTouched)) ||
				conns.exists:
					case conn: PlayableConn =>
						if conn.linked.isEmpty then
							state.deck(conn.order).id().exists(!conn.ids.contains(_))
						else
							conn.linked.forall(state.deck(_).id().exists(!conn.ids.contains(_)))
					case conn =>
						state.deck(conn.order).id().exists(!conn.ids.contains(_))

			val finalizedConns = finalizeConns(conns, givenByUs = giver == state.ourPlayerIndex)

			Log.info(s"found connections: ${state.logConns(finalizedConns, id)}${if symmetric then " (symmetric)" else ""}${if preferOwn then " (ambiguous)" else ""}")
			Some(FocusPossibility(id, finalizedConns, ClueInterp.Play, symmetric, ambiguous = preferOwn))

		case _ => None

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
