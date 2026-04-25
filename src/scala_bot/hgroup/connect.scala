package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.utils._

import scala_bot.logger.{Log, Logger, LogLevel}

case class ConnectContext(
	looksDirect: Boolean,
	thinksStall: Set[Int],
	connected: Set[Int],
	ignore: Set[Int] = Set()
)

case class ConnectOpts(
	knownOnly: Set[Int] = Set(),
	assumeTruth: Boolean = false,
	bluff: Boolean = false,
	findOwn: Option[Int] = None,
	nonTargetFinessed: Boolean = false,
	noLayer: Boolean = false,
	preferOwn: Boolean = false,
	insertingInto: Option[Seq[Int]] = None
)

def findKnownConn(ctx: ClueContext, id: Identity, ignore: Set[Int], findOwn: Boolean) =
	val ClueContext(prev, game, action) = ctx
	val (common, state) = (game.common, game.state)
	val giver = action.giver

	def validKnown(order: Int) =
		!ignore.contains(order) &&
		game.state.deck(order).matches(id, assume = true) &&
		common.thoughts(order).matches(id, infer = true) &&
		!game.meta(order).hidden &&
		!game.xmeta(order).fStatus.contains(FStatus.PossiblyOn(giver)) &&
		(game.assumePlays || !game.xmeta(order).fStatus.contains(FStatus.PossiblyOn(state.ourPlayerIndex))) &&
		!common.linkedOrders(state).contains(order)

	def validLink(link: Link, order: Int) =
		link.matchesP:
			case Link.Promised(orders, i, _) => i == id && orders.contains(order)
			case Link.Sarcastic(orders, i) => i == id && orders.contains(order)

	def validPlayable(playerIndex: Int, order: Int) =
		!ignore.contains(order) &&
		common.thoughts(order).inferred.contains(id) &&
		!game.xmeta(order).fStatus.contains(FStatus.PossiblyOn(giver)) &&
		(game.assumePlays || !game.xmeta(order).fStatus.contains(FStatus.PossiblyOn(state.ourPlayerIndex))) &&
		common.orderPlayable(game, order, excludeTrash = true) &&
		!(	// Don't connect on our unknown playables for an id matching the focus:
			// giver would be bad touching
			playerIndex == state.ourPlayerIndex &&
			state.deck(ctx.focusResult.focus).id().contains(id)
		)

	def validLinkedPlay(order: Int) =
		state.deck(order).clued &&
		common.playLinks.find(_.target == order).isDefined &&
		common.thoughts(order).inferred.contains(id)

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
		link <- common.links if validLink(link, order) && common.thoughts(order).inferred.difference(state.playableSet).isEmpty
	yield
		PlayableConn(playerIndex, order, id, linked = link.getOrders.toList)

	// Log.info(s"finding known ${state.logId(id)} $ignore ${common.linkedOrders(state)} $findOwn")

	// Visible and going to be played (excludes giver)
	val playableConns = for
		playerIndex <- (0 until state.numPlayers).view if playerIndex != giver
		playables = state.hands(playerIndex).filter(validPlayable(playerIndex, _))
		order <- playables if state.deck(order).matches(id, assume = game.allowFindOwn && findOwn) && game.isTouched(order)
	yield
		PlayableConn(playerIndex, order, id, linked = playables.toList)

	val playLinkedConns = for
		playerIndex <- (0 until state.numPlayers).view if playerIndex != giver
		order <- state.hands(playerIndex).find(validLinkedPlay) if state.deck(order).matches(id, assume = game.allowFindOwn && findOwn)
	yield
		PlayableConn(playerIndex, order, id, linked = List(order))

	val previouslyPlayable = for
		playerIndex <- (0 until state.numPlayers).view if playerIndex != giver
		order <- state.hands(playerIndex).find: o =>
			prev.state.deck(o).clued &&
			prev.common.hypoPlays.contains(o) &&
			state.deck(o).matches(id, assume = game.allowFindOwn && findOwn) &&
			common.thoughts(o).inferred.contains(id)
	yield
		PlayableConn(playerIndex, order, id, linked = List(order))

	knownConns.headOption
	.orElse(linkedConns.headOption)
	.orElse(playableConns.headOption)
	.orElse(playLinkedConns.headOption)
	.orElse(previouslyPlayable.headOption)

def findUnknownConnecting(ctx: ClueContext, reacting: Int, id: Identity, connected: Set[Int], ignore: Set[Int], opts: ConnectOpts): Option[Connection] =
	val ClueContext(prev, game, action) = ctx
	val (state, level) = (game.state, game.level)
	val ClueAction(giver, target, _, _) = action
	val FocusResult(focus, _, _) = game.determineFocus(prev, action)

	// val flags = List(
	// 	opts.findOwn.map(own => s"own ($own)"),
	// 	Option.when(opts.noLayer)("no layer"),
	// 	opts.insertingInto.map(i => s"inserting into $i"),
	// 	Option.when(opts.bluff)("bluff")
	// ).flatten.mkString(", ")
	// Log.info(s"finding unknown connecting for ${state.logId(id)} (${state.names(reacting)}), $connected, flags: [$flags]")

	if opts.bluff then
		val clued = prev.common.findClued(prev, reacting, id, ignore ++ connected)
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

	val prompt = prev.common.findPrompt(prev, reacting, id, connected, ignore)
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
	val tryPinkPrompt = state.includesVariant(PINKISH) &&
		potentialFinesse.forall: f =>
			if level < Level.Bluffs then
				!game.common.thoughts(f).possible.exists: i =>
					state.variant.suits(i.suitIndex).suitType.pinkish &&
					state.isPlayable(i)
			else
				!game.common.thoughts(f).possible.exists(state.isPlayable)

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
	val insertingInto = if game.level < Level.IntermediateFinesses || !opts.findOwn.contains(reacting) then None else
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
					val possible = nextFinesse.exists(hypo.common.thoughts(_).possible.intersect(finesseIds).nonEmpty)

					Either.cond(possible, newConnected + nextFinesse.get, Set.empty)
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
					case None => Left(Set.empty)
					case Some(f) => Right(newConnected + f)

			if newConnected.isEmpty then None else
				game.findFinesseId(reacting, id, newConnected, ignore, overrideLayer = opts.insertingInto.exists(_.exists(prev.state.hands(reacting).contains)))
		else
			potentialFinesse

	finesse.flatMap(state.deck(_).id()) match
		case _ if finesse.exists(game.future(_).length == 1) =>
			val actualId = game.future(finesse.get).head

			val fKind = if actualId == id then
				FinesseKind.True
			else if opts.assumeTruth || !validBluff(game, action, actualId, id, reacting, connected) then
				FinesseKind.Hidden
			else
				FinesseKind.Bluff

			Option.when(state.isPlayable(actualId)):
				FinesseConn(reacting, finesse.get, List(actualId), fKind)

		case None if finesse.isDefined && knownLayeredIds.isDefined =>
			Some(FinesseConn(reacting, finesse.get, knownLayeredIds.get.toList, FinesseKind.Hidden))

		case _ if insertingInto.isDefined =>
			val (order, inserts) = insertingInto.get
			Some(PlayableConn(reacting, order, id, insertingInto = Some(inserts)))

		case None if opts.findOwn.isDefined && finesse.isDefined =>
			val thought = game.players(opts.findOwn.get).thoughts(finesse.get)
			val bluffableIds = thought.inferred.filter: i =>
				state.isPlayable(i) &&
				validBluff(game, action, i, id, reacting, connected)

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

				val ids = if trueFinesse then List(thought.id(infer = true).getOrElse(id)) else bluffableIds.toList

				FinesseConn(reacting, finesse.get, ids, fKind)

		case None => None

		case Some(finesseId) if finesseId == id =>
			if level == 1 && !inBetween(state.numPlayers, reacting, giver, target) then
				Log.warn(s"found non-forward finesse ${state.logId(id)} in ${state.names(reacting)}'s hand at lv 1!")
				None
			else
				Some(FinesseConn(reacting, finesse.get, List(id), FinesseKind.True))

		case Some(finesseId) =>
			val possiblyBluff = !opts.assumeTruth &&
				validBluff(game, action, finesseId, id, reacting, connected)

			if opts.findOwn.exists(i => i != state.ourPlayerIndex && game.players(i).thoughts(finesse.get).possible.contains(id)) then
				Some(FinesseConn(reacting, finesse.get, List(id), fKind = if possiblyBluff then FinesseKind.Bluff else FinesseKind.True))

			else if !opts.noLayer && level >= Level.IntermediateFinesses && state.isPlayable(finesseId) then
				if game.meta(finesse.get).status == CardStatus.Finessed && game.isDefinite(finesse.get) then
					Some(PlayableConn(reacting, finesse.get, finesseId, hidden = true))
				else
					// TODO: Check in resolver that we don't give uncertain finesses or likely dupes
					// Giver cannot give a layered finesse when they have a card matching the desired id
					val uncertainFinesse = !possiblyBluff &&
						state.hands(giver).exists(o => state.deck(o).clued && game.players(giver).thoughts(o).inferred.contains(id))

					if uncertainFinesse then
						Log.warn(s"disallowed hidden finesse on ${state.names(reacting)}, ${state.logId(id)} could be duplicated in giver's hand")
						None
					else
						Some(FinesseConn(reacting, finesse.get, List(finesseId), fKind = if possiblyBluff then FinesseKind.Bluff else FinesseKind.Hidden))

			else
				None

def findSingleConn(ctx: ClueContext, reacting: Int, id: Identity, connCtx: ConnectContext, opts: ConnectOpts, connections: List[Connection] = Nil): Option[List[Connection]] =
	val ClueContext(prev, game, action) = ctx
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
		// Log.info(s"skipping ${state.names(reacting)}, giver $giver target $target $connCtx ${opts.knownOnly}")
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
						.fold(game)(id => game.withState(_.withPlay(id)).elim())

					val newConnCtx = connCtx.copy(connected = connCtx.connected + conn.order)
					findSingleConn(ctx.copy(game = hypo), reacting, id, newConnCtx, opts, conn +: connections)

			case Some(conn) =>
				Some((conn +: connections).reverse)

def findConnecting(ctx: ClueContext, id: Identity, connCtx: ConnectContext, opts: ConnectOpts): Option[List[Connection]] =
	val ClueContext(prev, game, action) = ctx
	val state = game.state
	val ClueAction(giver, target, _, _) = action

	// Log.highlight(Console.GREEN, s"find connecting ${state.logId(id)}")

	if state.baseCount(id.toOrd) == state.cardCount(id.toOrd) then
		Log.info(s"all ${state.logId(id)} in trash!")
		return None

	val known = findKnownConn(ctx, id, connCtx.ignore ++ connCtx.connected, opts.findOwn.isDefined)

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

def connect(ctx: ClueContext, id: Identity, looksDirect: Boolean, thinksStall: Set[Int], assumeTruth: Boolean = false, ignoreKnown: Set[Int] = Set.empty, findOwn: Option[Int] = None, preferOwn: Boolean = false): Option[FocusPossibility] =
	val ClueContext(prev, game, action) = ctx
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
			val known = findKnownConn(newCtx, nextId, connCtx.ignore ++ connCtx.connected, findOwn = true)

			// See if we need to correct based on future information
			val actualKnown = known match
				case Some(c @ PlayableConn(reacting, order, id, linked, hidden, _)) =>
					if linked.forall(!game.future(_).contains(id)) then
						val playableIds = game.future(order).intersect(state.playableSet)

						if playableIds.isEmpty then None else
							Log.highlight(Console.CYAN, "playable conn is known to not match in the future, finding again")
							val unknown = findSingleConn(newCtx, playerIndex, nextId, connCtx.copy(connected = connCtx.connected + order), opts.copy(findOwn = Some(playerIndex)))
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
						positional ||
						!conns.existsM { case f: FinesseConn => f.reacting != target && !f.hidden }
					},
					connected = connCtx.connected ++ conns.map(_.order)
				)

				val newOpts = opts.copy(
					bluff = opts.bluff || conns.exists(_.isPossiblyBluff),
					nonTargetFinessed = opts.nonTargetFinessed || conns.exists(c => c.isInstanceOf[FinesseConn] && c.reacting != target),
					insertingInto = opts.insertingInto.orElse(conns.collectFirst { case c: PlayableConn if c.insertingInto.isDefined => c.insertingInto.get })
				)

				loop(newGame, nextRank + 1, connections ++ conns, newConnCtx, newOpts)

	val initialConnCtx = ConnectContext(looksDirect = looksDirect, thinksStall = thinksStall, connected = Set(focus), ignore = ignoreKnown)
	val initialOpts = ConnectOpts(assumeTruth = assumeTruth)

	loop(game, state.playStacks(id.suitIndex) + 1, Nil, initialConnCtx, initialOpts) match
		case (Left(conns), _) if conns.existsM { case _: KnownConn => true } =>
			val newIgnore = ignoreKnown + conns.collectFirst { case c: KnownConn => c.order }.get
			connect(ctx, id, looksDirect, thinksStall, ignoreKnown = newIgnore, findOwn = findOwn, assumeTruth = assumeTruth, preferOwn = preferOwn)

		case (Left(_), bluffed) if game.level >= Level.Bluffs && !assumeTruth && bluffed =>
			// Log.highlight(Console.MAGENTA, "bluff connection failed, retrying true finesse")

			connect(ctx, id, looksDirect, thinksStall, assumeTruth = true, findOwn = findOwn, ignoreKnown = ignoreKnown, preferOwn = preferOwn)

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
