package scala_bot.refSieve

import scala_bot.basics._
import scala_bot.logger.Log

import scala.annotation.tailrec

def findConnecting(ctx: ClueContext, id: Identity, playerIndex: Int, connected: Set[Int], looksDirect: Boolean, ignore: Set[Int] = Set(), findOwn: Boolean = false, alwaysConnect: Boolean = false, preferF: Boolean = false): Option[Connection] =
	val ClueContext(prev, game, action) = ctx
	val state = game.state
	val player = if findOwn then game.players(playerIndex) else game.common
	val hand = state.hands(playerIndex)

	// Log.info(s"find connecting ${state.logId(id)} in ${state.names(playerIndex)}'s hand, findOwn? $findOwn")

	val known = hand.find(o => !connected.contains(o) && player.thoughts(o).matches(id, infer = true))

	if known.exists(!ignore.contains(_)) then
		return Some(KnownConn(playerIndex, known.get, id))

	val playable = hand.find: o =>
		val thought = player.thoughts(o)

		!connected.contains(o) &&
		(game.isBlindPlaying(o) || (if findOwn then thought.inferred else thought.possible).forall(state.isPlayable)) &&
		thought.inferred.contains(id)

	if playable.exists(p => state.deck(p).matches(id, assume = findOwn) && !ignore.contains(p)) then
		return Some(PlayableConn(playerIndex, playable.get, id))

	if looksDirect || playable.nonEmpty || playerIndex == action.giver then
		return None

	lazy val prompt = game.common.findPrompt(prev, playerIndex, id, connected, ignore, rightmost = true).map: prompt =>
		if alwaysConnect || state.deck(prompt).matches(id, assume = findOwn) then
			Some(PromptConn(playerIndex, prompt, id))
		else
			if state.deck(prompt).id().isDefined then
				Log.warn(s"wrong prompt! ${state.logId(prompt)} looks like ${state.logId(id)}")
			None

	lazy val finesse =
		game.findFinesse(playerIndex, connected, ignore)
			.filter: o =>
				alwaysConnect ||
				(game.future(o).contains(id) && state.deck(o).matches(id, assume = findOwn))
			.map(FinesseConn(playerIndex, _, List(id)))

	if preferF then
		finesse
	else
		prompt.getOrElse(finesse)

def connect(ctx: ClueContext, targetOrder: Int, id: Identity, unknown: Boolean, findOwn: Boolean = false): Option[List[Connection]] =
	val ClueContext(prev, game, action) = ctx
	val state = game.state
	val Identity(suitIndex, rank) = id

	// Log.info(s"attempting to ${if (findOwn) "find own finesses" else "connect"} ${state.logId(id)} $targetOrder")

	@tailrec
	def loop(nextRank: Int, playerIndex: Int, connections: List[Connection] = Nil, turnsSincePlay: Int = 1, alwaysConnect: Boolean = false, remF: Int = 0): Option[List[Connection]] =
		if nextRank == rank then
			Some(connections.reverse)
		else
			val nextId = Identity(suitIndex, nextRank)
			val connected = connections.map(_.order).toSet + targetOrder
			val looksDirect = unknown && playerIndex == action.target
			val own = findOwn && playerIndex == state.ourPlayerIndex
			val connecting = findConnecting(ctx, nextId, playerIndex, connected, looksDirect, findOwn = own, alwaysConnect = alwaysConnect, preferF = remF > 0)

			val nextPlayerIndex = state.nextPlayerIndex(playerIndex)

			connecting match
				case None if (playerIndex == action.target && unknown) || (turnsSincePlay == state.numPlayers) => None
				case None =>
					loop(nextRank, nextPlayerIndex, connections, turnsSincePlay + 1, alwaysConnect, remF)
				case Some(conn) =>
					val newRemF = if conn.isInstanceOf[FinesseConn] then remF - 1 else remF
					loop(nextRank + 1, nextPlayerIndex, conn +: connections, 1, alwaysConnect, newRemF)

	val conns =
		if state.numPlayers == 2 then
			lazy val tryBluff = if !prev.state.deck(targetOrder).clued then None else
				game.findFinesse(action.target)
					.filter: o =>
						state.deck(o).id() match
							case Some(i) => state.isPlayable(i)
							case None if findOwn => game.common.thoughts(o).possible.intersect(state.playableSet).nonEmpty
							case _ => false
					.map(o => List(FinesseConn(action.target, o, game.common.thoughts(o).possible.intersect(state.playableSet).toList, bluff = true)))

			// First, count the # of finesses required. Then force finesses first, that many times.
			loop(state.playStacks(suitIndex) + 1, state.nextPlayerIndex(action.giver), alwaysConnect = true) match
				case Some(conns) =>
					if conns.exists(_.isInstanceOf[PromptConn]) && conns.exists(_.isInstanceOf[FinesseConn]) then
						val numFinesses = conns.count(_.isInstanceOf[FinesseConn])

						loop(state.playStacks(suitIndex) + 1, state.nextPlayerIndex(action.giver), remF = numFinesses)
							.orElse(tryBluff)
					else if conns.forall(_.isInstanceOf[PromptConn]) then
						val wrongPrompt = conns.exists:
							case p: PromptConn => !state.deck(p.order).matches(p.id, assume = findOwn)
							case _ => false

						Option.when(!wrongPrompt)(conns)
					else if conns.count(_.isInstanceOf[FinesseConn]) <= 1 then
						Some(conns)
					else
						tryBluff
				case None => tryBluff

		else
			loop(state.playStacks(suitIndex) + 1, state.nextPlayerIndex(action.giver))

	conns.map: cs =>
		Log.info(s"found connections ${state.logConns(cs, id)}")
		cs
