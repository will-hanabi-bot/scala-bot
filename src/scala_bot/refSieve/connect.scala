package scala_bot.refSieve

import scala_bot.basics._, RefSieve as RS
import scala_bot.basics.given_Conversion_IdentitySet_Iterable
import scala_bot.logger.Log

import scala.annotation.tailrec

def findConnecting(prev: RS, game: RS, id: Identity, playerIndex: Int, connected: List[Int], looksDirect: Boolean, ignore: Set[Int] = Set(), findOwn: Boolean = false): Option[Connection] =
	val state = game.state
	val player = if (findOwn) game.players(playerIndex) else game.common
	val hand = state.hands(playerIndex)

	// Log.info(s"find connecting ${state.logId(id)} in ${state.names(playerIndex)}'s hand, findOwn? $findOwn")

	val known = hand.find(o => !connected.contains(o) && player.thoughts(o).matches(id, infer = true))

	if (known.exists(!ignore.contains(_)))
		return Some(KnownConn(playerIndex, known.get, id))

	val playable = hand.find { o =>
		val thought = player.thoughts(o)

		!connected.contains(o) &&
		(game.isBlindPlaying(o) || (if (findOwn) thought.inferred else thought.possible).forall(state.isPlayable)) &&
		thought.inferred.contains(id)
	}

	if (playable.exists(p => state.deck(p).matches(id, assume = findOwn) && !ignore.contains(p)))
		return Some(PlayableConn(playerIndex, playable.get, id))

	if (looksDirect || playable.nonEmpty)
		return None

	game.common.findPrompt(prev, playerIndex, id, connected, ignore, rightmost = true).map { prompt =>
		if (!state.deck(prompt).matches(id, assume = findOwn))
			if (state.deck(prompt).id().isDefined)
				Log.warn(s"wrong prompt! ${state.logId(prompt)} looks like ${state.logId(id)}")
			None
		else
			Some(PromptConn(playerIndex, prompt, id))
	}
	.getOrElse {
		game.findFinesse(playerIndex, connected, ignore)
		.filter(state.deck(_).matches(id, assume = findOwn))
		.map(FinesseConn(playerIndex, _, List(id)))
	}

def connect(prev: RS, game: RS, targetOrder: Int, id: Identity, action: ClueAction, unknown: Boolean, findOwn: Boolean = false): Option[List[Connection]] =
	val state = game.state
	val Identity(suitIndex, rank) = id

	// Log.info(s"attempting to ${if (findOwn) "find own finesses" else "connect"} ${state.logId(id)} $targetOrder")

	@tailrec
	def loop(nextRank: Int, playerIndex: Int, connections: List[Connection], turnsSincePlay: Int = 1): Option[List[Connection]] =
		if (nextRank == rank)
			Some(connections.reverse)
		else
			val nextId = Identity(suitIndex, nextRank)
			val connected = targetOrder +: connections.map(_.order)
			val looksDirect = unknown && playerIndex == action.target
			val own = findOwn && playerIndex == state.ourPlayerIndex
			val connecting = findConnecting(prev, game, nextId, playerIndex, connected, looksDirect, findOwn = own)

			val nextPlayerIndex = state.nextPlayerIndex(playerIndex)

			connecting.match {
				case None if (playerIndex == action.target && unknown) || (turnsSincePlay == state.numPlayers) => None
				case None =>
					loop(nextRank, nextPlayerIndex, connections, turnsSincePlay + 1)
				case Some(conn) =>
					loop(nextRank + 1, nextPlayerIndex, conn +: connections, 1)
			}

	loop(state.playStacks(suitIndex) + 1, state.nextPlayerIndex(action.giver), List()).map { conns =>
		Log.info(s"found connections ${state.logConns(conns, id)}")
		conns
	}
