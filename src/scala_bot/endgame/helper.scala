package scala_bot.endgame

import scala_bot.basics._
import scala_bot.fraction.Frac
// import scala_bot.logger.Log
import scala_bot.utils.playersUntil

def findMustPlays(state: State, hand: Vector[Int]): Iterable[Identity] =
	val idGroups = hand.groupMap(state.deck(_).id())(identity(_))

	for {
		(id, group) <- idGroups
		i <- id if !state.isBasicTrash(i) && state.cardCount(i.toOrd) - state.baseCount(i.toOrd) == group.length
	}
	yield i

def unwinnableState(state: State, playerTurn: Int, depth: Int = 0): Boolean =
	if (state.ended || state.pace < 0)
		return true

	val voidPlayers = (0 until state.numPlayers).filter {
		state.hands(_).forall(state.deck(_).id().forall(state.isBasicTrash))
	}

	// println(s"${indent(depth)}void players: $voidPlayers, endgame_turns: ${state.endgameTurns}, current turn: ${state.names(playerTurn)}")

	val mustPlays = state.hands.map(findMustPlays(state, _))
	val mustStartEndgame = mustPlays.zipWithIndex.filter(_._1.size > 1).map(_._2)

	val impossibleEndgame = state.endgameTurns.exists { endgameTurns =>
		val possiblePlayers = (0 until endgameTurns).count { i =>
			!voidPlayers.contains((playerTurn + i) % state.numPlayers)
		}

		lazy val doublePlay = (0 until endgameTurns)
			.map(i => (playerTurn + i) % state.numPlayers)
			.find(mustPlays(_).size > 1)

		if (possiblePlayers + state.score < state.maxScore)
			// println(s"${indent(depth)}even if everyone (${possiblePlayers}) plays, can't reach max (${state.score}/${state.maxScore})")
			true
		else if (doublePlay.isDefined)
			// println(s"${indent(depth)}final round has started, ${state.names(doublePlay.get)} still needs to play ${mustPlays(doublePlay.get)}")
			true
		else
			false
	}

	if (impossibleEndgame)
		return true

	if (state.cardsLeft == 1)
		// At least 2 people need to play 2 cards
		if (mustStartEndgame.length > 1)
			// println(s"${indent(depth)}${mustStartEndgame.map(state.names)} need to start endgame, only 1 card left")
			return true

		if (mustStartEndgame.length == 1)
			val target = mustStartEndgame.head
			if (playerTurn != target && playersUntil(state.numPlayers, playerTurn, target).length > state.clueTokens)
				// println(s"${indent(depth)}${state.names(target)} needs to start endgame, not enough clues to reach their turn")
				return true

	else if (state.endgameTurns.isEmpty && voidPlayers.length > state.pace)
		// println(s"${indent(depth)}too many void players, pace ${state.pace}")
		return true

	false

/** Returns whether an endgame in the final round is winnable just by everyone playing what they know. */
def triviallyWinnable(game: Game, playerTurn: Int): WinnableResult =
	val state = game.state

	state.endgameTurns match {
		case Some(endgameTurns) if state.remScore <= endgameTurns =>
			val initial = (PerformAction.Discard(state.hands(playerTurn).head), state.playStacks)

			val (perform, finalPlayStacks) = (0 until endgameTurns).foldLeft(initial) { (acc, i) =>
				val (action, stacks) = acc
				val playerIndex = (playerTurn + i) % state.numPlayers
				val playables = game.players(playerIndex).obviousPlayables(game, playerIndex)

				if (playables.isEmpty)
					acc
				else state.deck(playables.head).id() match {
					case None => acc
					case Some(id) =>
						val perform = if (i == 0) PerformAction.Play(playables.head) else action
						(perform, stacks.updated(id.suitIndex, id.rank))
				}
			}

			Either.cond(finalPlayStacks.sum == state.maxScore, (List(perform), Frac.one), "")

		case _ => UNWINNABLE
	}

/** Generates a map of game arrangements for the possible actions.*/
def genArrs(game: Game, remaining: RemainingMap, clueOnly: Boolean, _depth: Int = 0): (List[GameArr], List[GameArr]) =
	val state = game.state
	val undrawn = GameArr(Frac.one, remaining, None)

	assert(remaining.values.map(_.missing).sum == state.cardsLeft, s"genArr failed $remaining ${state.cardsLeft}")

	val drawn =
		if (clueOnly)
			List()

		else if (remaining.size > 0 && remaining.forall((id, _) => state.isBasicTrash(id)))
			// Log.info(s"${indent(depth)}short-circuiting all remaining trash!")
			val (id, _) = remaining.head
			List(GameArr(Frac.one, remaining.rem(id), Some(id)))

		else
			val drawnArrs = remaining.map { case (id, RemainingEntry(missing, _)) =>
				val newProb = Frac(missing, state.cardsLeft)
				val newRemaining = remaining.rem(id)
				GameArr(newProb, newRemaining, Some(id))
			}.toList

			if (drawnArrs.isEmpty) List(undrawn) else drawnArrs

	(List(undrawn), drawn)
