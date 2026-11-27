package scala_bot.endgame

import scala_bot.basics._
import scala_bot.lib.{FastBitSet, Frac}
import scala_bot.utils._
// import scala_bot.logger.Log
import scala_bot.utils.playersUntil

def findMustPlays(state: State, hand: Vector[Int]) =
	val ids = hand.fastMap(state.deck(_).id())
	var ret = List.empty[Identity]

	loopIf(0, _ < hand.length, _ + 1, ids(_).isDefined): i =>
		val id = ids(i).get
		if state.isUseful(id) then
			var matches = 1

			loop(i + 1, _ < hand.length, _ + 1): j =>
				if ids(j).isDefined && ids(j).get.toOrd == id.toOrd then
					matches += 1

			if matches == state.cardCount(id.toOrd) - state.baseCount(id.toOrd) then
				ret = id +: ret

	ret

def unwinnableState(state: State, playerTurn: Int, remaining: RemainingMap, depth: Int, target: Int): Boolean =
	val effPace = state.pace + (state.maxScore - target)

	if state.ended || state.maxScore < target || effPace < 0 then
		return true

	val isVoid = Array.fill(state.numPlayers)(false)
	val mustPlays = Array.fill(state.numPlayers)(0)
	var mustStartEndgame: FastBitSet = FastBitSet.empty
	val allPlays = Array.fill(state.numPlayers)(List.empty[Identity])

	loop(state.numPlayers - 1, _ >= 0, _ - 1): i =>
		val hand = state.hands(i)
		val void = hand.forall(state.deck(_).id().forall(state.isBasicTrash))

		if void then
			isVoid(i) = true

		val plays = findMustPlays(state, hand)
		mustPlays(i) += plays.length
		allPlays(i) = plays

		if (plays.length > 1)
			mustStartEndgame = mustStartEndgame.incl(i)

	// println(s"${indent(depth)}void players: $voidPlayers, endgame_turns: ${state.endgameTurns}, current turn: ${state.names(playerTurn)}")

	val impossibleEndgame = state.endgameTurns.isDefined && {
		var possiblePlayers = 0
		var doublePlay = -1

		loop(0, _ < state.endgameTurns.get, _ + 1): i =>
			val playerIndex = (playerTurn + i) % state.numPlayers

			if !isVoid(playerIndex) then
				possiblePlayers += 1

				if mustPlays(playerIndex) > 1 then
					doublePlay = i

		if possiblePlayers + state.score < target then
			// println(s"${indent(depth)}even if everyone (${possiblePlayers}) plays, can't reach target (${state.score}/$target)")
			true
		else if target == state.maxScore && doublePlay != -1 then
			// println(s"${indent(depth)}final round has started, ${state.names(doublePlay.get)} still needs to play ${mustPlays(doublePlay.get)}")
			true
		else
			false
	}

	if impossibleEndgame then
		return true

	if state.cardsLeft == 1 then
		if target == state.maxScore then
			// At least 2 people need to play 2 cards
			if mustStartEndgame.size > 1 then
				// println(s"${indent(depth)}${mustStartEndgame.map(state.names)} need to start endgame, only 1 card left")
				return true

			if mustStartEndgame.size == 1 then
				val starter = mustStartEndgame.head
				if playerTurn != starter && playersUntil(state.numPlayers, playerTurn, starter).length > state.clueTokens then
					// println(s"${indent(depth)}${state.names(starter)} needs to start endgame, not enough clues to reach their turn")
					return true

			// If there is a critical card on the bottom and only 1 person has a playable card, that person must start the endgame.
			val remainingId = remaining.keys.head

			if state.isCritical(remainingId) && state.remScore == 3 && state.playStacks.contains(3) then
				var hold4 = -1
				var hold5 = -1
				var playableIndices = FastBitSet.empty

				loop(0, _ < allPlays.length, _ + 1): i =>
					val plays = allPlays(i)
					if plays.exists(_.rank == 4) then
						hold4 = i
					if plays.exists(_.rank == 5) then
						hold5 = i
					if plays.exists(state.isPlayable) then
						playableIndices = playableIndices.incl(i)

				if hold4 != -1 && hold5 != -1 && hold4 != hold5 && playableIndices.size == 1 then
					val firstOk = state.nextPlayerIndex(hold5)
					if playersUntil(state.numPlayers, playerTurn, firstOk).length > state.clueTokens then
						// println(s"${indent(depth)}cannot reach first ok ${state.names(firstOk)} with ${state.logId(remainingId)} on bottom!")
						return true

	else if state.endgameTurns.isEmpty && isVoid.count(_ == true) > effPace then
		// println(s"${indent(depth)}too many void players, pace $effPace")
		return true

	false

/** Returns whether an endgame in the final round is winnable just by everyone playing what they know. */
def triviallyWinnable[G <: Game](game: G, playerTurn: Int, target: Int)(using ops: GameOps[G]): WinnableResult =
	val state = game.state

	state.endgameTurns match
		case Some(endgameTurns) if target - state.score <= endgameTurns =>
			val defaultAction =
				if state.clueTokens == 8 then
					ops.findAllClues(game, playerTurn).headOption.getOrElse(PerformAction.tryDiscard(game, state.hands(playerTurn).head))
				else
					ops.findAllDiscards(game, playerTurn).headOption.getOrElse(PerformAction.tryDiscard(game, state.hands(playerTurn).head))
			val initial = (defaultAction, state.playStacks)

			val (perform, finalPlayStacks) = (0 until endgameTurns).foldLeft(initial): (acc, i) =>
				val (action, stacks) = acc
				val playerIndex = (playerTurn + i) % state.numPlayers
				val playables = game.players(playerIndex).obviousPlayables(game, playerIndex)

				if playables.isEmpty then
					acc
				else
					state.deck(playables.head).id().fold(acc): id =>
						val perform = if i == 0 then PerformAction.tryPlay(game, playables.head) else action
						(perform, stacks.updated(id.suitIndex, id.rank))

			Either.cond(finalPlayStacks.sum >= target, (List(perform), Frac.one), "")

		case _ => UNWINNABLE

/** Generates a map of game arrangements for the possible actions.*/
def genArrs(game: Game, remaining: RemainingMap, playerTurn: Int, clueOnly: Boolean): (List[GameArr], List[GameArr]) =
	val state = game.state
	val undrawn = GameArr(Frac.one, remaining, None)

	assert(remaining.values.sum == state.cardsLeft, s"genArr failed ${remaining.fmt2(state)} ${state.cardsLeft}")

	val drawn =
		if clueOnly then
			Nil

		else if remaining.size > 0 && remaining.forall((id, _) => state.isBasicTrash(id)) then
			// Log.info(s"${indent(depth)}short-circuiting all remaining trash!")
			val (id, _) = remaining.head
			List(GameArr(Frac.one, remaining.rem(id), Some(id)))

		else
			val drawnArrs =
				val (usefulArrs, trashArr) = remaining.foldLeft((List.empty[GameArr], GameArr(Frac.zero, remaining, None))):
					case ((acc, trashArr), (id, missing)) if state.isBasicTrash(id) || state.hands(playerTurn).exists(game.common.thoughts(_).matches(id, infer = true)) =>
						val newProb = Frac(missing, state.cardsLeft)
						val newRemaining = remaining.rem(id)
						(acc, trashArr.copy(prob = trashArr.prob + newProb, remaining = newRemaining, drew = Some(id)))

					case ((acc, trashArr), (id, missing)) =>
						val newProb = Frac(missing, state.cardsLeft)
						val newRemaining = remaining.rem(id)
						(GameArr(newProb, newRemaining, Some(id)) +: acc, trashArr)

				if trashArr.prob > Frac.zero then
					usefulArrs :+ trashArr
				else
					usefulArrs

			if drawnArrs.isEmpty then List(undrawn) else drawnArrs

	(List(undrawn), drawn)
