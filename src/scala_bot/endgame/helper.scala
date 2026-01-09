package scala_bot.endgame

import scala_bot.basics._
import scala_bot.fraction.Frac
import scala_bot.utils._
// import scala_bot.logger.Log
import scala_bot.utils.playersUntil

def findMustPlays(state: State, hand: Vector[Int]) =
	val ids = hand.fastMap(state.deck(_).id())
	var ret = List.empty[Identity]

	loopIf(0, _ < hand.length, _ + 1, ids(_).isDefined): i =>
		val id = ids(i).get
		if !state.isBasicTrash(id) then
			var matches = 1

			loop(i + 1, _ < hand.length, _ + 1): j =>
				if ids(j).isDefined && ids(j).get.toOrd == id.toOrd then
					matches += 1

			if matches == state.cardCount(id.toOrd) - state.baseCount(id.toOrd) then
				ret = id +: ret

	ret

def unwinnableState(state: State, playerTurn: Int, depth: Int = 0): Boolean =
	if state.ended || state.pace < 0 then
		return true

	var voidPlayers = List.empty[Int]
	var mustPlays = List.empty[List[Identity]]
	var mustStartEndgame = List.empty[Int]

	loop(state.numPlayers - 1, _ >= 0, _ - 1): i =>
		val hand = state.hands(i)
		val void = hand.fastForall(state.deck(_).id().forall(state.isBasicTrash))

		if void then
			voidPlayers = i +: voidPlayers

		val plays = findMustPlays(state, hand)
		mustPlays = plays +: mustPlays

		if (plays.length > 1)
			mustStartEndgame = i +: mustStartEndgame

	// println(s"${indent(depth)}void players: $voidPlayers, endgame_turns: ${state.endgameTurns}, current turn: ${state.names(playerTurn)}")

	val impossibleEndgame = state.endgameTurns.isDefined && {
		var possiblePlayers = 0
		var doublePlay = -1

		loop(0, _ < state.endgameTurns.get, _ + 1): i =>
			val playerIndex = (playerTurn + i) % state.numPlayers

			if !voidPlayers.contains(playerIndex) then
				possiblePlayers += 1

				if mustPlays(playerIndex).length > 1 then
					doublePlay = i

		if possiblePlayers + state.score < state.maxScore then
			// println(s"${indent(depth)}even if everyone (${possiblePlayers}) plays, can't reach max (${state.score}/${state.maxScore})")
			true
		else if doublePlay != -1 then
			// println(s"${indent(depth)}final round has started, ${state.names(doublePlay.get)} still needs to play ${mustPlays(doublePlay.get)}")
			true
		else
			false
	}

	if impossibleEndgame then
		return true

	if state.cardsLeft == 1 then
		// At least 2 people need to play 2 cards
		if mustStartEndgame.length > 1 then
			// println(s"${indent(depth)}${mustStartEndgame.map(state.names)} need to start endgame, only 1 card left")
			return true

		if mustStartEndgame.length == 1 then
			val target = mustStartEndgame.head
			if playerTurn != target && playersUntil(state.numPlayers, playerTurn, target).length > state.clueTokens then
				// println(s"${indent(depth)}${state.names(target)} needs to start endgame, not enough clues to reach their turn")
				return true

	else if state.endgameTurns.isEmpty && voidPlayers.length > state.pace then
		// println(s"${indent(depth)}too many void players, pace ${state.pace}")
		return true

	false

/** Returns whether an endgame in the final round is winnable just by everyone playing what they know. */
def triviallyWinnable(game: Game, playerTurn: Int): WinnableResult =
	val state = game.state

	state.endgameTurns match
		case Some(endgameTurns) if state.remScore <= endgameTurns =>
			val initial = (PerformAction.Discard(state.hands(playerTurn).head), state.playStacks)

			val (perform, finalPlayStacks) = (0 until endgameTurns).foldLeft(initial): (acc, i) =>
				val (action, stacks) = acc
				val playerIndex = (playerTurn + i) % state.numPlayers
				val playables = game.players(playerIndex).obviousPlayables(game, playerIndex)

				if playables.isEmpty then
					acc
				else state.deck(playables.head).id() match
					case None => acc
					case Some(id) =>
						val perform = if i == 0 then PerformAction.Play(playables.head) else action
						(perform, stacks.updated(id.suitIndex, id.rank))

			Either.cond(finalPlayStacks.sum == state.maxScore, (List(perform), Frac.one), "")

		case _ => UNWINNABLE

/** Generates a map of game arrangements for the possible actions.*/
def genArrs(game: Game, remaining: RemainingMap, clueOnly: Boolean): (List[GameArr], List[GameArr]) =
	val state = game.state
	val undrawn = GameArr(Frac.one, remaining, None)

	assert(remaining.values.summing(_.missing) == state.cardsLeft, s"genArr failed ${remaining.fmt2(state)} ${state.cardsLeft}")

	val drawn =
		if clueOnly then
			List()

		else if remaining.size > 0 && remaining.forall((id, _) => state.isBasicTrash(id)) then
			// Log.info(s"${indent(depth)}short-circuiting all remaining trash!")
			val (id, _) = remaining.head
			List(GameArr(Frac.one, remaining.rem(id), Some(id)))

		else
			val drawnArrs = {
				val (usefulArrs, trashArr) = remaining.foldLeft((List.empty[GameArr], GameArr(Frac.zero, remaining, None))):
					case ((acc, trashArr), (id, RemainingEntry(missing, _))) if state.isBasicTrash(id) =>
						val newProb = Frac(missing, state.cardsLeft)
						val newRemaining = remaining.rem(id)
						(acc, trashArr.copy(prob = trashArr.prob + newProb, remaining = newRemaining, drew = Some(id)))

					case ((acc, trashArr), (id, RemainingEntry(missing, _))) =>
						val newProb = Frac(missing, state.cardsLeft)
						val newRemaining = remaining.rem(id)
						(GameArr(newProb, newRemaining, Some(id)) +: acc, trashArr)
				if trashArr.prob > Frac.zero then
					usefulArrs :+ trashArr
				else
					usefulArrs
			}.toList

			if drawnArrs.isEmpty then List(undrawn) else drawnArrs

	(List(undrawn), drawn)
