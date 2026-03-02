package scala_bot.basics

import scala_bot.utils._
import scala_bot.logger.{Log, Logger, LogLevel}

/** Evaluates the game state assuming the provided giver clues on their turn.
  * If there are no clue tokens or there are no visible clues (outside of 2p), returns -999.
  * In 2p, simply lowers the clue count by 1 before calling [[advance]].
  * @param game       The current game.
  * @param giver      The index of the player who will clue.
  * @param advance    Evaluates the game state; called after cluing.
  * @param only       If provided, only allows the giver to clue this player.
  * @param clueFilter Filters what clues are allowed to be given.
  */
def forceClue[G <: Game](game: G, giver: Int, advance: G => Double, only: Option[Int] = None, clueFilter: Clue => Boolean = _ => true)(using ops: GameOps[G]): Double =
	val state = game.state

	if !state.canClue then
		return -999.0

	if state.numPlayers == 2 then
		return advance(game.withState(s => s.copy(clueTokens = s.clueTokens - 1)))

	val allClues =
		for
			i    <- 0 until state.numPlayers if i != giver && i != state.ourPlayerIndex && only.forall(_ == i)
			clue <- state.allValidClues(i) if clueFilter(clue)
		yield
			val list = state.clueTouched(state.hands(i), clue)
			ClueAction(giver, i, list, clue.base)

	val level = Logger.level
	val value = allClues.maximizing(-100.0): action =>
		Logger.setLevel(LogLevel.Off)
		val hypoGame = game.simulate(action)

		if hypoGame.lastMove == Some(ClueInterp.Mistake) then
			Logger.setLevel(level)
			-100.0
		else
			val value = advance(hypoGame)
			Logger.setLevel(level)
			Log.highlight(Console.YELLOW, s"${action.fmt(state)}: $value")
			value

	if value == -100 then
		// Hope they can clue something in our hand
		advance(game.withState(s => s.copy(clueTokens = s.clueTokens - 1)))
	else
		value
