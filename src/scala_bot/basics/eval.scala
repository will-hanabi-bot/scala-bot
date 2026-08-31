package scala_bot.basics

import scala_bot.utils._
import scala_bot.logger.{Log, Logger, LogLevel}
import scala_bot.basics.ForceClueResult._

enum ForceClueResult[G <: Game]:
	case Mistake()
	case Useless()
	case AssumeClueAvailable(value: Double)
	case BestClue(value: Double, action: ClueAction, hypo: G)

/** Evaluates the game state assuming the provided giver clues on their turn.
  * If there are no clue tokens or there are no visible clues (outside of 2p), returns -999.
  * In 2p, simply lowers the clue count by 1 before calling [[advance]].
  * @param game       The current game.
  * @param giver      The index of the player who will clue.
  * @param advance    Evaluates the game state; called after cluing.
  * @param only       If provided, only allows the giver to clue this player.
  * @param clueFilter Filters what clues are allowed to be given.
  */
def forceClue[G <: Game](game: G, giver: Int, advance: G => Double, offset: Int, only: Option[Int] = None, clueFilter: Clue => Boolean = _ => true)(using ops: GameOps[G]): ForceClueResult[G] =
	val state = game.state

	if !state.canClue then
		return Mistake()

	if state.numPlayers == 2 && giver != state.ourPlayerIndex then
		Log.info(s"${indent(offset)}${state.names(giver)} cluing (hypothetically)")
		return AssumeClueAvailable(advance(game.withState(s => s.copy(clueTokens = s.clueTokens - 1, endgameTurns = s.endgameTurns.map(_ - 1)))))

	val allClues =
		for
			i    <- 0 until state.numPlayers if i != giver && i != state.ourPlayerIndex && only.forall(_ == i)
			clue <- state.allValidClues(i) if clueFilter(clue)
		yield
			val list = state.clueTouched(state.hands(i), clue)
			ClueAction(giver, i, list, clue.base)

	val level = Logger.level
	val result: ForceClueResult[G] = allClues.foldLeft(Mistake()): (acc, action) =>
		Logger.setLevel(LogLevel.Off)
		val hypoGame = game.simulate(action, log = Some(false))

		if hypoGame.lastMove == Some(ClueInterp.Mistake) then
			Logger.setLevel(level)
			// Log.highlight(Console.YELLOW, s"${indent(offset)}${action.fmt(state)}: -100 (mistake)")
			acc
		else if hypoGame.lastMove == Some(ClueInterp.Useless) then
			Logger.setLevel(level)
			// Log.highlight(Console.YELLOW, s"${action.fmt(state)}: -50 (useless)")
			if acc.matchesP { case Mistake() => true } then Useless() else acc
		else
			val value = advance(hypoGame)
			Logger.setLevel(level)
			Log.highlight(Console.YELLOW, f"${indent(offset)}${action.fmt(state)}: $value%.2f")
			BestClue(value, action, hypoGame)

	result match
		case x @ BestClue(_, _, _) => x
		case _ =>
			// Hope they can clue something in our hand
			AssumeClueAvailable(advance(game.withState(s => s.copy(clueTokens = s.clueTokens - 1, endgameTurns = s.endgameTurns.map(_ - 1)))))
