package scala_bot.basics

import scala_bot.utils._
import scala_bot.logger.{Log, Logger, LogLevel}

def forceClue[G <: Game](game: G, giver: Int, advance: G => Double, only: Option[Int] = None)(using ops: GameOps[G]): Double =
	val state = game.state

	if !state.canClue then
		return -100.0

	val allClues =
		for
			i    <- 0 until state.numPlayers if i != giver && i != state.ourPlayerIndex && only.forall(_ == i)
			clue <- state.allValidClues(i)
		yield
			val list = state.clueTouched(state.hands(i), clue)
			ClueAction(giver, i, list, clue.toBase)

	val level = Logger.level
	allClues.maximizing(0.0): action =>
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
