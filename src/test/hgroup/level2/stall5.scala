package tests.hgroup.level2

import scala_bot.basics._
import scala_bot.test.{setup, takeTurn}
import scala_bot.hgroup.{HGroup, StallInterp}
import scala_bot.logger.{Logger, LogLevel}

import scala.util.chaining.scalaUtilChainingOps

class Stall5 extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("gives a 5 stall before ending early game"):
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "r5", "b4", "y4", "b3")
		),
			clueTokens = 7
		)
		.pipe(takeTurn("Alice clues 5 to Bob"))

		assert(game.lastMove == Some(ClueInterp.Stall))
		assert(game.stallInterp == Some(StallInterp.Stall5))

	test("doesn't 5 stall on a trash 5"):
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "r5", "b4", "y4", "b3")
		),
			discarded = Vector("r4", "r4"),
			clueTokens = 7
		)
		.pipe(takeTurn("Alice clues 5 to Bob"))

		assert(game.lastMove == Some(ClueInterp.Mistake))
