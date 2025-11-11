package tests.hgroup.level1

import scala_bot.basics._
import scala_bot.test.{hasInfs, Player, setup, takeTurn}, Player._
import scala_bot.hgroup.HGroup
import scala_bot.logger.{Logger,LogLevel}

import scala.util.chaining.scalaUtilChainingOps

class Level1 extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("does not finesse from a 2 Save") {
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r5", "r4", "r2", "y4", "y2"),
			Vector("g5", "b4", "g1", "y2", "b3")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 2 to Bob"))

		hasInfs(game, None, Bob, 5, Vector("r2", "y2", "g2", "b2", "p2"))

		assert(game.common.thoughts(game.state.hands(Alice.ordinal)(0)).inferred.length > 0)
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(0)).status, CardStatus.None)
	}

	test("counts playables connecting on unknown plays") {
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "g1", "p5", "r2", "y2"),
			Vector("g2", "g3", "p2", "p1", "b4")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 1 to Bob"))

		val hypo = takeTurn("Alice clues 2 to Cathy")(game)

		val (_, playables) = playablesResult(game, hypo)
		assertEquals(playables.length, 1)
	}
