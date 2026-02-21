package tests.reactor.variants

import scala_bot.reactor.Reactor
import scala_bot.basics._
import scala_bot.test.{hasInfs, hasStatus, Player, setup, takeTurn, TestVariant}, Player._

import scala_bot.utils.pipe
import scala_bot.logger.{Logger, LogLevel}

class Variants extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("it understands a playable pink promise"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "r1", "r4", "y4", "y4"),
			Vector("g4", "g1", "g4", "b4", "b4")
		),
			playStacks = Some(Vector(1, 2, 1, 1, 2)),
			variant = TestVariant.Pink5,
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 2 to Alice (slots 2,4)"))

		// Alice should play slot 2.
		assertEquals(game.takeAction, PerformAction.Play(game.state.hands(Alice.ordinal)(1)))
		hasInfs(game, None, Alice, 2, Vector("r2", "g2", "b2"))

		// Alice's slot 4 is not playable.
		val playables = game.common.obviousPlayables(game, Alice.ordinal)
		assert(playables.length == 1 && playables(0) == game.state.hands(Alice.ordinal)(1))

	test("it understands a brown tcm"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "r1", "r4", "y4", "y4"),
			Vector("g4", "g1", "g4", "b4", "b4")
		),
			playStacks = Some(Vector(1, 2, 1, 1, 2)),
			variant = TestVariant.Brown5,
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 1 to Alice (slots 2,4)"))

		// Alice does not have a playable.
		assert(game.common.obviousPlayables(game, Alice.ordinal).isEmpty)
		hasStatus(game, Alice, 1, CardStatus.None)
