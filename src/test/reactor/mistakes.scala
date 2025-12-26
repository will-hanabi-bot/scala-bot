package tests.reactor.mistakes

import scala_bot.reactor.Reactor
import scala_bot.basics._
import scala_bot.test.{Player, setup, takeTurn}, Player._
import scala_bot.logger.{Logger, LogLevel}
import scala.util.chaining.scalaUtilChainingOps

class Mistakes extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("it cancels a missed reaction 1"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g1", "r1", "g4", "b4", "b4"),
			Vector("b1", "r3", "r4", "y4", "y4"),
		))
		.pipe(takeTurn("Alice clues 4 to Cathy"))
		.tap { g =>
			// Bob is called to play r1 (slot 2) -> Cathy plays b1 (slot 1).
			assertEquals(g.meta(g.state.hands(Bob.ordinal)(1)).status, CardStatus.CalledToPlay)
		}
		.pipe(takeTurn("Bob discards g1", "y3"))

		val bobS2 = game.state.hands(Bob.ordinal)(1)

		// Bob is no longer called to play r1, and that card can be anything.
		assertEquals(game.meta(bobS2).status, CardStatus.None)
		assertEquals(game.common.thoughts(bobS2).inferred.length, game.common.thoughts(bobS2).possible.length)

		// Cathy is not called to play slot 1 (Cathy might have some wrong priority elim notes).
		assertEquals(game.meta(game.state.hands(Cathy.ordinal)(0)).status, CardStatus.None)

	test("it cancels a missed reaction 2"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g1", "r1", "g4", "b4", "b4"),
			Vector("b1", "r1", "r4", "y4", "y4"),
		))
		.pipe(takeTurn("Alice clues 4 to Cathy"))
		.tap { g =>
			// Bob is called to play r1 (slot 2) -> Cathy plays b1 (slot 1).
			assertEquals(g.meta(g.state.hands(Bob.ordinal)(1)).status, CardStatus.CalledToPlay)
		}
		.pipe(takeTurn("Bob plays g1", "y3"))

		val bobS2 = game.state.hands(Bob.ordinal)(1)

		// Bob is no longer called to play r1, and that card can be anything.
		assertEquals(game.meta(bobS2).status, CardStatus.None)
		assertEquals(game.common.thoughts(bobS2).inferred.length, game.common.thoughts(bobS2).possible.length)

		// Cathy is not called to play slot 1 (Cathy might have some wrong priority elim notes).
		assertEquals(game.meta(game.state.hands(Cathy.ordinal)(0)).status, CardStatus.None)
