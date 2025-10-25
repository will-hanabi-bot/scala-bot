package tests.reactor.invert

import scala_bot.reactor.Reactor
import scala_bot.basics._
import scala_bot.test.{Player, preClue, setup, takeTurn, TestClue}, Player._
import scala_bot.logger.{Logger, LogLevel}
import scala.util.chaining.scalaUtilChainingOps

class Invert extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("it reacts to a response inversion") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g1", "y5", "g4", "b4", "b4"),
			Vector("b1", "r1", "r4", "y4", "y4"),
		),
			starting = Cathy,
			// Alice has a clued 1 in slot 5.
			init = preClue(Alice, 5, Vector(TestClue(ClueKind.Rank, 1, Bob)))
		)
		.pipe(takeTurn("Cathy clues green to Bob"))
		.tap { g =>
			// We are called to discard slot 2.
			assertEquals(g.meta(g.state.hands(Alice.ordinal)(1)).status, CardStatus.CalledToDiscard)

			// We should discard slot 2 urgently.
			assertEquals(g.takeAction, PerformAction.Discard(g.state.hands(Alice.ordinal)(1)))
		}
		.pipe(takeTurn("Alice discards r4 (slot 2)"))

		// Bob is called to play g1.
		assertEquals(game.meta(game.state.hands(Bob.ordinal)(0)).status, CardStatus.CalledToPlay)
	}

	test("it receives a response inversion") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "y5", "g4", "b4", "b4"),
			Vector("y4", "r1", "r4", "y4", "y1"),
		),
			starting = Bob,
			// Cathy has a clued 1 in slot 5.
			init = preClue(Cathy, 5, Vector(TestClue(ClueKind.Rank, 1, Alice)))
		)
		.pipe(takeTurn("Bob clues green to Alice (slot 4)"))
		.tap { g =>
			// We are called to play slot 3.
			assertEquals(g.meta(g.state.hands(Alice.ordinal)(2)).status, CardStatus.CalledToPlay)
		}
		.pipe(takeTurn("Cathy plays r1", "r4"))

		// We are called to discard slot 2.
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(1)).status, CardStatus.CalledToDiscard)

		// Slot 3 is no longer called to play.
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(2)).status, CardStatus.None)
	}

	test("it doesn't receive a declined inversion play") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "y5", "g4", "b4", "b4"),
			Vector("y4", "r1", "r4", "y4", "y1"),
		),
			starting = Bob,
			// Cathy has a clued 1 in slot 5.
			init = preClue(Cathy, 5, Vector(TestClue(ClueKind.Rank, 1, Alice)))
		)
		.pipe(takeTurn("Bob clues green to Alice (slot 4)"))
		.tap { g =>
			// We are called to play slot 3.
			assertEquals(g.meta(g.state.hands(Alice.ordinal)(2)).status, CardStatus.CalledToPlay)
		}
		.pipe(takeTurn("Cathy plays y1", "r4"))

		// We are not called to discard slot 4.
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(3)).status, CardStatus.None)

		// Slot 3 is still called to play.
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(2)).status, CardStatus.CalledToPlay)
	}

	test("it doesn't receive a declined inversion discard") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "y5", "g4", "b4", "b3"),
			Vector("y4", "r1", "r4", "y4", "y1"),
		))
		// Lock Bob
		.pipe(takeTurn("Alice clues 3 to Bob"))
		.pipe(takeTurn("Bob clues 3 to Alice (slot 3)"))
		.tap { g =>
			// We are called to discard slot 4.
			assertEquals(g.meta(g.state.hands(Alice.ordinal)(3)).status, CardStatus.CalledToDiscard)
		}
		.pipe(takeTurn("Cathy discards y4 (slot 1)", "r4"))

		// We are not called to play slot 2.
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(1)).status, CardStatus.None)

		// Slot 4 is still called to discard.
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(3)).status, CardStatus.CalledToDiscard)
	}

	test("it understands a bad lock") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "y5", "g1", "b4", "b1"),
			Vector("y4", "r1", "b4", "y4", "y3"),
		),
			// Bob has a clued 1 in slot 5.
			init = preClue(Bob, 5, Vector(TestClue(ClueKind.Rank, 1, Alice)))
		)
		// Blue is available to push Cathy's r1.
		.pipe(takeTurn("Alice clues 3 to Cathy"))
		.tap { g =>
			// Bob is called to play slot 3.
			assertEquals(g.meta(g.state.hands(Bob.ordinal)(2)).status, CardStatus.CalledToPlay)
		}
		.pipe(takeTurn("Bob plays g1", "r4"))

		// Cathy is called to play slot 2.
		assertEquals(game.meta(game.state.hands(Cathy.ordinal)(1)).status, CardStatus.CalledToPlay)
		assert(!game.common.obviousLocked(game, Cathy.ordinal))
	}
