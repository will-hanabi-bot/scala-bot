package tests.reactor

import cats.effect.unsafe.implicits.global

import scala_bot.reactor.Reactor
import scala_bot.basics._
import scala_bot.test.{fullyKnown, hasStatus, Player, preClue, setup, takeTurn}, Player._

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger, LogLevel}

class Invert extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("it reacts to a response inversion"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g1", "y5", "g4", "b4", "b4"),
			Vector("b1", "r1", "r4", "y4", "y4"),
		),
			starting = Cathy,
			init = preClue(Alice, 5, Seq("1"))
		)
		.pipe(takeTurn("Cathy clues green to Bob"))
		.tap: g =>
			hasStatus(g, Alice, 2, CardStatus.CalledToDiscard)

			// We should discard slot 2 urgently.
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Discard(g.state.hands(Alice.ordinal)(1)))
		.pipe(takeTurn("Alice discards r4 (slot 2)"))

		hasStatus(game, Bob, 1, CardStatus.CalledToPlay)

	test("it receives a response inversion"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "y5", "g4", "b4", "b4"),
			Vector("y4", "r1", "r4", "y4", "y1"),
		),
			starting = Bob,
			init = preClue(Cathy, 5, Seq("1"))
		)
		.pipe(takeTurn("Bob clues green to Alice (slot 4)"))
		.tap: g =>
			hasStatus(g, Alice, 3, CardStatus.CalledToPlay)
		.pipe(takeTurn("Cathy plays r1", "r4"))

		hasStatus(game, Alice, 2, CardStatus.CalledToDiscard)
		hasStatus(game, Alice, 3, CardStatus.None)

	test("it doesn't receive a declined inversion play"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "y5", "g4", "b4", "b4"),
			Vector("y4", "r1", "r4", "y4", "y1"),
		),
			starting = Bob,
			init = preClue(Cathy, 5, Seq("1"))
		)
		.pipe(takeTurn("Bob clues green to Alice (slot 4)"))
		.tap: g =>
			hasStatus(g, Alice, 3, CardStatus.CalledToPlay)
		.pipe(takeTurn("Cathy plays y1", "r4"))

		hasStatus(game, Alice, 4, CardStatus.None)
		hasStatus(game, Alice, 3, CardStatus.CalledToPlay)

	test("it doesn't receive a declined inversion discard from chop"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "y5", "g4", "b4", "b3"),
			Vector("y4", "r1", "r4", "y4", "y1"),
		))
		// Lock Bob
		.pipe(takeTurn("Alice clues 3 to Bob"))
		.pipe(takeTurn("Bob clues 3 to Alice (slot 3)"))
		.tap: g =>
			hasStatus(g, Alice, 4, CardStatus.CalledToDiscard)
		.pipe(takeTurn("Cathy discards y4 (slot 1)", "r4"))

		hasStatus(game, Alice, 2, CardStatus.None)
		hasStatus(game, Alice, 4, CardStatus.CalledToDiscard)

	test("it doesn't receive a declined inversion discard from kt"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "y5", "g4", "b4", "b3"),
			Vector("y4", "r1", "r4", "y4", "y1"),
		),
			playStacks = Some(Vector(0, 1, 0, 0, 0)),
			init = fullyKnown(Cathy, 5, "y1")
		)
		// Lock Bob
		.pipe(takeTurn("Alice clues 3 to Bob"))
		.pipe(takeTurn("Bob clues 3 to Alice (slot 3)"))
		.tap: g =>
			hasStatus(g, Alice, 4, CardStatus.CalledToDiscard)
		.pipe(takeTurn("Cathy discards y1", "r4"))

		hasStatus(game, Alice, 3, CardStatus.None)
		hasStatus(game, Alice, 4, CardStatus.CalledToDiscard)

	test("it receives an inverted dc when the reacter is loaded"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "y5", "g4", "b4", "b3"),
			Vector("y4", "r1", "r4", "y4", "y1"),
		),
			clueTokens = 7,
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			init =
				fullyKnown[Reactor](Cathy, 2, "r1") andThen
				fullyKnown[Reactor](Cathy, 5, "y1")
		)
		.pipe(takeTurn("Alice clues 3 to Bob"))
		.pipe(takeTurn("Bob clues red to Alice (slot 3)"))		// Since Cathy is loaded, this is stable
		.tap: g =>
			hasStatus(g, Alice, 2, CardStatus.CalledToPlay)
		.pipe(takeTurn("Cathy discards r1 (slot 2)", "r4"))		// Even though this is kt, Cathy is expected to play

		hasStatus(game, Alice, 1, CardStatus.CalledToPlay)
		hasStatus(game, Alice, 2, CardStatus.None)

	test("it doesn't give a response inversion to dc kt when reactor is unloaded"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "r1", "g4", "b4", "b3"),
			Vector("p5", "y1", "r4", "g3", "y4"),
		),
			starting = Cathy,
			clueTokens = 7,
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			init = fullyKnown(Bob, 2, "r1")
		)
		.pipe(takeTurn("Cathy clues 5 to Alice (slot 5)"))		// Lock Alice
		.pipe(takeTurn("Alice clues green to Cathy"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("it gives a response inversion to dc kt when reacter is loaded"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "r1", "g4", "b4", "b1"),
			Vector("p5", "y1", "r4", "g3", "y4"),
		),
			starting = Cathy,
			clueTokens = 7,
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			init =
				fullyKnown[Reactor](Bob, 2, "r1") andThen
				fullyKnown[Reactor](Bob, 5, "b1")
		)
		.pipe(takeTurn("Cathy clues 5 to Alice (slot 5)"))		// Lock Alice
		.pipe(takeTurn("Alice clues green to Cathy"))
		.tap: g =>
			assertEquals(g.lastMove, Some(ClueInterp.Reactive))
		.pipe(takeTurn("Bob discards r1", "r3"))

		// Since Bob was expected to play b1, discarding r1 is a reaction.
		hasStatus(game, Cathy, 2, CardStatus.CalledToPlay)

	test("it understands a bad lock"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "y5", "g1", "b4", "b1"),
			Vector("y4", "r1", "b4", "y4", "y3"),
		),
			init = preClue(Bob, 5, Seq("1"))
		)
		// Blue is available to push Cathy's r1.
		.pipe(takeTurn("Alice clues 3 to Cathy"))
		.tap: g =>
			hasStatus(g, Bob, 3, CardStatus.CalledToPlay)
		.pipe(takeTurn("Bob plays g1", "r4"))

		hasStatus(game, Cathy, 2, CardStatus.CalledToPlay)
		assert(!game.common.obviousLocked(game, Cathy.ordinal))
