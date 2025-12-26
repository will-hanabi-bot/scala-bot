package tests.reactor.invert

import scala_bot.reactor.Reactor
import scala_bot.basics._
import scala_bot.test.{hasStatus, Player, preClue, setup, takeTurn}, Player._
import scala_bot.logger.{Logger, LogLevel}
import scala.util.chaining.scalaUtilChainingOps

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
			assertEquals(g.takeAction, PerformAction.Discard(g.state.hands(Alice.ordinal)(1)))
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

	test("it doesn't receive a declined inversion discard"):
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
