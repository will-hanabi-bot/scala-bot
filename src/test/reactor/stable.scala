package tests.reactor.stable

import scala_bot.reactor.Reactor
import scala_bot.basics._
import scala_bot.test.{Colour, hasInfs, Player, preClue, setup, takeTurn, TestClue}, Player._
import scala.util.chaining.scalaUtilChainingOps
import scala_bot.logger.{Logger, LogLevel}

class Stable extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("it understands a ref play") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "g2", "r2", "r3", "g5"),
			Vector("p4", "b5", "p2", "b1", "g4")
		))
		.pipe(takeTurn("Alice clues green to Bob"))

		assertEquals(game.meta(game.state.hands(Bob.ordinal)(0)).status, CardStatus.CalledToPlay)
		hasInfs(game, None, Bob, 1, Vector("r1", "y1", "b1", "p1"))
	}

	test("it understands a gapped ref play") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p4", "b1", "p2", "b5", "g4"),
			Vector("b1", "g2", "r2", "r3", "g5"),
		))
		.pipe(takeTurn("Alice clues purple to Bob"))

		assertEquals(game.meta(game.state.hands(Bob.ordinal)(1)).status, CardStatus.CalledToPlay)
		hasInfs(game, None, Bob, 2, Vector("r1", "y1", "g1", "b1"))
	}

	test("it understands a chop ref play") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "b2", "p2", "b5", "g4"),
			Vector("b1", "g2", "r2", "r3", "g5"),
		))
		.pipe(takeTurn("Alice clues blue to Bob"))

		hasInfs(game, None, Bob, 1, Vector("b1"))
	}

	test("it understands a ref discard") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p4", "p2", "p2", "b5", "g3"),
			Vector("b1", "g2", "r2", "r3", "g5"),
		))
		.pipe(takeTurn("Alice clues 4 to Bob"))

		assertEquals(game.meta(game.state.hands(Bob.ordinal)(1)).status, CardStatus.CalledToDiscard)
	}

	test("it gives a ref discard") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p4", "p2", "p2", "b5", "g3"),
			Vector("b3", "g2", "r2", "r3", "g5"),
		))

		// Alice should clue 4 to Bob.
		assertEquals(game.takeAction, PerformAction.Rank(Bob.ordinal, 4))
	}

	test("eliminates direct ranks from focus") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p4", "p2", "p2", "r5", "g3"),
			Vector("b5", "y4", "g2", "r4", "y3")
		),
			starting = Cathy,
			playStacks = Some(Vector(1, 1, 0, 1, 1))
		)
		.pipe(takeTurn("Cathy clues 1 to Alice (slots 2,3)"))

		assertEquals(game.meta(game.state.hands(Alice.ordinal)(3)).status, CardStatus.None)
		hasInfs(game, None, Alice, 2, Vector("g1"))

		// Alice"s slot 3 should be trash
		val trash = game.common.thinksTrash(game, Alice.ordinal)
		assert(trash.contains(game.state.hands(Alice.ordinal)(2)))
	}

	test("it understands a lock") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p4", "p2", "p2", "b5", "g4"),
			Vector("b1", "g2", "r2", "r3", "g5"),
		))
		.pipe(takeTurn("Alice clues 4 to Bob"))

		assert(game.common.obviousLocked(game, Bob.ordinal))
	}

	test("it doesn't focus the wrong card for the last id") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "y1", "g1", "b1", "p1"),
			Vector("r1", "y1", "g1", "b1", "p1"),
		),
			playStacks = Some(Vector(3, 3, 3, 3, 2)),
			starting = Cathy,
			// Alice's slot 5 is clued with purple.
			init = preClue(Alice, 5, Vector(TestClue(ClueKind.Colour, Colour.Purple.ordinal, Cathy)))
		)
		// Although Alice could play slot 2, she should play slot 5 first.
		.pipe(takeTurn("Cathy clues 3 to Alice (slots 2,5)"))

		assertEquals(game.takeAction, PerformAction.Play(0))
	}
