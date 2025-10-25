package tests.refSieve.stable

import scala_bot.refSieve.RefSieve
import scala_bot.basics._
import scala_bot.test.{Colour, hasInfs, Player, preClue, setup, takeTurn, TestClue}, Player._
import scala_bot.logger.{Logger, LogLevel}

import scala.util.chaining.scalaUtilChainingOps

class Stable extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("it understands a ref play") {
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "g2", "r2", "r3", "g5"),
			Vector("p4", "b5", "p2", "b1", "g4")
		))
		.pipe(takeTurn("Alice clues green to Bob"))

		assertEquals(game.meta(game.state.hands(Bob.ordinal)(0)).status, CardStatus.CalledToPlay)
		hasInfs(game, None, Bob, 1, Vector("r1", "y1", "b1", "p1"))
	}

	test("it understands a gapped ref play") {
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p4", "b1", "p2", "b5", "g4"),
			Vector("b1", "g2", "r2", "r3", "g5"),
		))
		.pipe(takeTurn("Alice clues purple to Bob"))

		assertEquals(game.meta(game.state.hands(Bob.ordinal)(1)).status, CardStatus.CalledToPlay)
		hasInfs(game, None, Bob, 2, Vector("r1", "y1", "g1", "b1"))
	}

	test("it understands a chop ref play") {
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "b2", "p2", "b5", "g4"),
			Vector("b1", "g2", "r2", "r3", "g5"),
		))
		.pipe(takeTurn("Alice clues blue to Bob"))

		hasInfs(game, None, Bob, 1, Vector("b1"))
	}

	test("it understands a loaded colour clue") {
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "b2", "p2", "b5", "g4"),
			Vector("b1", "g2", "b2", "r3", "g5"),
		))
		.pipe(takeTurn("Alice clues 1 to Cathy"))
		.pipe(takeTurn("Bob clues red to Cathy"))
		.pipe(takeTurn("Cathy plays b1", "y3"))

		hasInfs(game, None, Cathy, 3, Vector("b2"))
	}

	test("it understands a playable colour clue is also referential") {
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "b2", "p2", "b5", "g4"),
			Vector("b1", "g2", "b2", "r3", "g5"),
		),
			playStacks = Some(Vector(4, 0, 0, 0, 0)),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 2)"))

		assertEquals(game.meta(game.state.hands(Alice.ordinal)(0)).status, CardStatus.CalledToPlay)
	}

	test("it understands a ref discard") {
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p4", "p2", "p2", "b5", "g3"),
			Vector("b1", "g2", "r2", "r3", "g5"),
		))
		.pipe(takeTurn("Alice clues 4 to Bob"))

		assertEquals(game.meta(game.state.hands(Bob.ordinal)(1)).status, CardStatus.CalledToDiscard)
	}

	test("it gives a ref discard") {
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p4", "p2", "p2", "b5", "g3"),
			Vector("b3", "g2", "r2", "r3", "g5"),
		))

		// Alice should clue 4 to Bob.
		assertEquals(game.takeAction, PerformAction.Rank(Bob.ordinal, 4))
	}

	test("it understands a lock") {
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p5", "p2", "p2", "b4", "g4"),
			Vector("b1", "g2", "r2", "r3", "g5"),
		))
		.pipe(takeTurn("Alice clues 4 to Bob"))

		assert(game.common.obviousLocked(game, Bob.ordinal))
	}

	test("it doesn't focus the wrong card for the last id") {
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "y1", "g1", "b1", "p1"),
			Vector("r1", "y1", "g1", "b1", "p1"),
		),
			playStacks = Some(Vector(3, 3, 3, 3, 2)),
			starting = Cathy,
			// Alice"s slot 5 is clued with purple.
			init = preClue(Alice, 5, Vector(TestClue(ClueKind.Colour, Colour.Purple.ordinal, Cathy)))
		)
		// Although Alice could play slot 2, she should play slot 5 first.
		.pipe(takeTurn("Cathy clues 3 to Alice (slots 2,5)"))

		assertEquals(game.takeAction, PerformAction.Play(0))
	}

	test("understands a trash push with rank") {
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "b1", "r2", "r4", "g5")
		),
			starting = Bob,
			playStacks = Some(Vector(1, 1, 1, 1, 1))
		)
		.pipe(takeTurn( "Bob clues 1 to Alice (slot 3)"))

		val slot2 = game.meta(game.state.hands(Alice.ordinal)(1))
		val playables = game.common.thinksPlayables(game, Alice.ordinal)

		assertEquals(slot2.status, CardStatus.CalledToPlay)
		assert(playables.contains(slot2.order))
	}

	test("understands a trash push touching old cards") {
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "b1", "r2", "r4", "g5")
		),
			starting = Bob,
			playStacks = Some(Vector(2, 2, 2, 2, 1))
		)
		.pipe(takeTurn("Bob clues 2 to Alice (slots 2,3)"))
		.pipe(takeTurn("Alice plays p2 (slot 2)"))
		.pipe(takeTurn("Bob clues 2 to Alice (slots 1,3)"))

		val slot5 = game.meta(game.state.hands(Alice.ordinal)(4))
		val playables = game.common.thinksPlayables(game, Alice.ordinal)

		assertEquals(slot5.status, CardStatus.CalledToPlay)
		assert(playables.contains(slot5.order))
	}

	test("wraps around a loaded trash push") {
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "b1", "r2", "r4", "g5")
		),
			starting = Bob,
			playStacks = Some(Vector(2, 2, 2, 2, 1))
		)
		.pipe(takeTurn("Bob clues 2 to Alice (slots 2,3)"))
		.pipe(takeTurn("Alice plays p2 (slot 2)"))
		.pipe(takeTurn("Bob clues 1 to Alice (slot 1)"))

		val slot5 = game.meta(game.state.hands(Alice.ordinal)(4))
		val playables = game.common.thinksPlayables(game, Alice.ordinal)

		assertEquals(slot5.status, CardStatus.CalledToPlay)
		assert(playables.contains(slot5.order))
	}
