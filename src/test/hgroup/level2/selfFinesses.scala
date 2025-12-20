package tests.hgroup.level2

import scala_bot.basics._
import scala_bot.test.{Colour, hasInfs, Player, preClue, setup, takeTurn, TestClue}, Player._
import scala_bot.hgroup.HGroup
import scala_bot.logger.{Logger, LogLevel}

import scala.util.chaining.scalaUtilChainingOps

class SelfFinesses extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("plays into a self-finesse") {
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g1", "p4", "b4", "b4", "y4")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 2 to Alice (slot 1)"))
		.tap { g =>
			assertEquals(g.meta(g.state.hands(Alice.ordinal)(1)).status, CardStatus.Finessed)
		}
		.pipe(takeTurn("Alice plays g1 (slot 2)"))

		hasInfs(game, None, Alice, 2, Vector("g2"))
	}

	test("doesn't give a bad self-finesse") {
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g1", "p4", "b4", "b4", "y4"),
			Vector("g3", "g2", "r4", "r4", "y4")
		),
			starting = Cathy,
			playStacks = Some(Vector(2, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Cathy clues green to Bob"))
		.pipe(takeTurn("Alice clues 3 to Cathy"))

		// This clue is illegal.
		assertEquals(game.lastMove, Some(ClueInterp.Mistake))
	}

	test("interprets a self-finesse when giver knows less") {
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "r1", "g4", "b1", "g3"),
			Vector("g1", "g2", "r5", "y3", "p3")
		))
		.pipe(takeTurn("Alice clues 1 to Bob"))
		.pipe(takeTurn("Bob clues 2 to Cathy"))

		// Cathy's slot 1 should be finessed.
		assertEquals(game.meta(game.state.hands(Cathy.ordinal)(0)).status, CardStatus.Finessed)
	}

	test("interprets a self-finesse when other possibilities are impossible") {
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r3", "b4", "g4", "p1"),
			Vector("b2", "b3", "p1", "g3"),
			Vector("b5", "b2", "r4", "p5")
		),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues purple to Alice (slot 2)"))	// p1 play
		.pipe(takeTurn("Alice plays p1 (slot 2)"))
		.pipe(takeTurn("Bob clues blue to Cathy"))					// b1 reverse on us
		.pipe(takeTurn("Cathy clues 5 to Donald"))

		.pipe(takeTurn("Donald clues 4 to Bob"))					// connect b4
		.pipe(takeTurn("Alice plays b1 (slot 1)"))					// b1, p1 now played
		.pipe(takeTurn("Bob clues 2 to Alice (slot 3)"))			// neg purple, b2 is clued

		// Alice's slot 1 should be finessed.
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(0)).status, CardStatus.Finessed)
		hasInfs(game, None, Alice, 3, Vector("r2", "y2", "g2"))
	}

	test("doesn't give a self-finesse that looks like a prompt") {
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "b4", "g4", "p1", "b2"),
			Vector("y2", "b2", "r5", "y3", "y4")
		),
			playStacks = Some(Vector(1, 1, 0, 0, 0)),
			init = preClue(Cathy, 3, Vector(TestClue(ClueKind.Colour, Colour.Red.ordinal, Bob)))
		)
		.pipe(takeTurn("Alice clues 3 to Cathy"))

		// This clue is illegal, since r5 will prompt as r2.
		assertEquals(game.lastMove, Some(ClueInterp.Mistake))
	}

	test("gives a self-finesse that doesn't look like a prompt") {
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "b3", "g1", "p1", "y2"),
			Vector("g2", "b3", "p1", "g3", "b2")
		),
			playStacks = Some(Vector(2, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues 3 to Bob"))
		.pipe(takeTurn("Bob plays r3", "g3"))
		.pipe(takeTurn("Cathy clues 5 to Alice (slot 5)"))

		.pipe(takeTurn("Alice clues 3 to Bob"))

		// g1 self-finesse, g2 finesse on Cathy
		assertEquals(game.meta(game.state.hands(Bob.ordinal)(2)).status, CardStatus.Finessed)
		assertEquals(game.meta(game.state.hands(Cathy.ordinal)(0)).status, CardStatus.Finessed)
	}

	test("doesn't give a self-finesse that isn't the simplest interpretation") {
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g1", "p3", "r3", "g3"),
			Vector("p3", "g5", "p4", "g2"),
			Vector("b1", "b2", "p1", "g4")
		),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues 2 to Cathy"))
		.pipe(takeTurn("Alice clues 3 to Bob"))

		// This clue is illegal, since the focus will look like b3.
		assertEquals(game.lastMove, Some(ClueInterp.Mistake))
	}

	test("maintains a self-finesse even as inferences are reduced") {
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y1", "b4", "b1", "g1"),
			Vector("r1", "r3", "r1", "b4"),
			Vector("y1", "r4", "p3", "g1")
		))
		.pipe(takeTurn("Alice clues 1 to Bob"))
		.pipe(takeTurn("Bob plays y1", "p4"))
		.pipe(takeTurn("Cathy clues 3 to Alice (slot 2)"))
		.tap { g =>
			// All of these are valid self-finesses.
			hasInfs(g, None, Alice, 1, Vector("r1", "y2", "g2", "b2", "p1"))
			assertEquals(g.meta(g.state.hands(Alice.ordinal)(0)).status, CardStatus.Finessed)
		}
		.pipe(takeTurn("Donald clues green to Alice (slot 4)"))

		// After knowing we have g2 in slot 4, the finesse should still be on.
		hasInfs(game, None, Alice, 4, Vector("g2"))
		hasInfs(game, None, Alice, 1, Vector("r1", "y2", "b2", "p1"))
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(0)).status, CardStatus.Finessed)
	}

	// test("prefers the simplest connection even when needing to self-finesse") {
	// 	val game = setup(HGroup.atLevel(2), Vector(
	// 		Vector("xx", "xx", "xx", "xx"),
	// 		Vector("b1", "r3", "y3", "g4"),
	// 		Vector("p3", "g5", "p4", "g3"),
	// 		Vector("p1", "b4", "g1", "b2")
	// 	),
	// 		starting = Cathy,
	// 		playStacks = Some(Vector(0, 2, 1, 0, 1)),
	// 		init =
	// 			preClue[HGroup](Bob, 3, Vector(TestClue(ClueKind.Colour, Colour.Yellow.ordinal, Cathy))) andThen
	// 			preClue(Donald, 4, Vector(TestClue(ClueKind.Rank, 2, Cathy))) andThen
	// 			(_.copy(level = 2))
	// 	)
	// 	.pipe(takeTurn("Cathy clues 4 to Alice (slot 1)"))
	// }

	test("understands an asymmetric self-finesse") {
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r3", "r5", "y3", "g4"),
			Vector("r4", "b5", "p4", "g3"),
			Vector("p3", "b4", "g3", "b3")
		),
			starting = Donald,
			playStacks = Some(Vector(2, 0, 0, 4, 0)),
			clueTokens = 7,
			init = _.copy(inEarlyGame = false)
		)
		.pipe(takeTurn("Donald clues 5 to Bob"))

		// Donald, Bob and Alice can see b5, so we all know this is an r5 finesse.
		// hasInfs(game, None, Bob, 2, Vector("r5"))
		assertEquals(game.meta(game.state.hands(Bob.ordinal)(0)).status, CardStatus.Finessed)
	}

	test("plays even if it could be an asymmetric finesse") {
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r3", "r5", "y3", "g4"),
			Vector("r4", "y5", "p4", "g3"),		// Cathy has y5 instead of b5
			Vector("p3", "b4", "g3", "b3")
		),
			starting = Donald,
			playStacks = Some(Vector(2, 0, 0, 4, 0)),
			clueTokens = 7,
			init = _.copy(inEarlyGame = false)
		)
		.pipe(takeTurn("Donald clues 5 to Bob"))

		// We must play into this, since we can't assume we have b5.
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(0)).status, CardStatus.Finessed)
	}

	test("prefers to self-finesse over assuming asymmetric information") {
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "r1", "y2", "r2", "b4"),
			Vector("r3", "g3", "b2", "b3", "m4")
		),
			playStacks = Some(Vector(0, 0, 2, 0, 3)),
			discarded = Vector("r3", "m4"),
			variant = "Rainbow (5 Suits)"
		)
		.pipe(takeTurn("Alice clues red to Cathy"))			// r1, r3, m4
		.pipe(takeTurn("Bob clues 5 to Alice (slot 5)"))
		.pipe(takeTurn("Cathy clues 5 to Alice (slot 5)"))

		// Alice's slot 1 should be finessed as g3.
		hasInfs(game, None, Alice, 1, Vector("y1", "g3", "b1"))
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(0)).status, CardStatus.Finessed)
	}

	test("realizes a self-finesse after other possibilities are stomped") {
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y4", "r5", "y3", "g4"),
			Vector("r2", "g4", "p1", "g1"),
			Vector("r3", "b4", "g1", "b2")
		),
			starting = Donald,
			playStacks = Some(Vector(1, 1, 0, 0, 0)),
			init =
				preClue[HGroup](Cathy, 3, Vector(TestClue(ClueKind.Rank, 1, Bob))) andThen
				preClue[HGroup](Cathy, 4, Vector(TestClue(ClueKind.Rank, 1, Bob)))
		)
		.pipe(takeTurn("Donald clues 4 to Alice (slot 3)"))
		.pipe(takeTurn("Alice discards y3 (slot 4)"))
		.pipe(takeTurn("Bob clues red to Donald"))				// finessing r2, proving !r

		hasInfs(game, None, Alice, 4, Vector("y4", "g4", "p4"))
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(2)).status, CardStatus.Finessed)
	}

	test("understands a very delayed finesse") {
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("p4", "y4", "g1", "r4"),
			Vector("b1", "g1", "y2", "y2"),
			Vector("b3", "y1", "p1", "p3")
		))
		.pipe(takeTurn("Alice clues 1 to Cathy"))
		.pipe(takeTurn("Bob clues 3 to Alice (slot 4)"))
		.pipe(takeTurn("Cathy plays g1", "y5"))
		.pipe(takeTurn("Donald clues yellow to Bob"))

		.pipe(takeTurn("Alice clues 5 to Cathy"))			// 5 Stall
		.pipe(takeTurn("Bob clues red to Alice (slot 3)"))
		.tap {
			hasInfs(_, None, Alice, 3, Vector("r1"))
		}
		.pipe(takeTurn("Cathy plays b1", "g4"))
		.pipe(takeTurn("Donald clues 5 to Alice (slot 2)"))

		.pipe(takeTurn("Alice plays y1 (slot 1)"))

		// The finesse is revealed to be yellow.
		hasInfs(game, None, Alice, 4, Vector("y3"))
	}

	test("understands a fake self-finesse") {
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y2", "y5", "b4", "y3", "r1"),
			Vector("r4", "g5", "p4", "g3", "b5")
		),
			starting = Cathy,
			playStacks = Some(Vector(0, 1, 1, 0, 0))
		)
		.pipe(takeTurn("Cathy clues 1 to Bob"))
		.pipe(takeTurn("Alice clues 3 to Bob"))
		.pipe(takeTurn("Bob plays r1", "b3"))

		// Bob needs to play r1 first to respect r3.
		assertEquals(game.meta(game.state.hands(Bob.ordinal)(1)).status, CardStatus.Finessed)
		hasInfs(game, None, Bob, 5, Vector("r3", "y3", "g3"))
	}
