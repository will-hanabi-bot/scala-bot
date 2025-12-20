package tests.hgroup.level5

import scala_bot.basics._
import scala_bot.test.{hasInfs, Player, preClue, setup, takeTurn, TestClue}, Player._
import scala_bot.hgroup.HGroup
import scala_bot.logger.{Logger, LogLevel}

import scala.util.chaining.scalaUtilChainingOps
import scala_bot.test.fullyKnown

class LayeredFinesses extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands a layered finesse") {
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "g4", "r5", "b4"),
			Vector("g1", "y1", "r2", "y3", "p3")
		),
			starting = Bob,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues yellow to Alice (slot 3)"))
		.tap { g =>
			hasInfs(g, None, Alice, 3, Vector("y1", "y2"))
		}
		.pipe(takeTurn("Cathy plays g1", "b1"))
		.pipe(takeTurn("Alice discards b1 (slot 5)"))
		.pipe(takeTurn("Bob discards b4", "r1"))

		.pipe(takeTurn("Cathy plays y1", "y1"))

		// Alice's slot 4 (used to be 3) should just be y2 now.
		hasInfs(game, None, Alice, 4, Vector("y2"))
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(0)).status, CardStatus.None)
	}

	test("understands a fake asymmetric layered finesse") {
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b2", "r1", "r4", "r3"),
			Vector("r5", "g2", "g3", "g1"),
			Vector("b5", "y4", "r4", "b2")
		),
			starting = Donald,
			playStacks = Some(Vector(0, 0, 0, 1, 0)),
			init = preClue(Cathy, 4, Vector(TestClue(ClueKind.Rank, 1, Bob)))
		)
		.pipe(takeTurn("Donald clues 4 to Alice (slots 2,3)"))
		.tap { g =>
			hasInfs(g, None, Alice, 2, Vector("r4", "g4", "b4"))
		}
		.pipe(takeTurn("Alice clues 2 to Cathy"))
		.pipe(takeTurn("Bob plays b2", "b1"))
		.pipe(takeTurn("Cathy plays g1", "y2"))
		.pipe(takeTurn("Donald clues 3 to Cathy"))

		// We should have b3 in slot 1.
		hasInfs(game, None, Alice, 1, Vector("b3"))
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(0)).status, CardStatus.Finessed)
	}

	test("plays into a layered finesse") {
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b5", "p4", "y2", "g3", "r3"),
			Vector("r4", "r4", "g4", "r5", "b4")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues yellow to Bob"))
		.tap { g =>
			hasInfs(g, None, Alice, 1, Vector("y1"))
			assertEquals(g.meta(g.state.hands(Alice.ordinal)(0)).status, CardStatus.Finessed)
		}
		.pipe(takeTurn("Alice plays g1 (slot 1)"))

		// Alice's slot 2 should be [y1] now.
		hasInfs(game, None, Alice, 2, Vector("y1"))
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(1)).status, CardStatus.Finessed)
	}

	test("plays into a complex layered finesse") {
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b5", "p4", "y2", "g3"),
			Vector("g1", "r4", "g4", "y5"),
			Vector("g2", "r4", "b4", "g1")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 4 to Alice (slot 4)"))	// g4
		.pipe(takeTurn("Cathy plays g1", "p3"))
		.pipe(takeTurn("Donald plays g2", "r5"))
		.pipe(takeTurn("Alice plays p1 (slot 1)"))

		// Alice's slot 2 should be [g3] now.
		hasInfs(game, None, Alice, 2, Vector("g3"))
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(1)).status, CardStatus.Finessed)
	}

	test("understands when it dupes a card in its own layered finesse") {
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r2", "b4", "g2", "r5"),
			Vector("g1", "b2", "y3", "r4"),
			Vector("p1", "r3", "r1", "g3")
		),
			starting = Donald,
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Donald clues 4 to Cathy"))		// r2 (Cathy), r3 (Alice)
		.pipe(takeTurn("Alice clues green to Bob"))		// reverse finesse for g1
		.pipe(takeTurn("Bob plays r2", "b4"))
		.pipe(takeTurn("Cathy plays g1", "g4"))

		.pipe(takeTurn("Donald clues 5 to Alice (slot 4)"))
		.pipe(takeTurn("Alice bombs g1 (slot 1)"))		// trying to play r3

		// Slot 2 should still be finessed as r3.
		hasInfs(game, None, Alice, 2, Vector("r3"))
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(1)).status, CardStatus.Finessed)
	}

	test("doesn't give a layered finesse on the same id twice") {
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y1", "y1", "p1", "r5", "b4"),
			Vector("r2", "y4", "p2", "g3", "r3")
		))
		.pipe(takeTurn("Alice clues purple to Cathy"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))
	}

	test("doesn't give a selfish layered finesse on the same id twice") {
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y2", "y2", "y3", "r5", "b4"),
			Vector("r2", "y4", "p2", "g3", "r3")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues yellow to Alice (slot 2)"))
		.pipe(takeTurn("Alice clues yellow to Cathy"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))
	}

	test("gracefully handles a non-matching clue revealing a layered finesse") {
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "b5", "r2", "y1", "p4"),
			Vector("r4", "g2", "g4", "r5", "b4")
		),
			starting = Cathy,
			discarded = Vector("y4")
		)
		.pipe(takeTurn("Cathy clues red to Bob"))		// r2 layered finesse
		.pipe(takeTurn("Alice plays b1 (slot 1)"))		// expecting r1
		.pipe(takeTurn("Bob clues yellow to Alice (slots 2,5)"))	// y4 save

		.pipe(takeTurn("Cathy discards b4", "b1"))
		.tap { g =>
			// The revealed card should be known y1.
			hasInfs(g, None, Alice, 2, Vector("y1"))
			// Slot 3 should be finessed as the missing r1.
			hasInfs(g, None, Alice, 3, Vector("r1"))
			assertEquals(g.meta(g.state.hands(Alice.ordinal)(2)).status, CardStatus.Finessed)
		}
		.pipe(takeTurn("Alice plays r1 (slot 3)"))

		// y1 inference should remain (now on slot 3).
		hasInfs(game, None, Alice, 3, Vector("y1"))
	}

	test("gracefully handles a matching clue revealing a layered finesse") {
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "b5", "r2", "y1", "p4"),
			Vector("y4", "g2", "g4", "r5", "b4")
		),
			starting = Cathy,
			discarded = Vector("r4")
		)
		.pipe(takeTurn("Cathy clues red to Bob"))		// r2 layered finesse
		.pipe(takeTurn("Alice plays b1 (slot 1)"))		// expecting r1
		.pipe(takeTurn("Bob clues red to Alice (slots 3,5)"))	// r4 save

		.pipe(takeTurn("Cathy discards b4", "b1"))

		// Slot 2 should be finessed as [y1, g1, b2, p1].
		hasInfs(game, None, Alice, 2, Vector("y1", "g1", "b2", "p1"))
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(1)).status, CardStatus.Finessed)

		// Slot 3 should be known as the missing r1.
		hasInfs(game, None, Alice, 3, Vector("r1"))
	}

	test("plays into a layered finesse with self-connecting cards") {
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "b4", "y2", "r5", "r4"),
			Vector("g1", "r1", "b5", "g4", "b4")
		),
			starting = Cathy,
			clueTokens = 7
		)
		.pipe(takeTurn("Cathy clues yellow to Bob"))	// y2 layered finesse
		.pipe(takeTurn("Alice plays p1 (slot 1)"))		// expecting y1
		.pipe(takeTurn("Bob discards r4", "b2"))

		.pipe(takeTurn("Cathy discards b4", "b3"))
		.pipe(takeTurn("Alice plays p2 (slot 2)"))		// expecting y1

		hasInfs(game, None, Alice, 3, Vector("y1"))
	}

	test("doesn't connect on others inside a layered finesse on self") {
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b1", "b4", "y2", "r5"),
			Vector("p2", "y1", "b5", "g3"),
			Vector("g1", "r4", "b4", "g3")
		),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues yellow to Bob"))	// y2 layered finesse
		.pipe(takeTurn("Alice plays p1 (slot 1)"))		// expecting y1

		// We should connect with y1 in slot 2, not using Bob's y1.
		hasInfs(game, None, Alice, 2, Vector("y1"))
	}

	test("doesn't accomodate impossible layers") {
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g1", "g2", "b2", "g4"),
			Vector("g5", "p2", "g1", "p1"),
			Vector("p4", "r4", "y5", "y3")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues blue to Bob"))			// finessing b1
		.pipe(takeTurn("Donald clues 4 to Alice (slot 2)"))

		// Not enough cards to satisfy other possibilities.
		hasInfs(game, None, Alice, 2, Vector("g4", "b4"))
	}

	test("understands when a fake layered finesse is disproved") {
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g3", "g2", "g4", "b4"),
			Vector("g5", "p2", "g2", "b3"),
			Vector("p4", "y3", "y5", "r2")
		),
			playStacks = Some(Vector(0, 0, 0, 1, 0)),
			discarded = Vector("b3", "b4")
		)
		.pipe(takeTurn("Alice clues 3 to Cathy"))			// b3 save
		.pipe(takeTurn("Bob clues red to Donald"))			// r1 reverse finesse
		.pipe(takeTurn("Cathy clues 5 to Donald"))
		.pipe(takeTurn("Donald clues blue to Bob"))			// b4 save (could be b2 play)
		.tap { g =>
			hasInfs(g, None, Bob, 4, Vector("b2", "b4"))
		}
		.pipe(takeTurn("Alice plays r1 (slot 1)"))

		hasInfs(game, None, Bob, 4, Vector("b2", "b4"))
	}

	test("allows layered players to unintentionally dupe") {
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "g2", "g4", "b2", "p4"),
			Vector("b2", "r2", "g2", "b3", "y3")
		),
			starting = Bob,
			playStacks = Some(Vector(0, 0, 0, 1, 0)),
			init = fullyKnown(Alice, 5, "r1")
		)
		.pipe(takeTurn("Bob clues red to Alice (slots 2,5)"))	// r2 direct, or r3 layered
		.pipe(takeTurn("Cathy clues blue to Bob"))				// dupe b2
		.pipe(takeTurn("Alice plays r1 (slot 5)"))

		.pipe(takeTurn("Bob clues 5 to Alice (slot 5)"))
		.pipe(takeTurn("Cathy plays b2", "p2"))

		// r3 layered finesse in slot 3 (previously slot 2) is confirmed, not r2.
		hasInfs(game, None, Alice, 3, Vector("r3"))
	}
