package tests.hgroup.level5

import scala_bot.basics._
import scala_bot.test.{fullyKnown, hasInfs, Player, preClue, setup, takeTurn, TestClue}, Player._
import scala_bot.hgroup.HGroup
import scala_bot.logger.{Logger, LogLevel}

import scala.util.chaining.scalaUtilChainingOps

class HiddenFinesses extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands a hidden finesse (rank)"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "g4", "r5", "b4"),
			Vector("g2", "b3", "p3", "y3", "r2")
		),
			starting = Bob,
			playStacks = Some(Vector(1, 0, 1, 1, 0)),
			clueTokens = 4,
			init = preClue(Cathy, 5, Vector(TestClue(ClueKind.Rank, 2, Bob)))
		)
		.pipe(takeTurn("Bob clues 3 to Alice (slot 3)"))
		.tap: g =>
			hasInfs(g, None, Alice, 3, Vector("r3", "g3"))

		.pipe(takeTurn("Cathy plays r2", "r1"))
		.tap: g =>
			// Alice's slot 3 should still be [r3,g3] to allow for a hidden finesse.
			hasInfs(g, None, Alice, 3, Vector("r3", "g3"))

		.pipe(takeTurn("Alice discards b1 (slot 5)"))
		.pipe(takeTurn("Bob discards b4", "r1"))
		.pipe(takeTurn("Cathy discards p3", "y1"))

		// Alice's slot 4 (used to be 3) should just be r3 now.
		hasInfs(game, None, Alice, 4, Vector("r3"))
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(0)).status, CardStatus.None)

	test("understands a complicated fake hidden finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p1", "p4", "r3", "y3", "r5"),
			Vector("g1", "y2", "g2", "b1", "g3")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			init = preClue(Cathy, 4, Vector(TestClue(ClueKind.Rank, 1, Bob)))
		)
		.pipe(takeTurn("Alice clues purple to Bob"))
		.pipe(takeTurn("Bob clues 2 to Alice (slot 1)"))
		.tap: g =>
			hasInfs(g, None, Alice, 1, Vector("r2", "g2", "b2", "p2"))

		.pipe(takeTurn("Cathy plays b1", "y5"))
		.pipe(takeTurn("Alice clues 5 to Bob"))
		.pipe(takeTurn("Bob clues 2 to Cathy"))		// y2 reverse finesse
		.tap: g =>
			hasInfs(g, None, Alice, 2, Vector("y1"))

		.pipe(takeTurn("Cathy discards g3", "p3"))

		// Cathy didn't play into the green finesse, so we have [r2,b2,p2] in slot 1.
		hasInfs(game, None, Alice, 1, Vector("r2", "b2", "p2"))

	test("plays into a hidden finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g2", "r2", "r3", "p1", "b4"),
			Vector("p2", "g4", "y2", "b4", "p5")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 1 to Alice (slots 2,3)"))
		.pipe(takeTurn("Alice plays y1 (slot 3)"))
		.pipe(takeTurn("Bob clues 5 to Cathy"))

		.pipe(takeTurn("Cathy clues red to Bob"))	// r2 hidden finesse
		.pipe(takeTurn("Alice plays b1 (slot 3)"))	// expecting r1 playable

		// Our slot 2 (previously slot 1) should be r1.
		hasInfs(game, None, Alice, 2, Vector("r1"))

	test("generates focus possibilities for a potential hidden finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g1", "y4", "b1", "r5", "g4"),
			Vector("y3", "g2", "b3", "p5", "p4")
		),
			starting = Cathy,
			init = preClue[HGroup](Bob, 3, Vector(TestClue(ClueKind.Rank, 1, Alice))) andThen
				fullyKnown(Cathy, 2, "g2")
		)
		.pipe(takeTurn("Cathy clues green to Alice (slot 2)"))

		hasInfs(game, None, Alice, 2, Vector("g1", "g3"))

	test("realizes a hidden/layered finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r2", "r2", "y4", "g2"),
			Vector("y4", "g4", "p3", "y3"),
			Vector("p3", "g4", "g2", "b2")
		),
			starting = Bob,
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			init = preClue(Alice, 3, Vector(TestClue(ClueKind.Rank, 1, Alice)))
		)
		.pipe(takeTurn("Bob clues 3 to Cathy"))			// looks like y1 (playable) -> y2 finesse on Alice
		.pipe(takeTurn("Cathy discards g4", "p4"))
		.pipe(takeTurn("Donald clues green to Bob"))	// we need to play g1, but "y2" is in slot 1.
		.tap: g =>
			// We are promised y1, y2 and g1
			// hasInfs(g, None, Alice, 3, Vector("y1", "g1"))
			assertEquals(g.meta(g.state.hands(Alice.ordinal)(0)).status, CardStatus.Finessed)
			// assertEquals(g.meta(g.state.hands(Alice.ordinal)(1)).status, CardStatus.Finessed)

			// Slot 2 is not playable.
			assert(!g.common.thinksPlayables(g, Alice.ordinal).contains(g.state.hands(Alice.ordinal)(1)))
		.pipe(takeTurn("Alice plays y1 (slot 3)"))

		// Alice's slot 2 (previously slot 1) should still be finessed.
		// hasInfs(game, None, Alice, 2, Vector("g1", "g3"))
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(1)).status, CardStatus.Finessed)
		// assertEquals(game.meta(game.state.hands(Alice.ordinal)(2)).status, CardStatus.Finessed)

	test("realizes a layered finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r2", "r2", "y4", "g2"),
			Vector("p4", "g4", "p3", "y4"),
			Vector("p3", "g4", "g2", "b2")
		),
			starting = Donald,
			playStacks = Some(Vector(0, 2, 0, 0, 0)),
			init = preClue(Alice, 3, Vector(TestClue(ClueKind.Rank, 1, Alice)))
		)
		.pipe(takeTurn("Donald clues 1 to Alice (slot 4)"))
		.pipe(takeTurn("Alice plays r1 (slot 4)"))
		.pipe(takeTurn("Bob clues yellow to Cathy"))	// Looks like y3 finesse from us
		.pipe(takeTurn("Cathy clues green to Bob"))		// we need to play g1, but slot 1 is [y3] and slot 2 is neg 1.
		.tap: g =>
			// hasInfs(g, None, Alice, 3, Vector("y1", "g1"))
			assertEquals(g.meta(g.state.hands(Alice.ordinal)(0)).status, CardStatus.Finessed)
			// assertEquals(g.meta(g.state.hands(Alice.ordinal)(1)).status, CardStatus.Finessed)

			// Slot 2 is not playable.
			assert(!g.common.thinksPlayables(g, Alice.ordinal).contains(g.state.hands(Alice.ordinal)(1)))

		.pipe(takeTurn("Donald discards b2", "b5"))
		.pipe(takeTurn("Alice plays g1 (slot 1)"))

		// Alice's slot 2 should still be finessed.
		hasInfs(game, None, Alice, 2, Vector("y3"))
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(1)).status, CardStatus.Finessed)

	test("correctly resolves an ambiguous hidden finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "y1", "g1", "g3"),
			Vector("b3", "y1", "g1", "b1"),
			Vector("p1", "p1", "r2", "y2")
		),
			starting = Cathy,
			playStacks = Some(Vector(5, 5, 1, 1, 5)),
			init = preClue(Alice, 2, Vector(TestClue(ClueKind.Rank, 2, Bob)))
		)
		.pipe(takeTurn("Cathy clues 3 to Bob"))			// delayed play or hidden finesse
		.pipe(takeTurn("Donald clues 4 to Alice (slot 4)"))
		// .tap { g =>
		// 	// Can connect as b4 (if g2 hidden, then Cathy finesse) or g4
		// 	hasInfs(g, None, Alice, 4, Vector("g4", "b4"))
		// }
		.pipe(takeTurn("Alice plays b2 (slot 2)"))		// revealing hidden finesse
		.tap: g =>
			// Slot 2 (previously slot 1) should be g2
			hasInfs(g, None, Alice, 2, Vector("g2"))
		.pipe(takeTurn("Bob discards g1", "p2"))
		.pipe(takeTurn("Cathy plays b3", "r3"))			// proving b4

		hasInfs(game, None, Alice, 4, Vector("b4"))
