package tests.hgroup.level5

import scala_bot.basics._
import scala_bot.test.{hasInfs, Player, setup, takeTurn}, Player._
import scala_bot.hgroup.HGroup

import scala.util.chaining.scalaUtilChainingOps

class QueuedFinesses extends munit.FunSuite:
	// override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands a queued finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r2", "g4", "r5", "b4"),
			Vector("g2", "b3", "r2", "y3", "p3")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues green to Cathy"))
		.tap: g =>
			hasInfs(g, None, Alice, 1, Vector("g1"))
			assertEquals(g.meta(g.state.hands(Alice.ordinal)(0)).status, CardStatus.Finessed)
		.pipe(takeTurn("Cathy clues 2 to Bob"))
		.tap: g =>
			// Only Alice's slot 1 is playable.
			assertEquals(g.common.thinksPlayables(g, Alice.ordinal), List(g.state.hands(Alice.ordinal)(0)))
		.pipe(takeTurn("Alice plays g1 (slot 1)"))

		// Alice's slot 2 should be r1.
		hasInfs(game, None, Alice, 2, Vector("r1"))
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(1)).status, CardStatus.Finessed)

	test("understands a delayed queued finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "g3", "g4", "r5", "b4"),
			Vector("r3", "b3", "r4", "y3", "p3")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues green to Bob"))		// g1, g2 on us
		.pipe(takeTurn("Alice plays g1 (slot 1)"))
		.pipe(takeTurn("Bob clues red to Cathy"))

		// Only Alice's slot 2 is playable.
		assertEquals(game.common.thinksPlayables(game, Alice.ordinal), List(game.state.hands(Alice.ordinal)(1)))

	test("waits for a queued finesse to resolve"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g2", "b3", "r2", "y3", "p3"),
			Vector("g1", "r1", "r4", "g4", "b4")
		))
		.pipe(takeTurn("Alice clues green to Bob"))
		.pipe(takeTurn("Bob clues red to Alice (slot 2)"))		// r2 finesse
		.pipe(takeTurn("Cathy plays g1", "b1"))

		// Alice should wait for Cathy.
		hasInfs(game, None, Alice, 2, Vector("r1", "r2"))

	test("plays queued finesses in the right order"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r2", "g4", "r5", "b4"),
			Vector("g2", "b3", "r2", "y3", "p3")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 2 to Bob"))
		.pipe(takeTurn("Alice plays b1 (slot 1)"))		// expecting r1 finesse
		.pipe(takeTurn("Bob clues green to Cathy"))		// g2 reverse finesse

		.pipe(takeTurn("Cathy discards p3", "y1"))

		// Only Alice's slot 2 is playable.
		assertEquals(game.common.thinksPlayables(game, Alice.ordinal), List(game.state.hands(Alice.ordinal)(1)))

	test("waits for an older unplayable finesse before playing into a new one"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g2", "b3", "g4", "p5"),
			Vector("r1", "r2", "r3", "y3"),
			Vector("b4", "r5", "p3", "y2")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Donald"))		// r5 finesse, Alice has r4
		.pipe(takeTurn("Cathy plays r1", "b1"))
		.pipe(takeTurn("Donald clues 5 to Alice (slot 4)"))
		.pipe(takeTurn("Alice discards y4 (slot 3)"))	// r4 now in slot 2

		.pipe(takeTurn("Bob clues yellow to Donald"))	// y2 finesse, Alice has y1

		// Alice cannot play y1 in slot 1, because y1 could be layered in the r4 finesse.
		assertEquals(game.common.thinksPlayables(game, Alice.ordinal), List.empty)

	test("doesn't wait for an older finesse when it can't be layered"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g2", "b3", "g4", "p4"),
			Vector("r1", "r2", "r3", "y3"),
			Vector("b4", "r5", "p3", "y2")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Donald"))		// r5 finesse, Alice has r4
		.pipe(takeTurn("Cathy plays r1", "b1"))
		.pipe(takeTurn("Donald clues 5 to Alice (slots 3,4)"))
		.pipe(takeTurn("Alice discards y4 (slot 2)"))	// r4 now in slot 2

		.pipe(takeTurn("Bob clues yellow to Donald"))	// y2 finesse, Alice has y1

		// Alice can play y1 in slot 1, because the queued r4 can't be layered.
		assertEquals(game.common.thinksPlayables(game, Alice.ordinal), List(game.state.hands(Alice.ordinal)(0)))
		hasInfs(game, None, Alice, 1, Vector("y1"))

	test("orders complex finesses correctly"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b5", "r4", "y2", "p4"),
			Vector("p2", "r1", "b3", "g4"),
			Vector("r3", "b1", "r4", "b1")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 2 to Alice (slot 1)"))	// g2 self-finesse
		.pipe(takeTurn("Cathy clues 2 to Bob"))				// + y1 to Alice's finesses
		.pipe(takeTurn("Donald clues green to Cathy"))		// + g3 to Alice's finesses
		.pipe(takeTurn("Alice plays g1 (slot 2)"))
		.tap: g =>
			assertEquals(g.meta(g.state.hands(Alice.ordinal)(2)).status, CardStatus.Finessed)
			// assertEquals(g.meta(g.state.hands(Alice.ordinal)(3)).status, CardStatus.Finessed)
		.pipe(takeTurn("Bob clues red to Cathy"))
		.pipe(takeTurn("Cathy plays r1", "g1"))
		.pipe(takeTurn("Donald clues 5 to Bob"))
		.tap: g =>
			// Alice should play y1 in slot 3.
			hasInfs(g, None, Alice, 3, Vector("y1"))
		.pipe(takeTurn("Alice plays y1 (slot 3)"))

		hasInfs(game, None, Alice, 4, Vector("g3"))
