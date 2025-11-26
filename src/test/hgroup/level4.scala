package tests.hgroup.level4

import scala_bot.basics._
import scala_bot.test.{hasInfs, Player, setup, takeTurn}, Player._
import scala_bot.hgroup.HGroup
import scala_bot.logger.{Logger, LogLevel}

import scala.util.chaining.scalaUtilChainingOps

class TrashCM extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("interprets a rank tcm for 1 card") {
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "b1", "r1", "b4")
		),
			playStacks = Some(Vector(2, 2, 2, 2, 2))
		)
		.pipe(takeTurn("Alice clues 1 to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Discard))
		assertEquals(game.meta(game.state.hands(Bob.ordinal)(4)).status, CardStatus.ChopMoved)
	}

	test("doesn't tcm if chop is trash") {
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "b1", "b4", "g2")
		),
			playStacks = Some(Vector(2, 2, 2, 2, 2))
		)
		.pipe(takeTurn("Alice clues 1 to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))
	}

	test("doesn't tcm if chop is duplicated") {
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "b1", "g4", "g4")
		),
			playStacks = Some(Vector(2, 2, 2, 2, 2))
		)
		.pipe(takeTurn("Alice clues 1 to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))
	}

	test("doesn't tcm if chop can be clued directly (critical)") {
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "b1", "r1", "g5")
		),
			playStacks = Some(Vector(2, 2, 2, 2, 2))
		)
		.pipe(takeTurn("Alice clues 1 to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))
	}

	test("doesn't tcm if chop can be clued directly (playable)") {
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "b1", "r1", "g3")
		),
			playStacks = Some(Vector(2, 2, 2, 2, 2))
		)
		.pipe(takeTurn("Alice clues 1 to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))
	}

	test("recognizes a delayed tcm on other") {
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "y4", "y1", "r3", "y3"),
			Vector("b4", "b4", "g4", "r1", "b3")
		),
			starting = Cathy,
			playStacks = Some(Vector(3, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Cathy clues red to Alice (slots 2,3)"))
		.pipe(takeTurn("Alice plays r4 (slot 2)"))
		.pipe(takeTurn("Bob clues red to Cathy"))

		assertEquals(game.meta(game.state.hands(Cathy.ordinal)(4)).status, CardStatus.ChopMoved)
	}

	test("recognizes a delayed tcm on self") {
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b4", "g4", "r5", "r4"),
			Vector("g3", "y4", "y1", "y5", "b3"),
		),
			playStacks = Some(Vector(3, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues red to Bob"))
		.pipe(takeTurn("Bob plays r4", "g1"))
		.pipe(takeTurn("Cathy clues red to Alice (slot 4)"))

		assertEquals(game.meta(game.state.hands(Alice.ordinal)(4)).status, CardStatus.ChopMoved)
		assert(game.meta(game.state.hands(Alice.ordinal)(3)).trash)
		assertEquals(game.common.thinksPlayables(game, Alice.ordinal).length, 0)
	}

class CM5 extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("doesn't interpret a false 5cm") {
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "g4", "r3", "r5")
		),
			starting = Bob,
			clueTokens = 5,
			init = _.copy(inEarlyGame = false)
		)
		.pipe(takeTurn("Bob clues 5 to Alice (slots 3,5)"))

		// Slot 4 should not be chop moved.
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(3)).status, CardStatus.None)
	}

class OrderCM extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("will not ocm trash") {
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "g4", "g4", "r1")
		),
			starting = Bob,
			playStacks = Some(Vector(2, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slots 3,4)"))
		.pipe(takeTurn("Alice plays b1 (slot 3)"))

		assertEquals(game.lastMove, Some(PlayInterp.Mistake))
		assertEquals(game.meta(game.state.hands(Bob.ordinal)(4)).status, CardStatus.ChopMoved)
	}

	test("interprets an ocm skipping a player") {
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b1", "g1", "r1", "r5"),
			Vector("y4", "r4", "g4", "r4", "b5")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 5 to Alice (slot 5)"))
		.pipe(takeTurn("Alice clues 1 to Bob"))
		.pipe(takeTurn("Bob plays b1", "r1"))

		// Alice's slot 4 should be chop moved.
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(3)).status, CardStatus.ChopMoved)
	}

	test("interprets an ocm that bombs") {
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b1", "g1", "r1", "r5"),
			Vector("y4", "r4", "g4", "r4", "b5")
		),
			playStacks = Some(Vector(0, 0, 2, 0, 0))
		)
		.pipe(takeTurn("Alice clues 1 to Bob"))
		.pipe(takeTurn("Bob bombs g1", "r1"))

		// Cathy's slot 5 should be chop moved.
		assertEquals(game.meta(game.state.hands(Cathy.ordinal)(4)).status, CardStatus.ChopMoved)
	}

	test("interprets new focus correctly") {
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b3", "g3", "r3", "r5")
		),
			starting = Bob,
			init = (game) =>
				game.withMeta(game.state.hands(Alice.ordinal)(3))(_.copy(status = CardStatus.ChopMoved))
					.withMeta(game.state.hands(Alice.ordinal)(4))(_.copy(status = CardStatus.ChopMoved))
		)
		.pipe(takeTurn("Bob clues purple to Alice (slots 2,5)"))

		hasInfs(game, None, Alice, 2, Vector("p1"))
	}

	test("interprets focus touching only cm correctly") {
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b3", "g3", "r3", "r5")
		),
			starting = Bob,
			init = (game) =>
				game.withMeta(game.state.hands(Alice.ordinal)(3))(_.copy(status = CardStatus.ChopMoved))
					.withMeta(game.state.hands(Alice.ordinal)(4))(_.copy(status = CardStatus.ChopMoved))
		)
		.pipe(takeTurn("Bob clues purple to Alice (slots 4,5)"))

		hasInfs(game, None, Alice, 4, Vector("p1"))
	}

	test("interprets focus touching only cm and clued correctly") {
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b3", "g3", "r3", "r5")
		),
			starting = Bob,
			init = (game) =>
				game.withMeta(game.state.hands(Alice.ordinal)(3))(_.copy(status = CardStatus.ChopMoved))
					.withMeta(game.state.hands(Alice.ordinal)(4))(_.copy(status = CardStatus.ChopMoved))
		)
		.pipe(takeTurn("Bob clues purple to Alice (slots 2,3,4,5)"))
		.pipe(takeTurn("Alice plays p1 (slot 3)"))
		.pipe(takeTurn("Bob clues purple to Alice (slots 3,4,5)"))

		hasInfs(game, None, Alice, 3, Vector("p2"))
	}

	test("prioritizes new cards over gt-eliminated chop moved cards") {
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b1", "g1", "r5", "r2"),
			Vector("y2", "b2", "p3", "y1", "r4")
		),
			playStacks = Some(Vector(1, 5, 5, 5, 5)),
			discarded = Vector("r3", "r4"),
			clueTokens = 4,
		)
		.pipe(takeTurn("Alice clues 5 to Bob"))			// known r5 stall
		.pipe(takeTurn("Bob clues 4 to Cathy"))			// r4 save
		.pipe(takeTurn("Cathy clues 1 to Alice (slot 4)"))	// TCM, saving slot 5 (r3)

		.pipe(takeTurn("Alice discards b1 (slot 4)"))	// draws r2 in slot 1
		.pipe(takeTurn("Bob clues red to Cathy"))		// reverse finesse on r4
		.tap { g =>
			hasInfs(g, None, Alice, 1, Vector("r2"))
		}

		.pipe(takeTurn("Cathy discards y1", "r1"))
		.tap { g =>
			assertEquals(g.takeAction, PerformAction.Play(g.state.hands(Alice.ordinal)(0)))
		}

		.pipe(takeTurn("Alice plays r2 (slot 1)"))
		.pipe(takeTurn("Bob discards r2", "p2"))
		.pipe(takeTurn("Cathy discards p3", "y4"))

		assertEquals(game.takeAction, PerformAction.Play(game.state.hands(Alice.ordinal)(1)))
	}

	test("symmetrically elminates against cm cards") {
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b1", "g1", "r3", "r2"),
			Vector("p4", "b2", "y1", "p3", "y4")
		),
			starting = Bob,
			playStacks = Some(Vector(4, 5, 5, 5, 5)),
			clueTokens = 2
		)
		.pipe(takeTurn("Bob clues 1 to Cathy"))			// cm trash
		.pipe(takeTurn("Cathy clues red to Alice (slots 2,3)"))

		hasInfs(game, None, Alice, 2, Vector("r5"))

		assertEquals(game.takeAction, PerformAction.Play(game.state.hands(Alice.ordinal)(1)))
	}
