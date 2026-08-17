package tests.hgroup.level12

import cats.effect.unsafe.implicits.global

import scala_bot.basics._
import scala_bot.test.{hasInfs, hasStatus, Player, setup, takeTurn, TestVariant}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger, LogLevel}

class FocusInversion extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands focus inversion"):
		val game = setup(HGroup.atLevel(12), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y3", "g3", "b3", "p3"),
			Vector("r4", "y4", "g4", "b4", "p4")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 1 to Alice (slot 5)"))
		.pipe(takeTurn("Alice plays r1 (slot 5)"))
		.pipe(takeTurn("Bob clues yellow to Alice (slots 1,5)"))

		hasInfs(game, None, Alice, 1, Vector("y1"))
		hasInfs(game, None, Alice, 5, Vector("y2", "y3", "y4", "y5"))

class Stale1s extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("interprets a TCM from a stale 1"):
		val game = setup(HGroup.atLevel(12), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y3", "g3", "b3", "p3"),
			Vector("r4", "y4", "g4", "b4", "p4")
		),
			starting = Bob,
			playStacks = Some(Vector(1, 1, 1, 0, 0)),
			clueTokens = 4
		)
		.pipe(takeTurn("Bob discards p3", "r5"))
		.pipe(takeTurn("Cathy clues 1 to Alice (slot 4)"))

		// This is a Trash Chop Move.
		assert(game.meta(game.state.hands(Alice.ordinal)(3)).trash)
		assert(game.common.thinksPlayables(game, Alice.ordinal).isEmpty)
		assertEquals(game.common.thinksTrash(game, Alice.ordinal), Vector(game.state.hands(Alice.ordinal)(3)))
		hasStatus(game, Alice, 5, CardStatus.ChopMoved)

	test("doesn't randomly skip a 1"):
		val game = setup(HGroup.atLevel(12), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y3", "g3", "b3", "p3"),
			Vector("r4", "y4", "g4", "b4", "p4")
		),
			starting = Cathy,
			playStacks = Some(Vector(1, 1, 1, 0, 0)),
			clueTokens = 4
		)
		.pipe(takeTurn("Cathy clues 1 to Alice (slots 3,4)"))

		assertEquals(game.common.thinksPlayables(game, Alice.ordinal), Vector(game.state.hands(Alice.ordinal)(2), game.state.hands(Alice.ordinal)(3)))
		assertEquals(game.common.thinksTrash(game, Alice.ordinal), Vector.empty)

	test("skips a stale 1"):
		val game = setup(HGroup.atLevel(12), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y3", "g3", "b3", "p3"),
			Vector("r4", "y4", "g4", "b4", "p4")
		),
			starting = Bob,
			playStacks = Some(Vector(1, 1, 1, 0, 0)),
			clueTokens = 4
		)
		.pipe(takeTurn("Bob discards p3", "r5"))
		.pipe(takeTurn("Cathy clues 1 to Alice (slots 2,3,4)"))
		.tap: g =>
			// This focuses slot 3, and slot 4 is kt.
			assert(g.meta(g.state.hands(Alice.ordinal)(3)).trash)
			assertEquals(g.common.thinksPlayables(g, Alice.ordinal), Vector(g.state.hands(Alice.ordinal)(1), g.state.hands(Alice.ordinal)(2)))
			assertEquals(g.common.thinksTrash(g, Alice.ordinal), Vector(g.state.hands(Alice.ordinal)(3)))
			hasInfs(g, None, Alice, 2, Vector("b1", "p1"))
		.pipe(takeTurn("Alice plays b1 (slot 3)"))

		// This is not an OCM on Bob.
		hasStatus(game, Bob, 5, CardStatus.PermissionToDiscard)
		assertEquals(game.common.thinksPlayables(game, Alice.ordinal), Vector(game.state.hands(Alice.ordinal)(2)))
		assertEquals(game.common.thinksTrash(game, Alice.ordinal), Vector(game.state.hands(Alice.ordinal)(3)))

	test("gives a stale 1's chop move"):
		val game = setup(HGroup.atLevel(12), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y3", "g3", "r1", "p3"),
			Vector("r4", "y4", "g4", "b4", "p4")
		),
			starting = Cathy,
			playStacks = Some(Vector(1, 1, 1, 0, 0)),
			clueTokens = 4
		)
		.pipe(takeTurn("Cathy discards p4", "b4"))
		.tap: g =>
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Rank(Bob.ordinal, 1))
		.pipe(takeTurn("Alice clues 1 to Bob"))

		hasStatus(game, Bob, 5, CardStatus.ChopMoved)
		assert(game.meta(game.state.hands(Bob.ordinal)(3)).trash)

	test("understands stale 1's in pink"):
		val game = setup(HGroup.atLevel(12), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y3", "g3", "b3", "i3"),
			Vector("r4", "y4", "g4", "b4", "i4")
		),
			starting = Bob,
			playStacks = Some(Vector(1, 1, 1, 0, 0)),
			clueTokens = 4,
			variant = TestVariant.Pink5
		)
		.pipe(takeTurn("Bob discards i3", "r5"))
		.pipe(takeTurn("Cathy clues 1 to Alice (slots 2,3,4)"))
		.tap: g =>
			// This focuses slot 3, and slot 4 is some pink.
			assertEquals(g.common.thinksPlayables(g, Alice.ordinal), Vector(g.state.hands(Alice.ordinal)(2)))
			assertEquals(g.common.thinksTrash(g, Alice.ordinal), Vector.empty)
		.pipe(takeTurn("Alice plays b1 (slot 3)"))

		// This is not an OCM on Bob.
		hasStatus(game, Bob, 5, CardStatus.PermissionToDiscard)
		assertEquals(game.common.thinksPlayables(game, Alice.ordinal), Vector(game.state.hands(Alice.ordinal)(2)))
		assertEquals(game.common.thinksTrash(game, Alice.ordinal), Vector.empty)
