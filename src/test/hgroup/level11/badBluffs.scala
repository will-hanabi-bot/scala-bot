package tests.hgroup.level11

import scala_bot.basics._
import scala_bot.test.{hasInfs, Player, setup, takeTurn, TestVariant}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger, LogLevel}

class BadBluffs extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("doesn't give a self-colour bluff"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "y2", "g2", "p2", "b2"),
			Vector("p4", "r3", "g1", "y4", "y3")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 2 to Bob"))
		.pipe(takeTurn("Alice clues blue to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("doesn't bluff a card that isn't immediately playable"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y1", "b5", "y1", "r1"),
			Vector("y4", "p1", "g3", "g4"),
			Vector("y3", "g1", "g3", "r1")
		),
			starting = Cathy,
			playStacks = Some(Vector(1, 0, 0, 1, 2))
		)
		.pipe(takeTurn("Cathy clues 1 to Donald"))
		.pipe(takeTurn("Donald plays r1", "p1"))

		val bad1 = takeTurn("Alice clues 3 to Donald")(game)
		val bad2 = takeTurn("Alice clues green to Donald")(game)

		assertEquals(bad1.lastMove, Some(ClueInterp.Mistake))
		assertEquals(bad2.lastMove, Some(ClueInterp.Mistake))

	test("doesn't bluff through a self-finesse"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y1", "b5", "y1", "r4", "y4"),
			Vector("p2", "p3", "g3", "g4", "y5")
		))
		.pipe(takeTurn("Alice clues 3 to Cathy"))

		// This is an illegal bluff target.
		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("doesn't bluff when Cathy will connect 1"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y1", "b5", "y1", "r4", "y4"),
			Vector("p2", "r2", "b2", "g4", "y5")
		))
		.pipe(takeTurn("Alice clues 2 to Cathy"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("doesn't bluff when it can't be recognized by all players"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y1", "b5", "y2", "y2", "y4"),
			Vector("p2", "r2", "b2", "g4", "y5")
		))
		.pipe(takeTurn("Alice clues 2 to Cathy"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("doesn't bluff when Cathy will connect 2"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r2", "r1", "p2", "r4"),
			Vector("p3", "p2", "g1", "g4"),
			Vector("r1", "r1", "r4", "g3")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues purple to Bob"))
		.pipe(takeTurn("Donald plays r1 (slot 1)", "r5"))
		.tap: g =>
			hasInfs(g, None, Bob, 3, Vector("p2"))
		.pipe(takeTurn("Alice clues 3 to Cathy"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("doesn't bluff on top of unknown queued cards"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g1", "r2", "y1", "r4"),
			Vector("p2", "b2", "r3", "y4"),
			Vector("g1", "g2", "g3", "p4")
		),
			starting = Donald,
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Donald clues red to Cathy"))

		val bad1 = takeTurn("Alice clues 2 to Cathy")(game)
		assertEquals(bad1.lastMove, Some(ClueInterp.Mistake))

		val bad2 = takeTurn("Alice clues blue to Cathy")(game)
		assertEquals(bad2.lastMove, Some(ClueInterp.Mistake))

		val bad3 = takeTurn("Alice clues purple to Cathy")(game)
		assertEquals(bad3.lastMove, Some(ClueInterp.Mistake))

	test("doesn't bluff a previously-finessed player"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y2", "y1", "g1", "b5", "r4"),
			Vector("g2", "r2", "r3", "y5", "y4")
		),
			starting = Cathy,
			variant = TestVariant.Pink5
		)
		.pipe(takeTurn("Cathy clues 2 to Bob"))		// self-finesse
		.pipe(takeTurn("Alice clues 2 to Cathy"))	// stacking g1 on top
		.pipe(takeTurn("Bob plays y1", "i1"))

		.pipe(takeTurn("Cathy clues 5 to Alice (slot 5)"))

		val bad1 = takeTurn("Alice clues 3 to Cathy")(game)
		assertEquals(bad1.lastMove, Some(ClueInterp.Mistake))

		val bad2 = takeTurn("Alice clues red to Cathy")(game)
		assertEquals(bad2.lastMove, Some(ClueInterp.Mistake))

	test("doesn't bluff a previously-finessed player"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y2", "y1", "g1", "b5", "r4"),
			Vector("g2", "r2", "r3", "y5", "y4")
		),
			starting = Cathy,
			variant = TestVariant.Pink5
		)
		.pipe(takeTurn("Cathy clues 2 to Bob"))		// self-finesse
		.pipe(takeTurn("Alice clues 2 to Cathy"))	// stacking g1 on top
		.pipe(takeTurn("Bob plays y1", "i1"))

		.pipe(takeTurn("Cathy clues 5 to Alice (slot 5)"))

		val bad1 = takeTurn("Alice clues 3 to Cathy")(game)
		assertEquals(bad1.lastMove, Some(ClueInterp.Mistake))

		val bad2 = takeTurn("Alice clues red to Cathy")(game)
		assertEquals(bad2.lastMove, Some(ClueInterp.Mistake))

	test("doesn't bluff on top of a possibly-layered gd"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g1", "y1", "r2", "y1", "r4"),
			Vector("g1", "b2", "g3", "g5", "p4")
		),
			starting = Bob,
			clueTokens = 4
		)
		.pipe(takeTurn("Bob clues green to Cathy"))
		.pipe(takeTurn("Cathy discards g1", "y4"))

		// With g1 already queued, we can't bluff Bob's y1.

		val bad1 = takeTurn("Alice clues 2 to Cathy")(game)
		assertEquals(bad1.lastMove, Some(ClueInterp.Mistake))

		val bad2 = takeTurn("Alice clues blue to Cathy")(game)
		assertEquals(bad2.lastMove, Some(ClueInterp.Mistake))

	test("doesn't bluff when Bob will colour prompt"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "r4", "y1", "y1", "y5"),
			Vector("p4", "b2", "r3", "b5", "y4")
		))
		.pipe(takeTurn("Alice clues red to Bob"))
		.pipe(takeTurn("Bob plays r1", "g1"))
		.pipe(takeTurn("Cathy clues 5 to Bob"))

		val bad1 = takeTurn("Alice clues 3 to Cathy")(game)
		assertEquals(bad1.lastMove, Some(ClueInterp.Mistake))

		val bad2 = takeTurn("Alice clues red to Cathy")(game)
		assertEquals(bad2.lastMove, Some(ClueInterp.Mistake))

	test("doesn't bluff when Bob will rank prompt"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y2", "r2", "y1", "y1", "y5"),
			Vector("g4", "b2", "p3", "b5", "y4")
		),
			playStacks = Some(Vector(0, 1, 0, 0, 1))
		)
		.pipe(takeTurn("Alice clues 2 to Bob"))
		.pipe(takeTurn("Bob plays y2", "g1"))
		.pipe(takeTurn("Cathy clues 5 to Bob"))

		val bad1 = takeTurn("Alice clues 3 to Cathy")(game)
		assertEquals(bad1.lastMove, Some(ClueInterp.Mistake))

		val bad2 = takeTurn("Alice clues purple to Cathy")(game)
		assertEquals(bad2.lastMove, Some(ClueInterp.Mistake))

	test("doesn't give a delayed bluff"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p1", "g3", "p4", "p3", "b3"),
			Vector("g1", "y5", "g4", "r1", "b1")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 1 to Cathy"))
		.pipe(takeTurn("Cathy plays b1", "r4"))
		.pipe(takeTurn("Alice clues 3 to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("doesn't use an illegal bluff target"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b1", "g1", "p2", "y3"),
			Vector("r1", "b1", "r3", "b4"),
			Vector("r1", "p3", "r3", "r2")
		))
		.pipe(takeTurn("Alice clues 3 to Donald"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("doesn't give a self-bluff that looks like a reverse finesse"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y4", "g2", "r3", "y3", "p3"),
			Vector("r3", "r4", "y5", "p4", "r2"),
		),
			playStacks = Some(Vector(2, 2, 1, 0, 0))
		)
		.pipe(takeTurn("Alice clues 4 to Bob"))

		// Bob will wait for r3 to play, causing the bluff to fail.
		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("doesn't bluff through a wrong prompt"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "b1", "r3", "y3"),
			Vector("b1", "g4", "b4", "b3"),
			Vector("g2", "p3", "r3", "b5")
		),
			starting = Donald,
			playStacks = Some(Vector(0, 0, 0, 2, 0)),
			discarded = Vector("b4")
		)
		.pipe(takeTurn("Donald clues blue to Cathy"))	// [b3,b4]
		.pipe(takeTurn("Alice clues blue to Donald"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("doesn't bluff symmetrically finessed cards 1"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r2", "y3", "p4", "y4"),
			Vector("p3", "r4", "b2", "b3"),
			Vector("r3", "y2", "r1", "b5")
		),
			starting = Donald,
			playStacks = Some(Vector(1, 0, 0, 2, 0)),
			discarded = Vector("b4")
		)
		.pipe(takeTurn("Donald clues 3 to Cathy"))	// could be r3 (Bob f) or b3 (direct)
		.pipe(takeTurn("Alice clues yellow to Donald"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("doesn't bluff symmetrically finessed cards 2"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r2", "b3", "p4", "y4"),
			Vector("y1", "r4", "b2", "b3"),
			Vector("r3", "p2", "r1", "b5")
		),
			starting = Donald,
			playStacks = Some(Vector(1, 0, 0, 2, 0)),
			discarded = Vector("b4")
		)
		.pipe(takeTurn("Donald clues 3 to Bob"))	// could be y3 (Cathy + Alice f) or b3 (direct)
		.pipe(takeTurn("Alice clues purple to Donald"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("doesn't bluff when a delayed self interpretation exists"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r2", "p4", "g1", "p3", "y4"),
			Vector("r4", "y1", "b2", "r1", "b3")
		),
			starting = Cathy,
			playStacks = Some(Vector(1, 2, 0, 0, 0))
		)
		.pipe(takeTurn("Cathy clues 2 to Bob"))
		.pipe(takeTurn("Alice clues 4 to Bob"))

		// Bob will always play r2 first, so the bluff doesn't work.
		assertEquals(game.lastMove, Some(ClueInterp.Mistake))
