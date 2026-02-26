package tests.hgroup

import scala_bot.test.{hasInfs, Player, setup, takeTurn, TestVariant}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.pipe
import scala_bot.logger.{Logger, LogLevel}

class Muddy extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands muddy save with red"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "r4"),
		),
			starting = Bob,
			variant = TestVariant.Muddy5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 5)"))

		hasInfs(game, None, Alice, 5, Vector("r1", "m1", "m2", "m5"))

	test("understands cocoa save with red"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "r4"),
		),
			starting = Bob,
			variant = TestVariant.Cocoa5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 5)"))

		hasInfs(game, None, Alice, 5, Vector("r1", "m1", "m2", "m3", "m4", "m5"))

	test("doesn't interpret other colours as saves"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "r4"),
		),
			starting = Bob,
			variant = TestVariant.Muddy5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues green to Alice (slot 5)"))

		hasInfs(game, None, Alice, 5, Vector("g1", "m1"))

	test("correctly interprets mud clues in muddy"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "r4"),
			Vector("r1", "y4", "r3", "r3", "r4"),
		),
			starting = Bob,
			variant = TestVariant.Muddy5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues red to Alice (slots 2,3,4,5)"))
		.pipe(takeTurn("Cathy clues green to Alice (slots 3,4,5)"))

		hasInfs(game, None, Alice, 4, Vector("m1"))

	test("correctly interprets mud clues in cocoa"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "r4"),
			Vector("r1", "y4", "r3", "r3", "r4"),
		),
			starting = Bob,
			variant = TestVariant.Muddy5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues red to Alice (slots 4,5)"))
		.pipe(takeTurn("Cathy clues yellow to Alice (slots 4,5)"))

		hasInfs(game, None, Alice, 5, Vector("m1"))

	test("recognizes normal tempo clues when the leftmost card isn't muddy"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4"),
			Vector("y4", "y4", "r4", "r4"),
			Vector("y3", "y3", "g3", "g3"),
		),
			starting = Bob,
			variant = TestVariant.Muddy5,
			playStacks = Some(Vector(0, 0, 1, 0, 0)),
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues red to Alice (slots 3,4)"))
		.pipe(takeTurn("Cathy clues 2 to Alice (slot 2)"))
		.pipe(takeTurn("Donald clues green to Alice (slots 2,3,4)"))

		hasInfs(game, None, Alice, 3, Vector("m1", "m2", "m3", "m4", "m5"))
		hasInfs(game, None, Alice, 4, Vector("m1", "m2", "m5"))

	test("wraps around a mud clue"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "r4"),
			Vector("y4", "y4", "r3", "r3", "r4"),
		),
			starting = Bob,
			variant = TestVariant.Muddy5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues yellow to Alice (slots 3,4,5)"))
		.pipe(takeTurn("Cathy clues red to Alice (slots 3,4)"))

		hasInfs(game, None, Alice, 3, Vector("m1"))

	test("skips over known non-muddy cards"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4"),
			Vector("y4", "y4", "r4", "r4"),
			Vector("y3", "y3", "g3", "g3"),
		),
			starting = Bob,
			variant = TestVariant.Muddy5,
			playStacks = Some(Vector(0, 0, 1, 0, 0)),
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues 5 to Alice (slot 3)"))
		.pipe(takeTurn("Cathy clues green to Alice (slots 1,2,4)"))
		.pipe(takeTurn("Donald clues blue to Alice (slots 1,2,3)"))

		hasInfs(game, None, Alice, 2, Vector("m1"))
