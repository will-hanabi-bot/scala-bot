package tests.hgroup.level11

import scala_bot.basics._
import scala_bot.test.{hasInfs, hasStatus, Player, preClue, setup, takeTurn, TestVariant}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.pipe
import scala_bot.logger.{Logger, LogLevel}

class ComplexBluffs extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("bluffs on top of differently-coloured plays"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "r2", "y1", "y1", "r4"),
			Vector("p5", "b2", "r3", "y5", "y4")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues red to Bob"))
		.pipe(takeTurn("Alice clues blue to Cathy"))

		hasStatus(game, Bob, 3, CardStatus.Bluffed)

	test("bluffs on top of differently-ranked plays"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "p3", "y1", "b4", "r4"),
			Vector("g5", "g3", "y4", "y4", "r4")
		),
			starting = Cathy,
			playStacks = Some(Vector(0, 0, 1, 0, 2))
		)
		.pipe(takeTurn("Cathy clues 1 to Bob"))
		.pipe(takeTurn("Alice clues 3 to Cathy"))

		hasStatus(game, Bob, 2, CardStatus.Bluffed)

	test("doesn't consider invalid bluff targets"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b1", "y2", "r2", "y4"),
			Vector("p3", "p1", "y1", "r4"),
			Vector("g5", "y1", "p4", "b5")
		),
			starting = Cathy,
			playStacks = Some(Vector(1, 0, 0, 0, 1)),
			init = preClue(Bob, 3, Seq("red"))
		)
		.pipe(takeTurn("Cathy clues yellow to Donald"))
		.pipe(takeTurn("Donald clues green to Alice (slots 3,4)"))
		.pipe(takeTurn("Alice clues 3 to Cathy"))
		.pipe(takeTurn("Bob plays b1", "b4"))

		// Cathy's card must be exactly p3 as a bluff.
		hasInfs(game, None, Cathy, 1, Vector("p3"))

	test("doesn't confuse a bluff for a layered finesse"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g1", "b1", "y4", "y3"),
			Vector("g4", "r5", "b2", "p4"),
			Vector("r1", "r1", "r3", "y1")
		))
		.pipe(takeTurn("Alice clues blue to Cathy"))
		.pipe(takeTurn("Bob plays g1", "b4"))

		hasStatus(game, Bob, 2, CardStatus.None)
		hasInfs(game, None, Cathy, 3, Vector("b2"))

	test("disambiguates a bluff/finesse when demonstrated"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p2", "b4", "y1", "p4", "g4"),
			Vector("r3", "p3", "g1", "y2", "b3")
		),
			starting = Cathy,
			playStacks = Some(Vector(1, 1, 0, 0, 0))
		)
		.pipe(takeTurn("Cathy clues 3 to Alice (slot 1)"))
		.pipe(takeTurn("Alice plays p1 (slot 2)"))

		val finesseGame = takeTurn("Bob plays p2", "r5")(game)

		// Slot 2 (was slot 1) is known to be p3 as a double finesse.
		hasInfs(finesseGame, None, Alice, 2, Vector("p3"))

		val bluffGame = takeTurn("Bob clues green to Cathy")(game)

		// Slot 2 (was slot 1) is known to be [r3,y3] as a bluff.
		hasInfs(bluffGame, None, Alice, 2, Vector("r3", "y3"))

	test("prompts when considering whether a bluff is valid"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b1", "g1", "p2", "y3"),
			Vector("r1", "b1", "r3", "b4"),
			Vector("b3", "p3", "r3", "r2")
		),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues blue to Alice (slots 1,3)"))
		.pipe(takeTurn("Alice plays b1 (slot 1)"))
		.pipe(takeTurn("Bob clues 4 to Alice (slot 1)"))	// could be b2 prompt
		.pipe(takeTurn("Cathy plays r1", "g2"))

		hasInfs(game, None, Alice, 3, Vector("b3"))

	test("interprets a bluff when a hidden finesse is impossible"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b4", "g3", "b3", "m2"),
			Vector("r2", "y4", "g4", "y3"),
			Vector("g2", "b4", "m4", "y1")
		),
			starting = Bob,
			variant = TestVariant.Rainbow5
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 2)"))	// r1,m1
		.pipe(takeTurn("Cathy clues red to Bob"))		// m2 (promising m1 in Alice's hand)
		.pipe(takeTurn("Donald clues red to Cathy"))	// bluffing Alice (can't be hidden f)

		hasStatus(game, Alice, 1, CardStatus.Bluffed)
