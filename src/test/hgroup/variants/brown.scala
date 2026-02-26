package tests.hgroup

import scala_bot.basics._
import scala_bot.test.{hasInfs, hasStatus, Player, setup, takeTurn, TestVariant}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.pipe
import scala_bot.logger.{Logger, LogLevel}

class Brown extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands n2/5 save with brown"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g4")
		),
			starting = Bob,
			variant = TestVariant.Brown5
		)
		.pipe(takeTurn("Bob clues brown to Alice (slot 5)"))

		hasInfs(game, None, Alice, 5, Vector("n1", "n2", "n5"))

	test("will save n5 with brown"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "n5")
		),
			variant = TestVariant.Brown5
		)

		assertEquals(game.takeAction, PerformAction.Colour(Bob.ordinal, 4))

	test("plays the correct card after a delayed play clue"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g2", "b1", "r2", "r3", "n5"),
			Vector("g4", "n1", "b3", "g4", "y3")
		),
			starting = Bob,
			variant = TestVariant.Brown5,
			playStacks = Some(Vector(0, 0, 0, 0, 3))
		)
		.pipe(takeTurn("Bob clues brown to Alice (slots 2,4,5)"))
		.pipe(takeTurn("Cathy clues brown to Bob"))

		// Alice's slot 5 is playable.
		assertEquals(game.common.thinksPlayables(game, Alice.ordinal),
			Vector(game.state.hands(Alice.ordinal)(4)))

	test("plays the correct card after a finesse"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("n5", "b1", "r2", "r3"),
			Vector("g3", "y3", "r1", "n1"),
			Vector("n3", "y1", "b3", "g4")
		),
			starting = Bob,
			variant = TestVariant.Brown5,
			playStacks = Some(Vector(0, 0, 0, 0, 2)),
			discarded = Vector("n3")
		)
		.pipe(takeTurn("Bob clues brown to Alice (slots 2,3,4)"))
		.pipe(takeTurn("Cathy clues brown to Bob"))
		.pipe(takeTurn("Donald plays n3", "y2"))

		// Alice's slot 4 is playable.
		assertEquals(game.common.thinksPlayables(game, Alice.ordinal),
			Vector(game.state.hands(Alice.ordinal)(3)))

	test("understands a brown tempo clue"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g2", "b1", "r2", "r3", "n5"),
			Vector("g4", "n1", "b3", "g4", "y3")
		),
			starting = Cathy,
			variant = TestVariant.Brown5
		)
		.pipe(takeTurn("Cathy clues brown to Alice (slots 3,4,5)"))
		.pipe(takeTurn("Alice clues brown to Bob"))
		.pipe(takeTurn("Bob clues brown to Alice (slots 3,4,5)"))

		.pipe(takeTurn("Cathy clues blue to Bob"))

		// Alice's slot 5 is playable.
		assertEquals(game.common.thinksPlayables(game, Alice.ordinal),
			Vector(game.state.hands(Alice.ordinal)(4)))

		// Slot 2 should not be chop moved.
		hasStatus(game, Alice, 2, CardStatus.None)
