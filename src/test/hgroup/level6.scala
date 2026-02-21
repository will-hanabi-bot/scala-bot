package tests.hgroup.level6

import scala_bot.basics._
import scala_bot.test.{hasStatus, Player, preClue, setup, takeTurn}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.pipe
import scala_bot.logger.{Logger, LogLevel}

class General extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands a tccm"):
		val game = setup(HGroup.atLevel(6), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "r2", "g4", "r5", "b4"),
			Vector("g1", "b3", "r2", "y3", "p3")
		))
		.pipe(takeTurn("Alice clues red to Bob"))
		.pipe(takeTurn("Bob plays r1", "y5"))
		.pipe(takeTurn("Cathy clues 2 to Bob"))

		hasStatus(game, Bob, 5, CardStatus.ChopMoved)

	test("understands a tccm on self"):
		val game = setup(HGroup.atLevel(6), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g2", "y5", "g4", "r5", "b4"),
			Vector("g1", "b3", "r2", "y3", "p3")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues red to Alice (slots 1,2)"))
		.pipe(takeTurn("Alice plays r1 (slot 1)"))
		.pipe(takeTurn("Bob clues 2 to Alice (slot 2)"))

		hasStatus(game, Alice, 5, CardStatus.ChopMoved)

	test("recognizes a tempo clue stall"):
		val game = setup(HGroup.atLevel(6), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g1", "g2", "r4", "y3"),
			Vector("b5", "y5", "g5", "r5"),
			Vector("b4", "p3", "g1", "g1")
		),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues 5 to Cathy"))
		.pipe(takeTurn("Alice clues green to Bob"))
		.pipe(takeTurn("Bob plays g1 (slot 1)", "y2"))
		.pipe(takeTurn("Cathy clues 2 to Bob"))

		hasStatus(game, Bob, 4, CardStatus.None)

	test("doesn't tccm if getting a chop moved card"):
		val game = setup(HGroup.atLevel(6), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g1", "g2", "r1", "y2"),
			Vector("b5", "y5", "g5", "r5"),
			Vector("b4", "p3", "g1", "g4")
		),
			starting = Donald,
			playStacks = Some(Vector(1, 1, 1, 1, 1))
		)
		.pipe(takeTurn("Donald clues 1 to Bob"))
		.pipe(takeTurn("Alice clues yellow to Bob"))

		hasStatus(game, Bob, 2, CardStatus.None)

	test("doesn't tccm if getting a playable in other hand"):
		val game = setup(HGroup.atLevel(6), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("p1", "p2", "r1", "y1"),
			Vector("b2", "y5", "g3", "y3"),
			Vector("b4", "p4", "g1", "g1")
		),
			starting = Cathy,
			playStacks = Some(Vector(2, 2, 2, 2, 0)),
			discarded = Vector("y3")
		)
		.pipe(takeTurn("Cathy clues purple to Bob"))
		.pipe(takeTurn("Donald clues 3 to Cathy"))
		.pipe(takeTurn("Alice clues 2 to Bob"))		// p2 play unlocks Cathy's touched g3

		hasStatus(game, Bob, 4, CardStatus.None)

	test("doesn't tccm if the card was already playing symmetrically"):
		val game = setup(HGroup.atLevel(6), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b2", "r3", "r1", "y1"),
			Vector("p1", "y5", "g3", "y3"),
			Vector("b4", "p4", "g1", "g1")
		),
		)
		.pipe(takeTurn("Alice clues purple to Cathy"))
		.pipe(takeTurn("Bob clues purple to Cathy"))

		hasStatus(game, Cathy, 4, CardStatus.None)

	test("tccms if a card was already playing but unknown"):
		val game = setup(HGroup.atLevel(6), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "y1", "r4", "r4", "y4"),
			Vector("g4", "g4", "b4", "b4", "y2")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 2 to Cathy"))
		.pipe(takeTurn("Cathy clues 1 to Bob"))
		.pipe(takeTurn("Alice clues yellow to Cathy"))

		hasStatus(game, Cathy, 4, CardStatus.ChopMoved)

	// test("doesn't tccm if the card was already playing asymmetrically 1"):
	// 	val game = setup(HGroup.atLevel(6), Vector(
	// 		Vector("xx", "xx", "xx", "xx"),
	// 		Vector("b2", "r5", "p4", "p3"),
	// 		Vector("p5", "y5", "g3", "y3"),
	// 		Vector("b4", "p4", "g1", "g1")
	// 	),
	// 		starting = Cathy,
	// 		playStacks = Some(Vector(0, 0, 0, 0, 2))
	// 	)
	// 	.pipe(takeTurn("Cathy clues purple to Bob"))
	// 	.pipe(takeTurn("Donald clues 5 to Alice (slot 4)"))
	// 	.pipe(takeTurn("Alice clues 4 to Bob"))

	// 	hasStatus(game, Bob, 2, CardStatus.None)

	test("prefers interpreting a tccm over tcm"):
		val game = setup(HGroup.atLevel(6), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b4", "g4", "r5", "r4"),
			Vector("g3", "y4", "y1", "y5", "b3"),
		),
			starting = Cathy,
			playStacks = Some(Vector(4, 0, 0, 0, 0)),
			init = preClue[HGroup](Alice, 5, Vector("5"))
		)
		.pipe(takeTurn("Cathy clues red to Alice (slots 2,5)"))

		// Slot 4 should be chop moved, but not slot 3.
		hasStatus(game, Alice, 4, CardStatus.ChopMoved)
		hasStatus(game, Alice, 3, CardStatus.None)
