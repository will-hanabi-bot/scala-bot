package tests.hgroup.level11

import scala_bot.basics._
import scala_bot.test.{fullyKnown, hasInfs, hasStatus, Player, preClue, setup, takeTurn, TestVariant}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.pipe
import scala_bot.logger.{Logger, LogLevel}

class TrueClues extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands a fake bluff"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b3", "g3", "r3", "g5"),
			Vector("p1", "r4", "b5", "b2"),
			Vector("r2", "b2", "g1", "y3")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues blue to Donald"))		// b1 finesse on us
		.pipe(takeTurn("Cathy discards b2", "y5"))

		hasStatus(game, Alice, 1, CardStatus.Finessed)
		hasInfs(game, None, Alice, 1, Vector("b1"))
		hasStatus(game, Cathy, 2, CardStatus.None)

	test("understands a direct play if the bluff isn't played into"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p1", "y5", "b1", "g5", "p2"),
			Vector("b3", "r1", "b5", "b2", "y4")
		),
			starting = Bob,
			playStacks = Some(Vector(2, 2, 2, 2, 2))
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 2)"))
		.pipe(takeTurn("Cathy discards y4", "y1"))

		hasInfs(game, None, Alice, 2, Vector("r3"))

	test("understands giving a direct play through a bluff opportunity"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b3", "r1", "b1", "g5", "p2"),
			Vector("p1", "r3", "b5", "b2", "y4")
		),
			playStacks = Some(Vector(2, 2, 2, 2, 2))
		)
		.pipe(takeTurn("Alice clues red to Cathy"))
		.pipe(takeTurn("Bob discards p2", "y5"))

		hasInfs(game, None, Cathy, 2, Vector("r3"))

	test("understands a self-finesse that's too far away to be a bluff"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g5", "y2", "b3", "y5"),
			Vector("p4", "r3", "g1", "y4"),
			Vector("p2", "b5", "b1", "y1")
		),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues 4 to Alice (slot 2)"))

		// No 4 is a valid bluff target, so this must be a finesse.
		hasStatus(game, Alice, 1, CardStatus.Finessed)
		hasInfs(game, None, Alice, 1, Vector("r1", "y1", "g1", "b1", "p1"))

	test("recognizes a finesse when focus is not a valid bluff target"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b3", "y3", "g1", "y1"),
			Vector("r2", "b1", "p5", "p1"),
			Vector("p2", "r1", "b2", "y1")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 2 to Donald"))

		hasStatus(game, Alice, 1, CardStatus.Finessed)
		hasInfs(game, None, Alice, 1, Vector("p1"))

	test("assumes a reverse finesse over a bluff"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b4", "r1", "g5", "g2"),
			Vector("b3", "w1", "w5", "w2"),
			Vector("r1", "r4", "b2", "y4")
		),
			starting = Donald,
			playStacks = Some(Vector(2, 2, 2, 2, 2)),
			variant = TestVariant.White5
		)
		.pipe(takeTurn("Donald clues blue to Bob"))

		hasStatus(game, Alice, 1, CardStatus.None)
		hasStatus(game, Cathy, 1, CardStatus.Finessed)
		hasInfs(game, None, Cathy, 1, Vector("b3"))

	test("assumes a finesse over a self-bluff"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g4", "g2", "p1", "b2"),
			Vector("b4", "g4", "y4", "r4"),
			Vector("y3", "r1", "g3", "p3")
		),
			starting = Cathy,
			playStacks = Some(Vector(0, 1, 0, 0, 1))
		)
		.pipe(takeTurn("Cathy clues red to Donald"))
		.pipe(takeTurn("Donald clues 3 to Alice (slot 1)"))

		// Since Alice has to respect r3, she can't be bluffed.
		hasStatus(game, Alice, 2, CardStatus.Finessed)

	test("understands a double finesse when the target is too far away"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g4", "p3", "r3", "b4"),
			Vector("y1", "y2", "p2", "p4"),
			Vector("b1", "y5", "g2", "r4")
		),
			starting = Donald,
			playStacks = Some(Vector(3, 4, 1, 1, 3)),
			discarded = Vector("r1", "y3", "g3")
		)
		.pipe(takeTurn("Donald clues blue to Bob"))

		hasStatus(game, Alice, 1, CardStatus.Finessed)
		hasInfs(game, None, Alice, 1, Vector("b2"))
		hasStatus(game, Alice, 2, CardStatus.Finessed)
		hasInfs(game, None, Alice, 2, Vector("b3"))

	test("receives a fake bluff into double finesse"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "r1", "y1", "g5", "p2"),
			Vector("p1", "r4", "b5", "b2", "y4")
		),
			starting = Bob,
			playStacks = Some(Vector(1, 1, 1, 0, 0))
		)
		.pipe(takeTurn("Bob clues 3 to Alice (slot 2)"))
		.pipe(takeTurn("Cathy discards y4", "y5"))

		hasStatus(game, Alice, 1, CardStatus.Finessed)

	test("understands a double reverse finesse when the target is too far away"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g3", "p4", "r2", "y1"),
			Vector("g4", "p1", "b4", "p3"),
			Vector("b4", "y2", "g5", "y3")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 3 to Donald"))

		hasStatus(game, Alice, 1, CardStatus.Finessed)
		hasInfs(game, None, Alice, 1, Vector("y1"))
		hasStatus(game, Alice, 2, CardStatus.Finessed)
		hasInfs(game, None, Alice, 2, Vector("y2"))

	test("understands a layered finesse when the target is too far away"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y4", "p4", "r2", "y1"),
			Vector("g5", "b1", "b4", "y3"),
			Vector("g4", "p1", "g3", "p3")
		),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues 3 to Cathy"))
		.pipe(takeTurn("Alice plays r1 (slot 1)"))

		hasStatus(game, Alice, 2, CardStatus.Finessed)
		hasInfs(game, None, Alice, 2, Vector("y1"))

	test("understands a clandestine finesse"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y4", "y5", "y1", "y1"),
			Vector("g3", "g3", "b2", "b1"),
			Vector("g1", "r1", "r3", "y2")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 3 to Alice (slot 4)"))
		.pipe(takeTurn("Donald plays g1", "g5"))

		// No 3 is a valid bluff target.
		hasInfs(game, None, Alice, 4, Vector("r3", "g3"))
		hasStatus(game, Donald, 2, CardStatus.Finessed)

	test("rank connects on a self-finesse"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b4", "p1", "y1", "p4"),
			Vector("r3", "p3", "p1", "y2"),
			Vector("b3", "y2", "g4", "r5")
		),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues 2 to Alice (slot 3)"))
		.pipe(takeTurn("Alice plays g1 (slot 1)"))

		hasInfs(game, None, Alice, 3, Vector("g2"))

	test("doesn't allow a second round when a bluff isn't played into"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g1", "g3", "p4", "p3"),
			Vector("r5", "y5", "g4", "r1"),
			Vector("r3", "g1", "p2", "r3")
		),
			starting = Cathy,
			playStacks = Some(Vector(2, 0, 0, 0, 0)),
			init = fullyKnown(Donald, 3, "p2")
		)
		.pipe(takeTurn("Cathy clues purple to Bob"))
		.pipe(takeTurn("Donald discards r3 (slot 4)", "r4"))

		hasStatus(game, Alice, 1, CardStatus.Finessed)
		hasInfs(game, None, Alice, 1, Vector("p1"))

	test("doesn't play into a bluff when a self-finesse + prompt is possible"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "r3", "y4", "g4", "b4"),
			Vector("p4", "p4", "y4", "g4", "b4")
		),
			starting = Cathy,
			init = preClue(Alice, 5, Seq("2"))
		)
		.pipe(takeTurn("Cathy clues 3 to Bob"))

		// Alice should wait for Bob to demonstrate.
		hasStatus(game, Alice, 1, CardStatus.None)
