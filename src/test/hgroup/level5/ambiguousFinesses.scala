package tests.hgroup.level5

import scala_bot.basics._
import scala_bot.test.{hasInfs, hasStatus, Player, setup, takeTurn}, Player._
import scala_bot.hgroup.HGroup
import scala_bot.logger.{Logger, LogLevel}

import scala.util.chaining.scalaUtilChainingOps
import scala_bot.test.preClue

class AmbiguousFinesses extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands an ambiguous finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r4", "g2", "r4", "y4"),
			Vector("r3", "y4", "b4", "b4"),
			Vector("g1", "p4", "p4", "r3")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues green to Bob"))
		.tap: g =>
			hasStatus(g, Donald, 1, CardStatus.Finessed)
		.pipe(takeTurn("Donald discards r3", "b1"))

		hasInfs(game, None, Alice, 1, Vector("g1"))
		hasStatus(game, Alice, 1, CardStatus.Finessed)

	test("understands an ambiguous finesse with a self component"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p4", "p4", "r4", "y4", "y4"),
			Vector("r1", "y5", "b4", "b4", "g4")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 2 to Alice (slot 3)"))
		.tap: g =>
			hasStatus(g, Cathy, 1, CardStatus.Finessed)
		.pipe(takeTurn("Cathy discards g4", "r1"))

		hasStatus(game, Alice, 1, CardStatus.Finessed)

	test("recognizes an ambiguous self-finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("p1", "p2", "r2", "g4"),
			Vector("p1", "b3", "g2", "b4"),
			Vector("y2", "y4", "r1", "g3")
		))
		.pipe(takeTurn("Alice clues 2 to Bob"))				// self-finesse on p1
		.tap:
			hasStatus(_, Bob, 1, CardStatus.Finessed)
		.pipe(takeTurn("Bob clues 1 to Alice (slots 2,3)")) // Bob thinks it's on Cathy
		.tap:
			hasStatus(_, Bob, 1, CardStatus.Finessed)
		.pipe(takeTurn("Cathy clues red to Donald"))		// Cathy passes back

		hasStatus(game, Bob, 1, CardStatus.Finessed)

	test("recognizes an ambiguous self-finesse when a direct clue is impossible"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4"),
			Vector("y2", "g4", "g4", "b4"),
			Vector("g2", "p4", "g5", "p4")
		),
			playStacks = Some(Vector(0, 1, 0, 0, 0)),
			discarded = Vector("y2"),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues 2 to Alice (slot 2)"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Finessed)
		.pipe(takeTurn("Alice plays r1 (slot 1)"))

		hasInfs(game, None, Alice, 2, Vector("r2"))

	test("recognizes an ambiguous reverse finesse where the finessed card is discarded"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p4", "r4", "r4", "p4", "y4"),
			Vector("r2", "b5", "p5", "y5", "r3")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			init = preClue[HGroup](Cathy, 5, Seq("red")) andThen
				preClue[HGroup](Cathy, 2, Seq("5")) andThen
				preClue[HGroup](Cathy, 3, Seq("5")) andThen
				preClue[HGroup](Cathy, 4, Seq("5")),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 3 to Cathy"))
		.pipe(takeTurn("Cathy discards r2", "p3"))

		hasInfs(game, None, Alice, 1, Vector("r2"))
		hasStatus(game, Alice, 1, CardStatus.Finessed)

	test("plays into an ambiguous self-finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("p1", "b3", "g2", "b4"),
			Vector("y2", "y4", "r1", "g3"),
			Vector("b2", "g1", "y1", "r4"),
		),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues 2 to Alice (slots 2,3)"))	// self-finesse on p1 (seems to be on Bob)
		.tap:
			hasStatus(_, Bob, 1, CardStatus.Finessed)
		.pipe(takeTurn("Alice clues 1 to Donald"))
		.pipe(takeTurn("Bob clues red to Cathy"))				// Bob passes back

		// Alice now knows she is self-finessed.
		hasStatus(game, Alice, 1, CardStatus.Finessed)
		hasInfs(game, None, Alice, 1, Vector("y1", "g1", "b1", "p1"))

	test("doesn't expect a passback if no other ids are possible"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("p1", "p2", "r2", "g4"),
			Vector("p1", "b3", "g2", "b4"),
			Vector("y2", "y4", "r1", "g3"),
		),
			playStacks = Some(Vector(2, 2, 2, 2, 0))
		)
		.pipe(takeTurn("Alice clues 2 to Bob"))
		.pipe(takeTurn("Bob clues 1 to Alice (slots 2,3)"))	// Bob thinks it's on Cathy

		// Cathy should now know to play p1.
		hasStatus(game, Bob, 1, CardStatus.None)
		hasStatus(game, Cathy, 1, CardStatus.Finessed)

	test("passes back a layered ambiguous finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r3", "g2", "g4", "r5"),
			Vector("g3", "y4", "p4", "y3"),
			Vector("r1", "r2", "b4", "p3"),
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 3 to Bob"))
		.pipe(takeTurn("Donald discards p3", "b3"))

		// Alice should pass back.
		hasStatus(game, Alice, 1, CardStatus.None)
		hasStatus(game, Donald, 2, CardStatus.Finessed)

	test("reacts to an ambiguous finesse passback"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "g4", "b4", "y4"),
			Vector("r3", "g2", "g4", "r5"),
			Vector("g3", "y4", "p4", "y3"),
		),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues 3 to Cathy"))
		.pipe(takeTurn("Alice clues 5 to Cathy"))		// We think the finesse is on Bob
		.pipe(takeTurn("Bob discards y4", "p3"))		// Bob passes back

		// Alice should play now.
		hasStatus(game, Alice, 1, CardStatus.Finessed)
		hasStatus(game, Bob, 1, CardStatus.None)
