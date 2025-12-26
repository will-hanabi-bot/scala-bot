package tests.hgroup.level5

import scala_bot.basics._
import scala_bot.test.{hasInfs, hasStatus, Player, setup, takeTurn}, Player._
import scala_bot.hgroup.HGroup
import scala_bot.logger.{Logger, LogLevel}

import scala.util.chaining.scalaUtilChainingOps

class ClandestineFinesses extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands a clandestine finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "g4", "r5", "b4"),
			Vector("g1", "r1", "b2", "y3", "p3")
		),
			starting = Bob,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues 2 to Alice (slot 3)"))
		.tap: g =>
			hasInfs(g, None, Alice, 3, Vector("r2", "g2"))
		.pipe(takeTurn("Cathy plays g1", "b1"))
		.tap: g =>
			// Alice's slot 3 should still be [r2,g2] to allow for a clandestine finesse.
			hasInfs(g, None, Alice, 3, Vector("r2", "g2"))
		.pipe(takeTurn("Alice discards b1 (slot 5)"))
		.pipe(takeTurn("Bob discards b4", "g5"))
		.pipe(takeTurn("Cathy plays r1", "r1"))

		// Alice's slot 4 (used to be 3) should just be r2 now.
		hasInfs(game, None, Alice, 4, Vector("r2"))
		hasStatus(game, Alice, 1, CardStatus.None)

	test("understands a fake clandestine finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "g4", "r5", "b4"),
			Vector("g1", "r1", "b2", "y3", "p3")
		),
			starting = Bob,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues 2 to Alice (slot 3)"))
		.pipe(takeTurn("Cathy plays g1", "b1"))
		.pipe(takeTurn("Alice discards b1 (slot 5)"))

		.pipe(takeTurn("Bob discards b4", "g5"))
		.pipe(takeTurn("Cathy clues 5 to Bob"))

		// Alice's slot 4 (used to be 3) should just be g2 now.
		hasInfs(game, None, Alice, 4, Vector("g2"))
		hasStatus(game, Alice, 1, CardStatus.None)

	test("understands a symmetric clandestine finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "g4", "r5", "r3"),
			Vector("r2", "y1", "b2", "y3", "p3")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues 3 to Bob"))

		// r3 reverse, y3 clandestine
		hasInfs(game, None, Bob, 5, Vector("r3", "y3"))

	test("doesn't give illegal clandestine self-finesses"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g2", "r1", "g1", "y3", "p3"),
			Vector("r4", "r4", "g4", "r5", "b4")
		))
		.pipe(takeTurn("Alice clues 2 to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("gives finesses that forces another to prevent a clandestine self-finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g2", "r1", "g1", "y3", "p3"),
			Vector("g1", "r4", "g4", "r5", "b4")
		))
		.pipe(takeTurn("Alice clues 2 to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Play))
		hasStatus(game, Cathy, 1, CardStatus.Finessed)

	test("plays into a finesse to prevent a clandestine self-finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "g4", "r5", "b4"),
			Vector("g2", "r1", "g1", "y3", "p3")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 2 to Cathy"))

		// We must have g1 on finesse, otherwise Cathy will bomb g2 as r2.
		hasStatus(game, Alice, 1, CardStatus.Finessed)
		hasInfs(game, None, Alice, 1, Vector("g1"))

	test("recognizes fake clandestine finesses"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b3", "r4", "p1", "b1"),
			Vector("b4", "y2", "y5", "b3"),
			Vector("b1", "b2", "y1", "p2")
		))
		.pipe(takeTurn("Alice clues 4 to Cathy")) 	// triple finesse, but could be y
		.pipe(takeTurn("Bob clues 2 to Cathy"))		// reverse finesse
		.tap: g =>
			// Donald's y1 should be finessed, not our slot 1.
			hasStatus(g, Donald, 3, CardStatus.Finessed)
			hasInfs(g, None, Donald, 3, Vector("y1"))

			hasStatus(g, Alice, 1, CardStatus.None)
		.pipe(takeTurn("Cathy clues purple to Bob"))
		.pipe(takeTurn("Donald plays b1", "r5"))

		// Donald's y1 should still be finessed.
		hasStatus(game, Donald, 3, CardStatus.Finessed)
		hasInfs(game, None, Donald, 3, Vector("y1"))

		hasStatus(game, Alice, 1, CardStatus.None)
