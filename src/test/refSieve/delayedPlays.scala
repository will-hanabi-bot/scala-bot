package tests.refSieve.delayedPlays

import scala_bot.refSieve.RefSieve
import scala_bot.basics._
import scala_bot.test.{hasInfs, Player, setup, takeTurn}, Player._
import scala_bot.logger.{Logger, LogLevel}

import scala.util.chaining.scalaUtilChainingOps

class DelayedPlays extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("gives a delayed play clue through a playable card"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "b4", "y4", "y4", "g4"),
			Vector("b2", "b4", "p4", "p4", "g4")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 1 to Bob"))
		.pipe(takeTurn("Alice clues blue to Cathy"))

		assertEquals(game.lastMove, Some(ClueInterp.Play))

	test("gives a delayed play clue through the leftmost playable card"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "r1", "y4", "y4", "g4"),
			Vector("b2", "b4", "p4", "p4", "g4")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 1 to Bob"))
		.pipe(takeTurn("Alice clues blue to Cathy"))

		assertEquals(game.lastMove, Some(ClueInterp.Play))

	test("doesn't give a delayed play clue through a non-leftmost playable card"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "r1", "y4", "y4", "g4"),
			Vector("r2", "b4", "p4", "p4", "g4")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 1 to Bob"))
		.pipe(takeTurn("Alice clues blue to Cathy"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("understands a self-delayed play clue"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "b4", "y4", "y4", "g4"),
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 3)"))	// slot 2 called to play
		.pipe(takeTurn("Alice clues blue to Bob"))
		.pipe(takeTurn("Bob clues yellow to Alice (slot 2)"))	// slot 2 is known y1, slot 1 called to play
		.pipe(takeTurn("Alice plays y1 (slot 2)"))
		.pipe(takeTurn("Bob clues purple to Alice (slots 1,2)"))	// slot 2 is known p1

		hasInfs(game, None, Alice, 1, Vector("p2"))
		hasInfs(game, None, Alice, 2, Vector("p1"))
