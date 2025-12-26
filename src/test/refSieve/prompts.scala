package tests.refSieve.prompts

import scala_bot.refSieve.RefSieve
import scala_bot.basics._
import scala_bot.test.{hasInfs, Player, preClue, setup, takeTurn}, Player._
import scala_bot.logger.{Logger, LogLevel}

import scala.util.chaining.scalaUtilChainingOps

class Prompts extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("recognizes a prompt via ref play"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "r1", "y4", "y4", "g4"),
			Vector("g1", "b4", "r2", "p4", "g4")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues red to Bob"))		// getting b1. r1 clued
		.pipe(takeTurn("Alice clues blue to Cathy"))	// getting g1. b4 clued
		.pipe(takeTurn("Bob plays b1", "p3"))
		.pipe(takeTurn("Cathy plays g1", "p3"))
		.pipe(takeTurn("Alice clues purple to Cathy"))	// getting r2

		// Bob's r1 should be prompted.
		hasInfs(game, None, Bob, 2, Vector("r1"))

	test("recognizes a prompt via fill-in"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "r1", "y4", "y4", "g4"),
			Vector("g1", "r2", "b4", "p4", "g4")
		),
			clueTokens = 7,
			init =
				preClue[RefSieve](Bob, 2, Seq("red")) andThen
				preClue[RefSieve](Cathy, 2, Seq("red"))
		)
		.pipe(takeTurn("Alice clues 2 to Cathy"))	// getting r2

		// Bob's r1 should be prompted.
		hasInfs(game, None, Bob, 2, Vector("r1"))

	test("doesn't give wrong prompt"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "r1", "r5", "y4", "g4"),
			Vector("g1", "r2", "b4", "p4", "g4")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues red to Bob"))		// getting b1. r1 and r5 clued
		.pipe(takeTurn("Alice clues red to Cathy"))		// getting g1. r2 clued
		.pipe(takeTurn("Bob plays b1", "r1"))
		.pipe(takeTurn("Cathy plays g1", "p3"))
		.pipe(takeTurn("Alice clues 2 to Cathy"))	// getting r2

		// This clue is nonsensical.
		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("recognizes a double prompt via fill-in"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "r2", "r1", "y4", "g4"),
			Vector("y4", "r3", "b4", "p4", "g4")
		),
			clueTokens = 7,
			init =
				preClue[RefSieve](Bob, 2, Seq("red")) andThen
				preClue[RefSieve](Bob, 3, Seq("red")) andThen
				preClue[RefSieve](Cathy, 2, Seq("red"))
		)
		.pipe(takeTurn("Alice clues 3 to Cathy"))

		// Bob's r1 and r2 should be prompted.
		hasInfs(game, None, Bob, 3, Vector("r1"))
		hasInfs(game, None, Bob, 2, Vector("r2"))

	test("doesn't give double prompts without filling in"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "r2", "r1", "y4", "g4"),
			Vector("y4", "r3", "b4", "p4", "g4")
		),
			init =
				preClue[RefSieve](Bob, 2, Seq("red")) andThen
				preClue[RefSieve](Bob, 3, Seq("red"))
		)
		.pipe(takeTurn("Alice clues blue to Cathy"))

		// This clue is nonsensical.
		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("plays into a prompt"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g1", "r2", "b4", "p4", "g4"),
			Vector("y4", "y4", "g4", "r4", "p4")
		),
			starting = Player.Cathy,
			clueTokens = 7,
			init =
				preClue[RefSieve](Alice, 2, Seq("red")) andThen
				preClue[RefSieve](Bob, 2, Seq("red"))
		)
		.pipe(takeTurn("Cathy clues 2 to Bob"))		// getting r2

		// We should be prompted in slot 2 for r1.
		hasInfs(game, None, Alice, 2, Vector("r1"))

	test("doesnt play into a satisfied prompt"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "y4", "b4", "p4"),
			Vector("r2", "y4", "g4", "r4"),
			Vector("g5", "b4", "r3", "y3")
		),
			starting = Donald,
			clueTokens = 7,
			init =
				// Alice, Bob, and Cathy all have cards clued with red in slot 1.
				preClue[RefSieve](Alice, 1, Seq("red")) andThen
				preClue[RefSieve](Bob, 1, Seq("red")) andThen
				preClue[RefSieve](Cathy, 1, Seq("red"))
		)
		.pipe(takeTurn("Donald clues 2 to Cathy"))		// prompting Bob's r2

		// We shouldn't be prompted in slot 1 for r1.
		assert(game.common.thoughts(game.state.hands(Alice.ordinal)(0)).inferred.length > 1)
