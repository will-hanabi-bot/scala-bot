package tests.hgroup.level5

import scala_bot.basics._
import scala_bot.test.{hasInfs, Player, preClue, setup, takeTurn, TestVariant}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.{pipe}
import scala_bot.logger.{Logger, LogLevel}

class MultiPrompts extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("doesn't give a wrong prompt in rainbow"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r2", "m3", "r1", "r3"),
			Vector("g4", "g2", "b3", "m5"),
			Vector("b1", "y2", "r3", "r4")
		),
			playStacks = Some(Vector(0, 0, 0, 2, 0)),
			variant = TestVariant.Rainbow5,
			init = preClue(Bob, 2, Seq("blue"))
		)
		.pipe(takeTurn("Alice clues 2 to Bob"))

		// m3 will bomb as prompt into m2.
		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("understands a fill-in prompt in omni"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "g3", "b3", "y3", "o5")
		),
			starting = Bob,
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			variant = TestVariant.Omni5
		)
		.pipe(takeTurn("Bob clues 2 to Alice (slots 2,3,5)"))
		.pipe(takeTurn("Alice clues 5 to Bob"))
		.pipe(takeTurn("Bob clues red to Alice (slots 4,5)"))

		// The focus is !2 and !omni, so it must be r3 with r2 prompt in slot 5.
		hasInfs(game, None, Alice, 4, Vector("r3"))
		hasInfs(game, None, Alice, 5, Vector("r2"))

	test("prompts the card with the most positive information in rainbow"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "g3", "b3", "y3", "r4"),
			Vector("y4", "y4", "b3", "y3", "g4")
		),
			starting = Cathy,
			variant = TestVariant.Rainbow5,
			playStacks = Some(Vector(2, 0, 0, 0, 0)),
			clueTokens = 7,
			init =
				preClue[HGroup](Alice, 1, Seq("red")) andThen
				preClue[HGroup](Alice, 2, Seq("red", "3"))
		)
		.pipe(takeTurn("Cathy clues red to Bob"))

		// Alice should prompt slot 2 instead of slot 1.
		hasInfs(game, None, Alice, 2, Vector("r3"))
		assert(game.common.thoughts(game.state.hands(Alice.ordinal)(0)).inferred.length > 1)

	test("prompts leftmost when known rainbow"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "g3", "b3", "y3", "m4"),
			Vector("y4", "y4", "b3", "y3", "g4")
		),
			starting = Cathy,
			variant = TestVariant.Rainbow5,
			playStacks = Some(Vector(0, 0, 0, 0, 2)),
			clueTokens = 7,
			init =
				preClue[HGroup](Alice, 1, Seq("red", "green")) andThen
				preClue[HGroup](Alice, 2, Seq("red", "green", "blue"))
		)
		.pipe(takeTurn("Cathy clues red to Bob"))

		// Alice should prompt slot 1.
		hasInfs(game, None, Alice, 1, Vector("m3"))
		assert(game.common.thoughts(game.state.hands(Alice.ordinal)(1)).inferred.length > 1)

	test("prompts leftmost when known pink"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "g3", "b3", "y3", "i4"),
			Vector("y4", "y4", "b3", "y3", "g4")
		),
			starting = Cathy,
			variant = TestVariant.Pink5,
			playStacks = Some(Vector(0, 0, 0, 0, 2)),
			clueTokens = 7,
			init =
				preClue[HGroup](Alice, 1, Seq("pink")) andThen
				preClue[HGroup](Alice, 2, Seq("pink", "3"))
		)
		.pipe(takeTurn("Cathy clues pink to Bob"))

		// Alice should prompt slot 1.
		hasInfs(game, None, Alice, 1, Vector("i3"))
		assert(game.common.thoughts(game.state.hands(Alice.ordinal)(1)).inferred.length > 1)

	test("prompts leftmost when known omni"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "g3", "b3", "y3", "o4"),
			Vector("y4", "y4", "b3", "y3", "g4")
		),
			starting = Cathy,
			variant = TestVariant.Omni5,
			playStacks = Some(Vector(0, 0, 0, 0, 2)),
			clueTokens = 7,
			init =
				preClue[HGroup](Alice, 1, Seq("red", "green")) andThen
				preClue[HGroup](Alice, 2, Seq("red", "green", "blue", "3", "5"))
		)
		.pipe(takeTurn("Cathy clues red to Bob"))

		// Alice should prompt slot 1.
		hasInfs(game, None, Alice, 1, Vector("o3"))
		assert(game.common.thoughts(game.state.hands(Alice.ordinal)(1)).inferred.length > 1)

	test("prompts the card with most positive information in pink"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "g3", "b3", "y3", "i4"),
			Vector("y4", "y4", "b3", "y3", "g4")
		),
			starting = Cathy,
			variant = TestVariant.Pink5,
			playStacks = Some(Vector(0, 0, 0, 0, 2)),
			clueTokens = 7,
			init =
				preClue[HGroup](Alice, 1, Seq("3")) andThen
				preClue[HGroup](Alice, 2, Seq("3", "5"))
		)
		.pipe(takeTurn("Cathy clues pink to Bob"))

		// Alice should prompt slot 2.
		hasInfs(game, None, Alice, 2, Vector("i3"))
		assert(game.common.thoughts(game.state.hands(Alice.ordinal)(0)).inferred.length > 1)
