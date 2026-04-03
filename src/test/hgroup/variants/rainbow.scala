package tests.hgroup

import scala_bot.basics._
import scala_bot.test.{hasInfs, hasStatus, Player, preClue, setup, takeTurn, TestVariant}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger, LogLevel}

class Rainbow extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands a delayed play through possible rainbow ids"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g2", "r1", "b2", "y3"),
			Vector("r2", "y4", "b3", "r3"),
			Vector("y4", "m2", "r4", "g1")
		),
			variant = TestVariant.Rainbow5,
			playStacks = Some(Vector(0, 0, 0, 0, 1)),
			clueTokens = 7
		)
		.pipe(takeTurn("Alice clues 2 to Donald"))		// getting m2
		.pipe(takeTurn("Bob clues green to Donald"))	// getting g1 (note [g1,m3])
		.pipe(takeTurn("Cathy clues green to Bob"))		// getting g2

		// Alice shouldn't be finessed for g1.
		hasStatus(game, Alice, 1, CardStatus.None)

	test("connects through own unknown cards"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "y2", "r2", "r3", "m3"),
			Vector("y4", "b3", "b4", "r1", "b2")
		),
			starting = Cathy,
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			variant = TestVariant.Rainbow5
		)
		.pipe(takeTurn("Cathy clues 1 to Alice (slots 2,4)"))
		.pipe(takeTurn("Alice plays g1 (slot 2)"))
		.pipe(takeTurn("Bob clues 2 to Alice (slots 2,3)"))		// xx x2 x2 x1 xx

		.pipe(takeTurn("Cathy clues yellow to Alice (slots 1,2,4)"))

		// Only slot 4 is playable.
		assertEquals(game.common.thinksPlayables(game, Alice.ordinal), Vector(game.state.hands(Alice.ordinal)(3)))

		val yellow = takeTurn("Alice plays y1 (slot 4)")(game)
		hasInfs(yellow, None, Alice, 2, Vector("y3", "m1"))

		val rainbow = takeTurn("Alice plays m1 (slot 4)")(game)
		hasInfs(rainbow, None, Alice, 2, Vector("y1", "m3"))

	test("doesn't consider a playable connection to be a prompt"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g1", "g1", "m1", "m1"),
			Vector("b4", "y2", "r2", "r3"),
			Vector("y4", "g3", "m2", "r4")
		),
			starting = Bob,
			playStacks = Some(Vector(0, 0, 2, 0, 1)),
			variant = TestVariant.Rainbow5,
			init = preClue(Donald, 3, Seq("2"))
		)
		.pipe(takeTurn("Bob clues green to Donald"))
		.tap: g =>
			hasInfs(g, None, Donald, 2, Vector("g3", "m3"))
		.pipe(takeTurn("Cathy clues 4 to Alice (slot 4)"))

		val wc = game.waiting.find: wc =>
			wc.target == game.state.hands(Alice.ordinal)(3) &&
			wc.inference == game.state.expandShort("g4")

		// Not a prompt.
		assert(wc.exists(w => !w.connections.exists(_.isInstanceOf[PromptConn])))

	test("prompts the card with the most positive information"):
		val game = setup(HGroup.atLevel(1), Vector(
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

	test("plays into a free choice prompt"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "g3", "b3", "b3", "m2"),
			Vector("y4", "y4", "y3", "y3", "g4")
		),
			starting = Cathy,
			variant = TestVariant.Rainbow5,
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			clueTokens = 7,
			init = preClue(Alice, 2, Seq("red"))
		)
		.pipe(takeTurn("Cathy clues red to Bob"))

		hasInfs(game, None, Alice, 2, Vector("m1"))

	test("plays into a free choice finesse"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "g3", "b3", "b3", "m2"),
			Vector("y4", "y4", "y3", "y3", "g4")
		),
			starting = Cathy,
			variant = TestVariant.Rainbow5,
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			clueTokens = 7,
			init = preClue(Alice, 2, Seq("red"))
		)
		.pipe(takeTurn("Cathy clues yellow to Bob"))

		hasStatus(game, Alice, 1, CardStatus.Finessed)
		hasInfs(game, None, Alice, 1, Vector("m1"))

	test("prompts when giver doesn't have free choice 1"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "y4", "g4", "b4", "m2"),
			Vector("r4", "y4", "g4", "b4", "m4")
		),
			starting = Cathy,
			variant = TestVariant.Rainbow5,
			clueTokens = 7,
			init = preClue(Alice, 3, Seq("red"))
		)

		// Since any clue will touch more than just the rainbow card,
		// Cathy doesn't have free choice and they all prompt Alice.
		Seq("red", "yellow", "green", "blue").foreach: clue =>
			val hypo = takeTurn(s"Cathy clues $clue to Bob")(game)

			hasStatus(hypo, Alice, 1, CardStatus.None)
			hasInfs(hypo, None, Alice, 3, Vector("m1"))

	test("prompts when giver doesn't have free choice 2"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "g4", "g4", "m2"),
			Vector("y4", "y4", "b4", "b4", "m4")
		),
			starting = Cathy,
			variant = TestVariant.Rainbow5,
			clueTokens = 7,
			init = preClue(Alice, 3, Seq("red"))
		)

		// Although yellow and blue only touch the rainbow card,
		// red doesn't, so Cathy doesn't have free choice.
		Seq("red", "yellow", "green", "blue").foreach: clue =>
			val hypo = takeTurn(s"Cathy clues $clue to Bob")(game)

			hasStatus(hypo, Alice, 1, CardStatus.None)
			hasInfs(hypo, None, Alice, 3, Vector("m1"))

	test("prompts when giver doesn't use free choice"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b5", "b4", "g4", "g4", "m2"),
			Vector("y4", "y4", "r4", "r4", "m4")
		),
			starting = Cathy,
			variant = TestVariant.Rainbow5,
			clueTokens = 7,
			init = preClue(Alice, 3, Seq("red"))
		)
		.pipe(takeTurn(s"Cathy clues blue to Bob"))

		// Although red and yellow only touch the rainbow card,
		// Cathy's clue touches non-rainbow cards, so it's not a free choice finesse.
		hasStatus(game, Alice, 1, CardStatus.None)
		hasInfs(game, None, Alice, 3, Vector("m1"))
