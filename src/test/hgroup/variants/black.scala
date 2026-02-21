package tests.hgroup

import scala_bot.basics._
import scala_bot.test.{hasInfs, hasStatus, Player, preClue, setup, takeTurn, TestVariant}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.pipe
import scala_bot.logger.{Logger, LogLevel}

class Black extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands k3/4 save with black"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g4")
		),
			starting = Bob,
			variant = TestVariant.Black5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues black to Alice (slot 5)"))

		hasInfs(game, None, Alice, 5, Vector("k1", "k3", "k4"))

	test("doesn't save k2 with black"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "k2")
		),
			variant = TestVariant.Black5,
			clueTokens = 7
		)
		.pipe(takeTurn("Alice clues black to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("doesn't save k5 with black"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "k5")
		),
			variant = TestVariant.Black5,
			clueTokens = 7
		)
		.pipe(takeTurn("Alice clues black to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("relaxes save restrictions when multiple cards are newly clued"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g4")
		),
			starting = Bob,
			variant = TestVariant.Black5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues black to Alice (slots 4,5)"))

		hasInfs(game, None, Alice, 5, Vector("k1", "k2", "k3", "k4", "k5"))

	test("doesn't relax save restrictions when only 1 card is newly clued"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g4")
		),
			starting = Bob,
			variant = TestVariant.Black5,
			clueTokens = 7,
			init = preClue(Alice, 5, Seq("black"))
		)
		.pipe(takeTurn("Bob clues black to Alice (slots 4,5)"))

		hasInfs(game, None, Alice, 4, Vector("k1", "k3", "k4"))

	test("relaxes save restrictions when filling in"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g4")
		),
			starting = Bob,
			variant = TestVariant.Black5,
			clueTokens = 7,
			init = preClue(Alice, 5, Seq("5"))
		)
		.pipe(takeTurn("Bob clues black to Alice (slots 4,5)"))

		hasInfs(game, None, Alice, 4, Vector("k1", "k2", "k3", "k4"))

	test("relaxes save restrictions when avoiding bad touch"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "b4", "b4", "r2", "k2"),
			Vector("y3", "y3", "y4", "y4", "g4"),
		),
			starting = Cathy,
			variant = TestVariant.Black5,
			playStacks = Some(Vector(4, 0, 0, 0, 0)),
			clueTokens = 7
		)
		.pipe(takeTurn("Cathy clues black to Bob"))

		hasStatus(game, Alice, 1, CardStatus.None)
		assertEquals(game.lastMove, Some(ClueInterp.Save))

	test("doesn't relax save restrictions when no bad touch"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "b4", "b4", "r3", "k2"),
			Vector("y3", "y3", "y4", "y4", "g4"),
		),
			starting = Cathy,
			variant = TestVariant.Black5,
			playStacks = Some(Vector(4, 0, 0, 0, 0)),
			clueTokens = 7
		)
		.pipe(takeTurn("Cathy clues black to Bob"))

		hasStatus(game, Alice, 1, CardStatus.Finessed)
