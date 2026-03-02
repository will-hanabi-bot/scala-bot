package tests.hgroup.level1

import scala_bot.basics._
import scala_bot.test.{hasInfs, Player, setup, takeTurn}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.pipe
import scala_bot.logger.{Logger,LogLevel}

class GoodTouch extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("eliminates from focus (direct play)"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r5", "r4", "r2", "y4", "y2")
		),
			starting = Bob,
			playStacks = Some(Vector(0, 0, 0, 0, 4))
		)
		.pipe(takeTurn("Bob clues purple to Alice (slots 4,5)"))

		hasInfs(game, None, Alice, 5, Vector("p5"))
		assert(game.common.thinksTrash(game, Alice.ordinal).contains(game.state.hands(Alice.ordinal)(3)))

	test("doesn't count bad touch when focusing the last card"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r5", "r4", "r2", "r1", "p1")
		),
			playStacks = Some(Vector(1, 1, 1, 1, 0))
		)

		val hypo = takeTurn("Alice clues 1 to Bob")(game)
		val (badTouch, trash, _) = badTouchResult(game, hypo, ClueAction(Alice.ordinal, Bob.ordinal, Seq(5, 6), BaseClue(ClueKind.Rank, 1)))

		assert(badTouch.isEmpty)
		assertEquals(trash, List(6))

	test("doesn't count bad touch when focusing a dupe of the last card"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p2", "y4", "y1", "b1", "r1"),
			Vector("p3", "p3", "r2", "r4", "b4")
		),
			playStacks = Some(Vector(3, 3, 3, 3, 1))
		)

		val hypo = takeTurn("Alice clues 3 to Cathy")(game)
		val (badTouch, trash, _) = badTouchResult(game, hypo, ClueAction(Alice.ordinal, Cathy.ordinal, Seq(13, 14), BaseClue(ClueKind.Rank, 3)))

		assert(badTouch.isEmpty)
		assertEquals(trash, List(13))
