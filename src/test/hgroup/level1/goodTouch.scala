package tests.hgroup.level1

import scala_bot.test.{hasInfs, Player, setup, takeTurn}, Player._
import scala_bot.hgroup.HGroup
import scala_bot.logger.{Logger,LogLevel}

import scala.util.chaining.scalaUtilChainingOps

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
