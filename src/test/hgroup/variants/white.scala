package tests.hgroup

import scala_bot.basics._
import scala_bot.test.{hasStatus, Player, preClue, setup, takeTurn, TestVariant}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.pipe
import scala_bot.logger.{Logger, LogLevel}

class White extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands loaded 3 play clues"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "r4")
		),
			starting = Bob,
			variant = TestVariant.White5,
			discarded = Vector("w3"),
			clueTokens = 7,
			init = preClue(Alice, 5, Seq("1"))
		)
		.pipe(takeTurn("Bob clues 3 to Alice (slot 4)"))

		// Since Alice is loaded, this finesses slot 1.
		hasStatus(game, Alice, 1, CardStatus.Finessed)
