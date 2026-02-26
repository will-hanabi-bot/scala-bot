package tests.hgroup

import scala_bot.test.{hasInfs, Player, setup, takeTurn, TestVariant}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.pipe
import scala_bot.logger.{Logger, LogLevel}

class Prism extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands prism save with colour"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "r4"),
		),
			starting = Bob,
			variant = TestVariant.Prism5,
			discarded = Vector("i3"),
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues green to Alice (slot 5)"))

		hasInfs(game, None, Alice, 5, Vector("g1", "i3"))

	test("understands prism saves aren't all ranks"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "r4"),
		),
			starting = Bob,
			variant = TestVariant.Prism5,
			discarded = Vector("i3"),
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues blue to Alice (slot 5)"))

		hasInfs(game, None, Alice, 5, Vector("b1"))
