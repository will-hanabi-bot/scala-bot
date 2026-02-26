package tests

import scala_bot.basics._
import scala_bot.test.{hasPoss, Player, setup, takeTurn, TestVariant}, Player._
import scala_bot.reactor.Reactor

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger, LogLevel}

class Variants extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands prism touch"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g2", "b1", "r2", "r3", "g5"),
		),
			starting = Bob,
			variant = TestVariant.Prism5
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 1)"))
		.tap: g =>
			hasPoss(g, None, Alice, 1, Vector("r1", "r2", "r3", "r4", "r5", "i1", "i5"))
		.pipe(takeTurn("Alice clues blue to Bob"))
		.pipe(takeTurn("Bob clues green to Alice (slot 2)"))

		hasPoss(game, None, Alice, 2, Vector("g1", "g2", "g3", "g4", "g5", "i3"))

	test("doesn't clue prism"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g2", "b1", "r2", "r3", "g5"),
		),
			starting = Bob,
			variant = TestVariant.Prism5
		)

		assert(!game.state.allValidClues(Bob.ordinal).exists(clue =>
			clue.kind == ClueKind.Colour && clue.value == 4))
