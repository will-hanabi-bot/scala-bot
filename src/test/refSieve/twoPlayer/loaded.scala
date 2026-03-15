package tests.refSieve.twoPlayer

import scala_bot.refSieve.RefSieve
import scala_bot.basics._
import scala_bot.test.{hasInfs, hasStatus, Player, preClue, setup, takeTurn}, Player._

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger, LogLevel}

class Loaded extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("interprets a loaded rank clue"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "b4", "y5", "y4", "b2")
		),
			clueTokens = 4,
			init = preClue(Bob, 1, Seq("1"))
		)
		.pipe(takeTurn("Alice clues 4 to Bob"))

		hasStatus(game, Bob, 5, CardStatus.CalledToPlay)

	test("receives a loaded rank clue"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b4", "y5", "y4", "b2")
		),
			starting = Bob,
			clueTokens = 4
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slots 1,2)"))
		.pipe(takeTurn("Alice plays r1 (slot 1)"))
		.pipe(takeTurn("Bob clues 4 to Alice (slots 1,4)"))
		.tap: g =>
			hasStatus(g, Alice, 5, CardStatus.CalledToPlay)
		.pipe(takeTurn("Alice plays b1 (slot 2)"))

		hasStatus(game, Alice, 5, CardStatus.CalledToPlay)
		hasInfs(game, None, Alice, 5, Vector("r2", "b2"))
