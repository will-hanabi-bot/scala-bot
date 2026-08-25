package tests.refSieve.twoPlayer

import scala_bot.refSieve.RefSieve
import scala_bot.basics._
import scala_bot.test.{hasStatus, Player, preClue, setup, takeTurn}, Player._

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger, LogLevel}

class Mistakes extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("resets ptd after an unexpected slot 1 discard"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "y4", "g4", "b4", "p4")
		),
			clueTokens = 4,
			init = preClue(Alice, 1, Seq("1"))
		)
		.pipe(takeTurn("Alice plays r1 (slot 1)"))
		.tap: g =>
			hasStatus(g, Bob, 1, CardStatus.PermissionToDiscard)
		.pipe(takeTurn("Bob bombs p4", "b3"))
		.pipe(takeTurn("Alice discards y3 (slot 1)"))
		.tap: g =>
			hasStatus(g, Bob, 1, CardStatus.None)
			hasStatus(g, Bob, 2, CardStatus.PermissionToDiscard)
		.pipe(takeTurn("Bob discards b3", "p3"))

		// We should reset PTD on Bob's slot 2.
		hasStatus(game, Bob, 2, CardStatus.None)
