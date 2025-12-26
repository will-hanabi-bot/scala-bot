package tests.empathy

import scala_bot.reactor.Reactor
import scala_bot.basics._
import scala_bot.test.{hasPoss, Player, preClue, setup, takeTurn}, Player._
import scala_bot.logger.{Logger, LogLevel}
import scala.util.chaining.scalaUtilChainingOps

class Empathy extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("it fails impossible setup"):
		intercept[IllegalArgumentException]:
			setup(Reactor.apply, Vector(
				Vector("xx", "xx", "xx", "xx", "xx"),
				Vector("r1", "r1", "r1", "r1", "r1")
			))

	test("it elims from count"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "r1", "r1", "r2", "r2")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 5)"))

		hasPoss(game, Some(Alice), Alice, 5, Vector("r3", "r4", "r5"))

	test("it visibly elims 5s"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx"),		// p5, t5 in slots 3 and 4
			Vector("g2", "b1", "r1", "g5"),
			Vector("g3", "p1", "b3", "b5"),
			Vector("r3", "b2", "r1", "y5")
		),
			starting = Donald,
			playStacks = Some(Vector(5, 0, 0, 0, 0, 0)),
			variant = "6 Suits"
		)
		.pipe(takeTurn("Donald clues green to Alice (slot 1)"))
		.pipe(takeTurn("Alice clues 5 to Bob"))
		.pipe(takeTurn("Bob clues 5 to Cathy"))
		.pipe(takeTurn("Cathy clues 5 to Donald"))
		.pipe(takeTurn("Donald clues 5 to Alice (slots 3,4)"))

		hasPoss(game, None, Alice, 3, Vector("p5", "t5"))
		hasPoss(game, None, Alice, 4, Vector("p5", "t5"))
		hasPoss(game, None, Bob, 4, Vector("g5"))
		hasPoss(game, None, Cathy, 4, Vector("b5"))
		hasPoss(game, None, Donald, 4, Vector("y5"))

	test("it visibly elims mixed cards"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g2", "b1", "r4", "r5", "y3"),
			Vector("y5", "p1", "b3", "b5", "g3")
		),
			starting = Alice,
			playStacks = Some(Vector(3, 0, 0, 0, 0)),
			discarded = Vector("r1", "r1", "r2", "r3"),
			init =
				preClue[Reactor](Alice, 5, Seq("red")) andThen
				preClue[Reactor](Bob, 3, Seq("red")) andThen
				preClue[Reactor](Bob, 4, Seq("red"))
		)

		// Everyone knows that Alice's card is known r4.
		hasPoss(game, None, Alice, 5, Vector("r4"))

		// Bob's cards could be r4 or r5.
		hasPoss(game, None, Bob, 3, Vector("r4", "r5"))
		hasPoss(game, None, Bob, 4, Vector("r4", "r5"))
