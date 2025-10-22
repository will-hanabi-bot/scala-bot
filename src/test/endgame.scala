package tests.reactor.endgame

import scala_bot.reactor.Reactor, Reactor.given
import scala_bot.basics._
import scala_bot.endgame.EndgameSolver
import scala_bot.fraction.Frac

import scala_bot.test.{fullyKnown, Player, setup}, Player._
import scala_bot.logger.{Logger, LogLevel}

class Endgame extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("it clues to start b45 endgame") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b4", "y1", "g1", "b5"),
			Vector("g1", "b1", "b1", "r5"),
			Vector("b4", "p1", "p1", "r1"),
		),
			playStacks = Some(Vector(4, 4, 5, 3, 5)),
			discarded =  Vector(
				"r2", "r3",
				"y2", "y3",
				"g2", "g3", "g4",
				"b2", "b3",
				"p2", "p3", "p4"
			),	// Missing: r1, y1, r4, y4
			init =
				fullyKnown[Reactor](Alice, 1, "y5") andThen
				fullyKnown(Bob, 1, "b4") andThen
				fullyKnown(Bob, 4, "b5") andThen
				fullyKnown(Cathy, 4, "r5") andThen
				fullyKnown(Donald, 1, "b4")
		)

		assertEquals(game.state.cardsLeft, 1)

		EndgameSolver().solve(game) match {
			case Left(msg) => throw new Exception(s"Game should be winnable! $msg")
			case Right((perform, winrate)) =>
				assertEquals(winrate, Frac.one)
				assert(perform.isClue)
		}
	}

	test("it clues to start endgame on a_double player") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g5", "y4", "g1", "r1"),
			Vector("g1", "b1", "b1", "r1"),
			Vector("y4", "p1", "p1", "y5"),
		),
			playStacks = Some(Vector(5, 3, 4, 5, 5)),
			discarded =  Vector(
				"r2", "r3",
				"y2", "y3",
				"g2", "g3",
				"b2", "b3",
				"p2", "p3", "p4"
			),	// Missing: y1, y1, r4, g4, b4
			init =
				fullyKnown[Reactor](Bob, 1, "g5") andThen
				fullyKnown(Bob, 2, "y4") andThen
				fullyKnown(Donald, 1, "y4") andThen
				fullyKnown(Donald, 4, "y5")
		)

		assertEquals(game.state.cardsLeft, 1)

		EndgameSolver().solve(game) match {
			case Left(msg) => throw new Exception(s"Game should be winnable! $msg")
			case Right((perform, winrate)) =>
				assertEquals(winrate, Frac.one)
				assert(perform.isClue)
		}
	}

	test("it plays to start simple endgame") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "r1", "y1", "b5"),
			Vector("g1", "g1", "y1", "p1"),
			Vector("b1", "b1", "p1", "r5"),
		),
			playStacks = Some(Vector(3, 5, 5, 4, 5)),
			discarded =  Vector(
					  "r3",
					  "y3", "y4",
					  "g3", "g4",
				"b2", "b3", "b4",
				"p2", "p3",	"p4"
			),	// Missing: r2, y2, b2
			init =
				fullyKnown[Reactor](Alice, 1, "r4") andThen
				fullyKnown(Alice, 2, "r4") andThen
				fullyKnown(Bob, 4, "b5") andThen
				fullyKnown(Donald, 4, "r5")
		)

		assertEquals(game.state.cardsLeft, 1)

		EndgameSolver().solve(game) match {
			case Left(msg) => throw new Exception(s"Game should be winnable! $msg")
			case Right((perform, winrate)) =>
				assertEquals(winrate, Frac.one)
				assertEquals(perform, PerformAction.Play(game.state.hands(Alice.ordinal)(0)))
		}
	}

	test("it plays to start endgame when other has dupes") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b1", "p4", "b1", "p4"),
			Vector("r1", "y1", "g1", "p1"),
			Vector("r1", "y1", "g1", "p5"),
		),
			playStacks = Some(Vector(5, 5, 5, 5, 2)),
			discarded =  Vector(
				"r2", "r3",
				"y2", "y3",
				"g2", "g3",
				"b2", "b3", "b4",
				"p2", "p3",
			),	// Missing: p1, r4, y4, g4
			init =
				fullyKnown[Reactor](Alice, 1, "p3") andThen
				fullyKnown(Bob, 2, "p4") andThen
				fullyKnown(Bob, 4, "p4") andThen
				fullyKnown(Donald, 4, "p5")
		)

		assertEquals(game.state.cardsLeft, 1)

		EndgameSolver().solve(game) match {
			case Left(msg) => throw new Exception(s"Game should be winnable! $msg")
			case Right((perform, winrate)) =>
				assertEquals(winrate, Frac.one)
				// Alice should play p3.
				assertEquals(perform, PerformAction.Play(game.state.hands(Alice.ordinal)(0)))
		}
	}

	test("it plays to start a_complex endgame where all cards are seen") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "r1", "g1", "p5", "p2"),
			Vector("g1", "b1", "r4", "r1", "g5"),
		),
			playStacks = Some(Vector(3, 5, 4, 5, 1)),
			discarded =  Vector(
				"r2", "r3",
				"y2", "y3", "y4",
				"g2", "g3", "g4",
				"b2", "b3", "b4",
				"p2", "p3"
			),	// Missing: y1, y1, p1, p1, p4
			init =
				// fullyKnown(Alice, 1, "p1")
				fullyKnown[Reactor](Alice, 2, "p3") andThen
				fullyKnown(Alice, 3, "p4") andThen
				fullyKnown(Alice, 4, "r5") andThen
				fullyKnown(Alice, 5, "r4") andThen
				fullyKnown(Bob, 4, "p5") andThen
				fullyKnown(Bob, 5, "p2") andThen
				fullyKnown(Cathy, 5, "g5")
		)

		assertEquals(game.state.cardsLeft, 4)

		// Alice plays r4 (3 left), Bob plays p2 (2 left), Cathy stalls
		// Alice plays p3 (1 left), Bob stalls, Cathy stalls
		// Alice plays p4 (0 left), Bob plays p5, Cathy plays g5
		// Alice plays r5.
		EndgameSolver().solve(game) match {
			case Left(msg) => throw new Exception(s"Game should be winnable! $msg")
			case Right((perform, winrate)) =>
				assertEquals(winrate, Frac.one)
				// Alice should play r4.
				assertEquals(perform, PerformAction.Play(game.state.hands(Alice.ordinal)(4)))
		}
	}

	test("it calculates basic winrate correctly") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "r1", "g1", "y1", "r4"),
			Vector("b1", "r1", "g1", "y1", "r5"),
		),
			playStacks = Some(Vector(2, 4, 5, 5, 5)),
			discarded =  Vector(
				"r2", "r3",
				"y2", "y3",
				"g2", "g3",
				"b2", "b3", "b4",
				"p2", "p3", "p4"
			),	// Missing: p1, p1, r4, y4, g4, y5
			clueTokens = 0,
			init =
				fullyKnown[Reactor](Alice, 5, "r3") andThen
				fullyKnown(Bob, 5, "r4") andThen
				fullyKnown(Cathy, 5, "r5")
		)

		assertEquals(game.state.cardsLeft, 2)

		EndgameSolver(monteCarlo = false).solve(game) match {
			case Left(msg) => throw new Exception(s"Game should be winnable! $msg")
			case Right((perform, winrate)) =>
				// We win if Bob draws y5, and lose if Bob doesn't. There are 6 locations that y5 could be.
				assertEquals(winrate, Frac(1, 6))
				// Alice should play r3.
				assertEquals(perform, PerformAction.Play(game.state.hands(Alice.ordinal)(4)))
		}
	}
