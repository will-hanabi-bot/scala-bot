package tests.reactor.general

import scala_bot.basics._
import scala_bot.test.{Colour, fullyKnown, hasInfs, hasStatus, Player, preClue, setup, takeTurn}, Player._
import scala_bot.reactor.{evalAction, Reactor}
import scala_bot.logger.{Logger,LogLevel}

import scala.util.chaining.scalaUtilChainingOps

class General extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("it elims from focus"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y4", "g2", "r2", "r3", "g5"),
			Vector("p4", "b5", "p2", "b1", "g4"),
		),
			playStacks = Some(Vector(4, 0, 0, 0, 0)),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues red to Alice (slots 1,2)"))

		// Alice's slot 1 should be known r5.
		hasInfs(game, None, Alice, 1, Vector("r5"))

		// Alice's slot 2 should be known trash.
		val hand = game.state.hands(Alice.ordinal)
		assert(game.common.thinksTrash(game, Alice.ordinal).contains(hand(1)))

	test("it understands a stable clue to Cathy"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "r4", "r4", "y4", "y4"),
			Vector("g1", "g4", "g4", "b4", "b4"),
		),
			// Bob's slot 1 is clued with 1.
			init = preClue(Bob, 1, Seq("1"))
		)
		.pipe(takeTurn("Alice clues green to Cathy"))

		// Cathy is called to play g1.
		// hasInfs(game, None, Cathy, 1, Vector("g1"))
		hasStatus(game, Cathy, 1, CardStatus.CalledToPlay)

		// takeTurn("Bob plays b1", "p4")

	test("it understands a reverse reactive clue"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "r1", "r4", "y4", "y4"),
			Vector("g4", "g1", "g4", "b4", "b4"),
		),
			clueTokens = 7,
			// Bob's slot 2 is clued with 1.
			init = preClue(Bob, 2, Seq("1"))
		)
		.pipe(takeTurn("Alice clues 4 to Bob"))
		.tap: g =>
			// Cathy is called to play g1.
			hasStatus(g, Cathy, 2, CardStatus.CalledToPlay)
		.pipe(takeTurn("Bob plays b1", "y3"))

		assert(game.common.obviousPlayables(game, Cathy.ordinal).contains(game.state.hands(Cathy.ordinal)(1)))

	test("it doesn't give a bad reverse reactive clue"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "r1", "r4", "y4", "y5"),
			Vector("y1", "g4", "g4", "b4", "b4"),
		),
			starting = Cathy,
		)
		.pipe(takeTurn("Cathy clues 5 to Bob"))
		.pipe(takeTurn("Alice plays g1 (slot 4)"))		// targeting Bob's b1
		.pipe(takeTurn("Bob clues green to Cathy"))

		.pipe(takeTurn("Cathy plays y1", "b1"))

		// We cannot give 4 to Bob as a reverse reactive, since Cathy would play b1 to react into r1 and Bob is already playing b1.
		val clue = ClueAction(
			giver = Alice.ordinal,
			target = Bob.ordinal,
			list = Vector(6, 7),
			clue = BaseClue(ClueKind.Rank, 4)
		)
		assert(evalAction(game, clue) < 8.0)

	test("it understands targeting dupes"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b3", "r4", "r4", "y4", "y5"),
			Vector("g4", "g1", "g4", "b4", "b4"),
		),
			starting = Cathy,
			// Bob's slots 2 and 3 are clued with red.
			init =
				preClue[Reactor](Bob, 2, Seq("red")) andThen
				preClue(Bob, 3, Seq("red"))
		)
		// 4 + 2 = 1
		.pipe(takeTurn("Cathy clues blue to Bob"))
		.tap: g =>
			hasStatus(g, Alice, 4, CardStatus.CalledToPlay)
		.pipe(takeTurn("Alice plays r1 (slot 4)"))

		hasStatus(game, Bob, 2, CardStatus.CalledToDiscard)

	test("it understands a known delayed stable play"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "y5", "g4", "b4", "b4"),
			Vector("b1", "r1", "r4", "y4", "y4"),
		),
			starting = Cathy,
			playStacks = Some(Vector(0, 0, 1, 0, 0)),
			init =
				fullyKnown[Reactor](Alice, 1, "r1") andThen
				fullyKnown[Reactor](Alice, 2, "g2")
		)
		.pipe(takeTurn("Cathy clues yellow to Bob"))

		hasStatus(game, Bob, 1, CardStatus.CalledToPlay)

		val action = game.takeAction

		// We should play g2 into it.
		assertEquals(action, PerformAction.Play(game.state.hands(Alice.ordinal)(1)))

	test("it understands an unknown delayed stable play"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g2", "y5", "g4", "b4", "b4"),
			Vector("b1", "r1", "r4", "y4", "y4"),
		),
			starting = Cathy,
			init = preClue(Alice, 1, Seq("1"))
		)
		.pipe(takeTurn("Cathy clues yellow to Bob"))

		hasStatus(game, Bob, 1, CardStatus.CalledToPlay)

		val action = game.takeAction

		// We should play our 1 into it as g1.
		assertEquals(action, PerformAction.Play(game.state.hands(Alice.ordinal)(0)))
		hasInfs(game, None, Alice, 1, Vector("g1"))

	test("it doesn't give a bad connecting clue"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "y1", "g1", "g2", "p2"),
			Vector("y1", "p3", "g5", "p5", "r5"),
		),
			playStacks = Some(Vector(1, 1, 1, 1, 1)),
			init =
				preClue[Reactor](Bob, 4, Seq("2")) andThen
				preClue[Reactor](Bob, 5, Seq("2"))
		)

		val clue = ClueAction(
			giver = Alice.ordinal,
			target = Cathy.ordinal,
			list = Vector(12),
			clue = BaseClue(ClueKind.Colour, Colour.Green.ordinal)
		)

		val newGame = game.simulateClue(clue, log = true)
		assertEquals(newGame.lastMove, Some(ClueInterp.Mistake))

	test("it understands bob won't react if alternative is on them"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y1", "r2", "g1", "g2", "p2"),
			Vector("r3", "p4", "g5", "y4", "r4"),
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			init = preClue(Cathy, 3, Seq("green")),
			clueTokens = 8
		)
		// Trying to get a finesse, but Bob doesn't see another clue for Alice to give.
		.pipe(takeTurn("Alice clues 5 to Cathy"))

		hasStatus(game, Bob, 2, CardStatus.None)

	test("it discards zcs"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y4", "r2", "g1", "g2", "p2"),
			Vector("r1", "p4", "g4", "y5", "r4"),
		),
			clueTokens = 1,
		)
		// Taking the team down to 0 clues - Bob writes ZCS on y4, Cathy writes ZCS on p4.
		.pipe(takeTurn("Alice clues 5 to Cathy"))
		.pipe(takeTurn("Bob plays g1", "b1"))
		.pipe(takeTurn("Cathy plays r1", "y3"))
		.pipe(takeTurn("Alice discards p4 (slot 1)"))
		.tap: g =>
			// Bob's chop should be slot 2.
			assertEquals(g.chop(Bob.ordinal), Some(g.state.hands(Bob.ordinal)(1)))
		.pipe(takeTurn("Bob discards y4", "r3"))

		assertEquals(game.zcsTurn, None)
		// Cathy's chop should be slot 1.
		assertEquals(game.chop(Cathy.ordinal), Some(game.state.hands(Cathy.ordinal)(0)))

	test("it interprets a gd"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y1", "r2", "g1", "g2", "p2"),
			Vector("r3", "p4", "g5", "y4", "r4"),
		),
			init = fullyKnown(Bob, 1, "y1"),
			starting = Bob,
			clueTokens = 6
		)
		.pipe(takeTurn("Bob discards y1", "y4"))

		hasStatus(game, Alice, 5, CardStatus.GentlemansDiscard)
		hasInfs(game, None, Alice, 5, Vector("y1"))
		assert(game.common.obviousPlayables(game, Alice.ordinal).contains(0))

	test("it interprets a sarcastic"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y1", "r2", "g1", "g2", "p2"),
			Vector("r3", "p4", "g5", "y4", "r4"),
		),
			init = fullyKnown(Bob, 2, "r2"),
			starting = Bob,
			clueTokens = 6
		)
		.pipe(takeTurn("Bob discards r2", "y4"))
		.pipe(takeTurn("Cathy clues blue to Alice (slots 2,3,4)"))
		.pipe(takeTurn("Alice plays r1 (slot 1)"))

		hasStatus(game, Alice, 5, CardStatus.Sarcastic)
		hasInfs(game, None, Alice, 5, Vector("r2"))
		assert(game.common.obviousPlayables(game, Alice.ordinal).contains(0))

	test("it doesn't perform a bad gd"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y1", "r2", "g1", "g2", "p2"),
			Vector("r3", "p4", "g5", "y4", "r4"),
		),
			init =
				fullyKnown[Reactor](Alice, 5, "g1") andThen
				preClue(Bob, 3, Vector("green")) andThen
				preClue(Bob, 4, Vector("green")),
			clueTokens = 6
		)

		val action = DiscardAction(0, Alice.ordinal, 2, 1, failed = false)
		val result = evalAction(game, action)

		// Discarding g1 in slot 5 is a bad GD.
		assertEquals(result, -100.0)
