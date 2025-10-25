package tests.reactor.general

import scala_bot.basics._
import scala_bot.test.{Colour, fullyKnown, hasInfs, Player, preClue, setup, takeTurn, TestClue}, Player._
import scala_bot.reactor.{evalAction, getResult, Reactor}
import scala_bot.logger.{Logger,LogLevel}

import scala.util.chaining.scalaUtilChainingOps

class General extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("it understands good touch") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "g2", "r2", "r3", "g5"),
			Vector("p4", "b5", "p2", "b1", "g4"),
		),
			playStacks = Some(Vector(2, 0, 0, 0, 0)),
			starting = Cathy,
			init = fullyKnown(Bob, 1, "r4")
		)
		.pipe(takeTurn("Cathy clues red to Alice (slots 1,2)"))	// Targeting r3 in slot 1
		.pipe(takeTurn("Alice plays r3 (slot 1)"))

		// Alice's slot 2 should be r4,r5.
		hasInfs(game, None, Alice, 2, Vector("r4", "r5"))

		// Bob's slot 1 should be known r4.
		hasInfs(game, None, Bob, 1, Vector("r4"))
	}

	test("it elims from focus") {
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
	}

	test("it understands a stable clue to Cathy") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "r4", "r4", "y4", "y4"),
			Vector("g1", "g4", "g4", "b4", "b4"),
		),
			// Bob's slot 1 is clued with 1.
			init = preClue(Bob, 1, Vector(TestClue(ClueKind.Rank, 1, Alice)))
		)
		.pipe(takeTurn("Alice clues green to Cathy"))

		// Cathy is called to play g1.
		// hasInfs(game, None, Cathy, 1, Vector("g1"))
		assertEquals(game.meta(game.state.hands(Cathy.ordinal)(0)).status, CardStatus.CalledToPlay)

		// takeTurn("Bob plays b1", "p4")
	}

	test("it understands a reverse reactive clue") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "r1", "r4", "y4", "y4"),
			Vector("g4", "g1", "g4", "b4", "b4"),
		),
			clueTokens = 7,
			// Bob's slot 2 is clued with 1.
			init = preClue(Bob, 2, Vector(TestClue(ClueKind.Rank, 1, Alice)))
		)
		.pipe(takeTurn("Alice clues 4 to Bob"))
		.tap { g =>
			// Cathy is called to play g1.
			assertEquals(g.meta(g.state.hands(Cathy.ordinal)(1)).status, CardStatus.CalledToPlay)
		}
		.pipe(takeTurn("Bob plays b1", "y3"))

		assert(game.common.obviousPlayables(game, Cathy.ordinal).contains(game.state.hands(Cathy.ordinal)(1)))
	}

	test("it doesn't give a bad reverse reactive clue") {
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
	}

	test("it understands targeting dupes") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b3", "r4", "r4", "y4", "y5"),
			Vector("g4", "g1", "g4", "b4", "b4"),
		),
			starting = Cathy,
			// Bob's slots 2 and 3 are clued with red.
			init =
				preClue[Reactor](Bob, 2, Vector(TestClue(ClueKind.Colour, Colour.Red.ordinal, Alice))) andThen
				preClue(Bob, 3, Vector(TestClue(ClueKind.Colour, Colour.Red.ordinal, Alice)))
		)
		// 4 + 2 = 1
		.pipe(takeTurn("Cathy clues blue to Bob"))
		.tap { g =>
			// Alice is called to play slot 4.
			assertEquals(g.meta(g.state.hands(Alice.ordinal)(3)).status, CardStatus.CalledToPlay)
		}
		.pipe(takeTurn("Alice plays r1 (slot 4)"))

		// Bob is called to discard slot 2.
		assertEquals(game.meta(game.state.hands(Bob.ordinal)(1)).status, CardStatus.CalledToDiscard)
	}

	test("it understands a known delayed stable play") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "y5", "g4", "b4", "b4"),
			Vector("b1", "r1", "r4", "y4", "y4"),
		),
			starting = Cathy,
			playStacks = Some(Vector(0, 0, 1, 0, 0)),
			// Alice has a known r1 (slot 1) and a known g2 (slot 2).
			init =
				fullyKnown[Reactor](Alice, 1, "r1") andThen
				fullyKnown[Reactor](Alice, 2, "g2")
		)
		.pipe(takeTurn("Cathy clues yellow to Bob"))

		// Bob is called to play g3.
		assertEquals(game.meta(game.state.hands(Bob.ordinal)(0)).status, CardStatus.CalledToPlay)

		val action = game.takeAction

		// We should play g2 into it.
		assertEquals(action, PerformAction.Play(game.state.hands(Alice.ordinal)(1)))
	}

	test("it understands an unknown delayed stable play") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g2", "y5", "g4", "b4", "b4"),
			Vector("b1", "r1", "r4", "y4", "y4"),
		),
			starting = Cathy,
			// Alice's slot 1 is clued with 1.
			init =
				preClue(Alice, 1, Vector(TestClue(ClueKind.Rank, 1, Bob)))
		)
		.pipe(takeTurn("Cathy clues yellow to Bob"))

		// Bob is called to play g2.
		assertEquals(game.meta(game.state.hands(Bob.ordinal)(0)).status, CardStatus.CalledToPlay)

		val action = game.takeAction

		// We should play our 1 into it as g1.
		assertEquals(action, PerformAction.Play(game.state.hands(Alice.ordinal)(0)))
		hasInfs(game, None, Alice, 1, Vector("g1"))
	}

	test("it doesn't give a bad connecting clue") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "y1", "g1", "g2", "p2"),
			Vector("y1", "p3", "g5", "p5", "r5"),
		),
			playStacks = Some(Vector(1, 1, 1, 1, 1)),
			// Bob's slots 4 and 5 are clued with 2.
			init =
				preClue[Reactor](Bob, 4, Vector(TestClue(ClueKind.Rank, 2, Alice))) andThen
				preClue[Reactor](Bob, 5, Vector(TestClue(ClueKind.Rank, 2, Alice)))
		)

		val clue = ClueAction(
			giver = Alice.ordinal,
			target = Cathy.ordinal,
			list = Vector(12),
			clue = BaseClue(ClueKind.Colour, Colour.Green.ordinal)
		)

		val newGame = game.simulateClue(clue)
		val result = getResult(game, newGame, clue)

		assertEquals(result, -100.0)
	}

	test("it understands bob won't react if alternative is on them") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y1", "r2", "g1", "g2", "p2"),
			Vector("r3", "p4", "g5", "y4", "r4"),
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			// Cathy's g5 is clued with green.
			init = preClue(Cathy, 3, Vector(TestClue(ClueKind.Colour, Colour.Green.ordinal, Alice))),
			clueTokens = 8
		)
		// Trying to get a finesse, but Bob doesn't see another clue for Alice to give.
		.pipe(takeTurn("Alice clues 5 to Cathy"))

		assertEquals(game.meta(game.state.hands(Bob.ordinal)(1)).status, CardStatus.None)
	}

	test("it discards zcs") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y4", "r2", "g1", "g2", "p2"),
			Vector("r1", "p4", "g4", "y5", "r4"),
		),
			clueTokens = 1,
		)
		// Taking the team down to 0 clues - Bob writes ZCS on y4, Cathy writes ZCS on p4.
		.pipe(takeTurn("Alice clues 5 to Cathy"))
		.tap { game =>
			assertEquals(game.meta(game.state.hands(Bob.ordinal)(0)).status, CardStatus.ZeroClueChop)
		}
		.pipe(takeTurn("Bob plays g1", "b1"))
		.tap { game =>
			assertEquals(game.meta(game.state.hands(Cathy.ordinal)(1)).status, CardStatus.ZeroClueChop)
		}
		.pipe(takeTurn("Cathy plays r1", "y3"))
		.pipe(takeTurn("Alice discards p4 (slot 1)"))

		// Bob's chop should be slot 2.
		assertEquals(Reactor.chop(game, Bob.ordinal), Some(game.state.hands(Bob.ordinal)(1)))
	}

	test("it interprets a gd") {
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

		assertEquals(game.meta(game.state.hands(Alice.ordinal)(4)).status, CardStatus.GentlemansDiscard)
		hasInfs(game, None, Alice, 5, Vector("y1"))
		assert(game.common.obviousPlayables(game, Alice.ordinal).contains(0))
	}

	test("it interprets a sarcastic") {
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

		assertEquals(game.meta(game.state.hands(Alice.ordinal)(4)).status, CardStatus.Sarcastic)
		hasInfs(game, None, Alice, 5, Vector("r2"))
		assert(game.common.obviousPlayables(game, Alice.ordinal).contains(0))
	}

	test("it doesn't perform a bad gd") {
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y1", "r2", "g1", "g2", "p2"),
			Vector("r3", "p4", "g5", "y4", "r4"),
		),
			init =
				fullyKnown[Reactor](Alice, 5, "g1") andThen
				preClue(Bob, 3, Vector(TestClue(ClueKind.Colour, Colour.Green.ordinal, Alice))) andThen
				preClue(Bob, 4, Vector(TestClue(ClueKind.Colour, Colour.Green.ordinal, Alice))),
			clueTokens = 6
		)

		val action = DiscardAction(0, Alice.ordinal, 2, 1, failed = false)
		val result = evalAction(game, action)

		// Discarding g1 in slot 5 is a bad GD.
		assertEquals(result, -100.0)
	}
