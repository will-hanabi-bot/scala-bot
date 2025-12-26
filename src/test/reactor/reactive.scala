package tests.reactor.reactive

import scala_bot.reactor.Reactor
import scala_bot.basics._
import scala_bot.test.{Colour, hasInfs, Player, preClue, setup, takeTurn, TestClue}, Player._
import scala_bot.logger.{Logger, LogLevel}
import scala.util.chaining.scalaUtilChainingOps

class Reactive extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("it understands a reactive play play"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "g2", "r2", "r3", "g5"),
			Vector("g1", "b5", "p2", "b1", "g4"),
		))
		.pipe(takeTurn("Alice clues 5 to Cathy"))

		.tap { g =>
			assertEquals(g.meta(g.state.hands(Bob.ordinal)(0)).status, CardStatus.CalledToPlay)
			hasInfs(g, None, Bob, 1, Vector("r1", "y1", "b1", "p1"))
		}
		.pipe(takeTurn("Bob plays b1", "p1"))

		assertEquals(game.meta(game.state.hands(Cathy.ordinal)(0)).status, CardStatus.CalledToPlay)
		hasInfs(game, None, Cathy, 1, Vector("r1", "y1", "g1", "b2", "p1"))

	test("it reacts to a reactive play play"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "g2", "r2", "r3", "g5"),
			Vector("g1", "b5", "p2", "b1", "g4"),
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 2 to Bob"))

		assertEquals(game.meta(game.state.hands(Alice.ordinal)(0)).status, CardStatus.CalledToPlay)
		hasInfs(game, None, Alice, 1, Vector("r1", "y1", "g1", "p1"))

		assertEquals(game.takeAction, PerformAction.Play(game.state.hands(Alice.ordinal)(0)))

	test("it receives a reactive play play"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "g2", "r2", "r3", "g5"),
			Vector("g1", "b5", "p2", "b1", "g4"),
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 4 to Alice (slot 3)"))
		.pipe(takeTurn("Cathy plays g1", "y3"))

		// Alice's slot 2 is called to play.
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(1)).status, CardStatus.CalledToPlay)
		hasInfs(game, None, Alice, 2, Vector("r1", "y1", "g2", "b1", "p1"))

	test("it reacts to a reverse reactive play play"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "b2", "r2", "r3", "g5"),
			Vector("g1", "b5", "p2", "g2", "g4"),
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues red to Bob"))
		.pipe(takeTurn("Alice discards r3 (slot 2)"))		// Bob's b1 is called to play
		.pipe(takeTurn("Bob clues blue to Cathy"))

		.pipe(takeTurn("Cathy plays g1", "y3"))
		.pipe(takeTurn("Alice clues 1 to Bob"))			// Reverse reactive, getting Cathy's g2 and Bob's b2 (4 + 2 = 1)

		.tap { g =>
			assertEquals(g.meta(g.state.hands(Cathy.ordinal)(3)).status, CardStatus.CalledToPlay)
		}
		.pipe(takeTurn("Bob plays b1", "y5"))

		hasInfs(game, None, Cathy, 4, Vector("r1", "y1", "g2", "p1"))

	test("it elims a reactive play play"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "g2", "r2", "y3", "g5"),
			Vector("g3", "b5", "p2", "b1", "g4")
		),
			init =
				// Bob's r2 is clued with red.
				preClue[Reactor](Bob, 3, Vector(TestClue(ClueKind.Colour, Colour.Red.ordinal, Alice))) andThen
				// Bob's y3 is clued with 3.
				preClue(Bob, 4, Vector(TestClue(ClueKind.Rank, 3, Alice)))
		)
		.pipe(takeTurn("Alice clues 4 to Cathy"))
		.tap { g =>
			assertEquals(g.meta(g.state.hands(Bob.ordinal)(0)).status, CardStatus.CalledToPlay)
		}
		.pipe(takeTurn("Bob plays r1", "p1"))

		val (common, state) = (game.common, game.state)

		assertEquals(game.meta(state.hands(Cathy.ordinal)(3)).status, CardStatus.CalledToPlay)

		// Since Bob cannot play a known 3, Cathy can't write !playable on slot 1.
		assert(Seq("r1", "y1", "g1", "b1", "p1").map(state.expandShort).forall(common.thoughts(state.hands(Cathy.ordinal)(0)).inferred.contains))

		// Since Bob cannot play r1 onto r1, Cathy can't write !r1 on slot 2.
		assert(Seq("y1", "g1", "b1", "p1").map(state.expandShort).forall(!common.thoughts(state.hands(Cathy.ordinal)(1)).inferred.contains(_)))
		assert(common.thoughts(state.hands(Cathy.ordinal)(1)).inferred.contains(state.expandShort("r1")))

		// Bob can play slot 2, so Cathy can write !playable on slot 3.
		assert(Seq("r1", "y1", "g1", "b1", "p1").map(state.expandShort).forall(!common.thoughts(state.hands(Cathy.ordinal)(2)).inferred.contains(_)))

	test("it understands a reactive dc play"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "g2", "r2", "r3", "g5"),
			Vector("g1", "b5", "p2", "b1", "g4"),
		),
			clueTokens = 7
		)
		.pipe(takeTurn("Alice clues blue to Cathy"))
		.tap { g =>
			assertEquals(g.meta(g.state.hands(Bob.ordinal)(0)).status, CardStatus.CalledToDiscard)
			// hasInfs(game, None, Bob, 1, Vector("r1", "y1", "b1", "p1"))
			assert(g.common.thinksTrash(g, Bob.ordinal).contains(g.state.hands(Bob.ordinal)(0)))
		}
		.pipe(takeTurn("Bob discards r3 (slot 1)", "p3"))

		assertEquals(game.meta(game.state.hands(Cathy.ordinal)(0)).status, CardStatus.CalledToPlay)
		hasInfs(game, None, Cathy, 1, Vector("r1", "y1", "g1", "p1"))

	test("it elims a reactive dc play"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "g2", "r2", "g5", "y3"),
			Vector("b3", "b5", "p2", "b1", "g4"),
		),
			playStacks = Some(Vector(1, 1, 1, 0, 0)),
			clueTokens = 7,
			init =
				// Bob's r2 is clued with red.
				preClue[Reactor](Bob, 3, Vector(TestClue(ClueKind.Colour, Colour.Red.ordinal, Alice))) andThen
				// Bob's g5 is clued with 5.
				preClue(Bob, 4, Vector(TestClue(ClueKind.Rank, 5, Alice)))
		)
		.pipe(takeTurn("Alice clues green to Cathy"))
		.tap { g =>
			assertEquals(g.meta(g.state.hands(Bob.ordinal)(0)).status, CardStatus.CalledToDiscard)
		}
		.pipe(takeTurn("Bob discards b1", "b4"))

		val (common, state) = (game.common, game.state)

		assertEquals(game.meta(game.state.hands(Cathy.ordinal)(3)).status, CardStatus.CalledToPlay)

		// Since Bob cannot discard a known 5, Cathy can't write !playable on slot 1.
		assert(Seq("r1", "r2", "y1", "b1").map(state.expandShort).forall(common.thoughts(state.hands(Cathy.ordinal)(0)).inferred.contains))

		// Bob can discard the other slots, so Cathy can write !playable on slots 2 and 3.
		assert(Seq("r2", "y2", "g2", "b1", "p1").map(state.expandShort).forall(!common.thoughts(state.hands(Cathy.ordinal)(1)).inferred.contains(_)))
		assert(Seq("r2", "y2", "g2", "b1", "p1").map(state.expandShort).forall(!common.thoughts(state.hands(Cathy.ordinal)(2)).inferred.contains(_)))

	test("it elims a reactive play dc"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "g2", "r2", "g5", "y3"),
			Vector("b1", "b5", "p2", "b3", "g4"),
		),
			playStacks = Some(Vector(1, 1, 1, 0, 0)),
			clueTokens = 7,
			starting = Bob,
			init =
				// Alice's slots 2 and 3 are known red.
				preClue[Reactor](Alice, 2, Vector(TestClue(ClueKind.Colour, Colour.Red.ordinal, Cathy))) andThen
				preClue(Alice, 3, Vector(TestClue(ClueKind.Colour, Colour.Red.ordinal, Cathy)))
		)
		.pipe(takeTurn("Bob clues green to Alice (slot 4)"))
		.pipe(takeTurn("Cathy plays b1", "r4"))

		assertEquals(game.meta(game.state.hands(Alice.ordinal)(2)).status, CardStatus.CalledToDiscard)

		// Alice's slot 1 can still be trash, since Cathy targeted clued trash.
		assert(game.common.thoughts(game.state.hands(Alice.ordinal)(0)).inferred.exists(game.state.isBasicTrash))

		// However, ALice's slot 2 is known !trash.
		assert(game.common.thoughts(game.state.hands(Alice.ordinal)(1)).inferred.forall(!game.state.isBasicTrash(_)))

	test("it reacts to a reactive finesse"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b2", "g2", "r2", "r3", "g5"),
			Vector("g1", "b5", "p2", "b1", "g4"),
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 3 to Bob"))

		// We should play slot 1 to target Bob's r2.
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(0)).status, CardStatus.CalledToPlay)
		hasInfs(game, None, Alice, 1, Vector("r1"))

		assertEquals(game.takeAction, PerformAction.Play(game.state.hands(Alice.ordinal)(0)))

	test("it doesnt play target an unclued dupe"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "g2", "r2", "r3", "g5"),
			Vector("g1", "b5", "p2", "b1", "g4"),
		),
			starting = Cathy,
			playStacks = Some(Vector(2, 0, 1, 0, 0)),
			clueTokens = 7,
			init =
				// Bob's r3 in slot 4 is clued.
				preClue(Bob, 4, Vector(TestClue(ClueKind.Colour, Colour.Red.ordinal, Alice)))
		)
		.pipe(takeTurn("Cathy clues green to Bob"))

		// We should discard slot 5 (so that Bob plays slot 2).
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(4)).status, CardStatus.CalledToDiscard)

	test("it doesnt play target a discarding dupe"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "g2", "y3", "r3", "g5"),
			Vector("g1", "b5", "p2", "b1", "g4"),
		),
			starting = Cathy,
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Cathy clues red to Bob"))
		.pipe(takeTurn("Alice plays r2 (slot 3)"))	// Targeting discard on r3
		.tap { g =>
			assertEquals(g.meta(g.state.hands(Bob.ordinal)(0)).status, CardStatus.CalledToDiscard)
		}
		.pipe(takeTurn("Bob clues 1 to Cathy"))
		.pipe(takeTurn("Cathy clues yellow to Bob"))

		// We should discard slot 4 (so that Bob plays the non-discarding dupe in slot 4).
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(3)).status, CardStatus.CalledToDiscard)

	test("it doesnt dc target an unclued dupe"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y1", "r2", "r3", "g5"),
			Vector("g1", "b5", "p2", "b1", "g4"),
		),
			starting = Cathy,
			playStacks = Some(Vector(0, 1, 0, 0, 0)),
			init =
				// Bob's r3 in slot 4 is clued.
				preClue(Bob, 4, Vector(TestClue(ClueKind.Colour, Colour.Red.ordinal, Alice)))
		)
		.pipe(takeTurn("Cathy clues green to Bob"))

		// We should play slot 4 (so that Bob discards slot 1).
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(3)).status, CardStatus.CalledToPlay)

	test("it reacts to a sacrifice"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "b2", "y3", "p3", "g5"),
			Vector("g1", "b5", "p2", "b1", "g4"),
		),
			starting = Cathy,
			playStacks = Some(Vector(2, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Cathy clues green to Bob"))

		// We should play slot 2 (Bob discards y3 in slot 3).
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(1)).status, CardStatus.CalledToPlay)

	test("it shifts a reaction"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "g2", "r2", "r3", "g5"),
			Vector("g1", "b5", "p2", "b1", "g4"),
		),
			starting = Cathy,
			init =
				// Alice has a clued 5 in slot 3.
				preClue(Alice, 3, Vector(TestClue(ClueKind.Rank, 5, Cathy)))
		)
		.pipe(takeTurn("Cathy clues 3 to Bob"))
		.tap { g =>
			// Normally, Alice would play slot 3 -> Bob slot 1 = 4. However, slot 3 is a known 5.
			assertEquals(g.meta(g.state.hands(Alice.ordinal)(2)).status, CardStatus.None)

			// Instead, Alice should play slot 1 -> Bob slot 3 as a finesse.
			assertEquals(g.meta(g.state.hands(Alice.ordinal)(0)).status, CardStatus.CalledToPlay)
			hasInfs(g, None, Alice, 1, Vector("r1"))

			assertEquals(g.takeAction, PerformAction.Play(g.state.hands(Alice.ordinal)(0)))
		}
		.pipe(takeTurn("Alice plays r1 (slot 1)"))

		// Bob's slot 1 should still be allowed to be b1.
		assert(game.common.thoughts(game.state.hands(Bob.ordinal)(0)).inferred.contains(game.state.expandShort("b1")))

	test("it targets a self connection"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "r5", "b2", "b3", "g4"),
			Vector("g5", "g2", "r2", "b1", "y3"),
		),
			clueTokens = 7,
			starting = Cathy,
			init =
				// Bob's slot 1 is known 1.
				preClue(Bob, 1, Vector(TestClue(ClueKind.Rank, 1, Alice)))
		)
		.pipe(takeTurn("Cathy clues red to Bob"))

		// Alice's slot 4 is called to discard (4 + 3 = 2)
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(3)).status, CardStatus.CalledToDiscard)
