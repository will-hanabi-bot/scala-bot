package tests.reactor

import cats.effect.unsafe.implicits.global

import scala_bot.basics._
import scala_bot.test.{Colour, hasInfs, hasStatus, Player, preClue, setup, takeTurn, TestVariant}, Player._
import scala_bot.reactor.Reactor

import scala_bot.utils.pipe
import scala_bot.logger.{Logger, LogLevel}

class Orange extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("calls the reacter's orange playable to discard"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "o1", "b4", "b4", "r4"),
			Vector("g1", "g4", "r5", "g4", "y4")
		),
			variant = TestVariant.Orange5,
			clueTokens = 7
		)

		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Colour(Cathy.ordinal, Colour.Red.ordinal))

	test("calls the receiver's orange playable to discard"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "o1", "b4", "b4", "r4"),
			Vector("r4", "g4", "r5", "g4", "y4")
		),
			starting = Cathy,
			variant = TestVariant.Orange5,
			clueTokens = 7
		)
		.pipe(takeTurn("Cathy clues red to Bob"))

		// Alice should play slot 3.
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Play(game.state.hands(Alice.ordinal)(2)))

	test("gives the correct clue to a receiver with known orange"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "g4", "g1", "b4", "r4"),
			Vector("b4", "g4", "r5", "o1", "y4")
		),
			variant = TestVariant.Orange5,
			clueTokens = 7,
			init = preClue(Cathy, 4, Seq("orange"))
		)

		// Alice should clue green to Cathy (Bob play + Cathy discard).
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Colour(Cathy.ordinal, Colour.Green.ordinal))

	test("understands when an orange card is used to react"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "r4", "b4", "b4", "r4"),
			Vector("g4", "o1", "r5", "g4", "y4")
		),
			starting = Bob,
			variant = TestVariant.Orange5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 4)"))
		.pipe(takeTurn("Cathy plays o1", "y4"))

		hasStatus(game, Alice, 2, CardStatus.CalledToPlay)

	test("drags a known orange to the play stacks after being called to play"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "r3", "g3", "g4", "r4"),
			Vector("b4", "y4", "r5", "g4", "r4")
		),
			starting = Bob,
			variant = TestVariant.Orange5,
			clueTokens = 7,
			init = preClue(Alice, 2, Seq("orange"))
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 4)"))
		.pipe(takeTurn("Cathy discards y4", "y4"))

		// hasStatus(game, Alice, 2, CardStatus.CalledToPlay)
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Play(game.state.hands(Alice.ordinal)(1)))

	test("drags an orange target of a ref play to the play stacks"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "y4", "g4", "b4", "o4"),
			Vector("r4", "y4", "g4", "b4", "o4")
		),
			starting = Cathy,
			variant = TestVariant.Orange5,
			clueTokens = 7
		)
		.pipe(takeTurn("Cathy clues orange to Alice (slots 1,2)"))

		hasStatus(game, Alice, 3, CardStatus.None)
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Play(game.state.hands(Alice.ordinal)(0)))

	test("always drags a trash pushed card to the play stacks"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "y1", "g1", "b1", "o1"),
			Vector("r1", "y1", "g1", "b1", "o1")
		),
			starting = Cathy,
			variant = TestVariant.Orange5,
			playStacks = Some(Vector(5, 5, 5, 5, 2)),
			clueTokens = 7
		)
		.pipe(takeTurn("Cathy clues 1 to Alice (slots 2,3)"))

		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Play(game.state.hands(Alice.ordinal)(0)))

	test("drags the target of an orange tempo clue to the discard stacks"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "y1", "g1", "b1", "o1"),
			Vector("r1", "y1", "g1", "b1", "o1")
		),
			starting = Cathy,
			variant = TestVariant.Orange5,
			playStacks = Some(Vector(5, 5, 5, 5, 2)),
			clueTokens = 7,
			init = preClue(Alice, 5, Vector("orange"))
		)
		.pipe(takeTurn("Cathy clues orange to Alice (slot 5)"))

		hasInfs(game, None, Alice, 5, Vector("o3"))
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Discard(game.state.hands(Alice.ordinal)(4)))
