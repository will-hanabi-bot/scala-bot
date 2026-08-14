package tests.refSieve

import cats.effect.unsafe.implicits.global

import scala_bot.basics._
import scala_bot.test.{Colour, hasStatus, Player, setup, takeTurn, TestVariant}, Player._
import scala_bot.refSieve.RefSieve

import scala_bot.utils.pipe
import scala_bot.logger.{Logger, LogLevel}

class Orange extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("calls an orange playable to discard"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "o1", "b4", "b4", "r4")
		),
			variant = TestVariant.Orange5
		)

		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Rank(Bob.ordinal, 3))

	test("calls orange unplayables to play"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("o4", "r3", "g3", "b3", "y3")
		),
			starting = Bob,
			variant = TestVariant.Orange5,
			clueTokens = 7,
		)
		.pipe(takeTurn("Bob clues blue to Alice (slot 2)"))

		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Colour(Bob.ordinal, Colour.Red.ordinal))

	test("lets a playable orange card discard"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("o1", "r3", "g3", "b3", "y3")
		),
			starting = Bob,
			variant = TestVariant.Orange5,
			clueTokens = 7,
		)
		.pipe(takeTurn("Bob clues blue to Alice (slot 2)"))

		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Play(game.state.hands(Alice.ordinal)(0)))

	test("drags an orange target of a ref play to the play stacks"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "r4")
		),
			starting = Bob,
			variant = TestVariant.Orange5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues orange to Alice (slots 1,2)"))

		hasStatus(game, Alice, 3, CardStatus.None)
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Play(game.state.hands(Alice.ordinal)(0)))

	test("gives an orange fix clue on a rank playable card"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "r1", "o1")
		),
			variant = TestVariant.Orange5,
			clueTokens = 7
		)
		.pipe(takeTurn("Alice clues 1 to Bob"))
		.pipe(takeTurn("Bob plays r1", "y4"))

		// Alice needs to prevent Bob from discarding the o1.
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Colour(Bob.ordinal, 4))

	test("drags to the play stacks even when the only playables are orange"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "r1", "y1", "y1", "g1")
		),
			starting = Bob,
			playStacks = Some(Vector(5, 5, 5, 5, 0)),
			variant = TestVariant.Orange5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 2)"))

		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Play(game.state.hands(Alice.ordinal)(0)))

	// test("discards receiving an orange finesse"):
	// 	val game = setup(RefSieve.apply, Vector(
	// 		Vector("xx", "xx", "xx", "xx", "xx"),
	// 		Vector("g4", "y4", "b4", "o4", "r4"),
	// 		Vector("o1", "g4", "y4", "b4", "r4"),
	// 	),
	// 		starting = Bob,
	// 		variant = TestVariant.Orange5,
	// 		clueTokens = 7
	// 	)
	// 	.pipe(takeTurn("Bob clues red to Alice (slot 2)"))
	// 	.pipe(takeTurn("Cathy discards o1", "r5"))

	// 	hasInfs(game, None, Alice, 1, Vector("o2"))
	// 	assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Discard(game.state.hands(Alice.ordinal)(0)))

	// test("discards into an orange finesse"):
	// 	val game = setup(RefSieve.apply, Vector(
	// 		Vector("xx", "xx", "xx", "xx", "xx"),
	// 		Vector("o2", "y4", "b4", "o4", "r4"),
	// 		Vector("g4", "g4", "y4", "b4", "r4"),
	// 	),
	// 		starting = Cathy,
	// 		variant = TestVariant.Orange5,
	// 		clueTokens = 7
	// 	)
	// 	.pipe(takeTurn("Cathy clues yellow to Bob"))

	// 	assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Discard(game.state.hands(Alice.ordinal)(0)))
	// 	hasInfs(game, None, Alice, 1, Vector("o1"))
