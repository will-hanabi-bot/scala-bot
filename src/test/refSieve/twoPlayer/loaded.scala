package tests.refSieve.twoPlayer

import cats.effect.unsafe.implicits.global

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

	test("interprets a loaded colour clue"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "b4", "y5", "y4", "b1")
		),
			clueTokens = 4,
			init = preClue(Bob, 5, Seq("1"))
		)
		.pipe(takeTurn("Alice clues red to Bob"))

		hasInfs(game, None, Bob, 1, Vector("r1"))

	test("receives a loaded colour clue"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b4", "y5", "y4", "b2")
		),
			starting = Bob,
			clueTokens = 4
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slots 1,2)"))
		.pipe(takeTurn("Alice plays r1 (slot 1)"))
		.pipe(takeTurn("Bob clues purple to Alice (slot 1)"))

		hasInfs(game, None, Alice, 1, Vector("p1"))

	test("discards ptd after an unknown play"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p1", "b4", "y5", "y4", "b3")
		),
			playStacks = Some(Vector(1, 1, 1, 1, 0)),
			clueTokens = 4,
			init = preClue(Alice, 5, Seq("2"))
		)
		.pipe(takeTurn("Alice clues blue to Bob"))
		.pipe(takeTurn("Bob plays p1", "b4"))
		.tap: g =>
			// Even though Alice is loaded, she has PTD because Bob couldn't know he was playing p1.
			hasStatus(g, Alice, 1, CardStatus.PermissionToDiscard)
		.pipe(takeTurn("Alice plays y2 (slot 5)"))
		.pipe(takeTurn("Bob discards b4 (slot 1)", "g1"))

		// Alice should discard slot 2 from PTD.
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Discard(game.state.hands(Alice.ordinal)(1)))

	test("doesn't write ptd when partner plays a known connector"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p1", "b4", "y5", "y4", "b3")
		),
			playStacks = Some(Vector(1, 1, 1, 1, 0)),
			clueTokens = 4,
			init = preClue(Alice, 5, Seq("2"))
		)
		.pipe(takeTurn("Alice clues 1 to Bob"))
		.pipe(takeTurn("Bob plays p1", "b4"))

		hasStatus(game, Alice, 1, CardStatus.None)

	test("loads on ptd"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p1", "b4", "y5", "y4", "b3")
		),
			clueTokens = 4
		)
		.pipe(takeTurn("Alice clues blue to Bob"))
		.pipe(takeTurn("Bob plays p1", "b1"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.PermissionToDiscard)
		.pipe(takeTurn("Alice clues yellow to Bob"))
		.pipe(takeTurn("Bob clues 5 to Alice (slot 4)"))

		// This is a LPC on slot 5.
		hasStatus(game, Alice, 5, CardStatus.CalledToPlay)

	test("revokes ptd after a clue"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p5", "r4", "r5", "r3", "b3")
		),
			clueTokens = 4
		)
		.pipe(takeTurn("Alice clues 5 to Bob"))
		.pipe(takeTurn("Bob discards r4", "b1"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.PermissionToDiscard)
		.pipe(takeTurn("Alice clues red to Bob"))
		.pipe(takeTurn("Bob clues 4 to Alice (slot 1)"))

		// Alice is called to discard slot 2 (ptd revoked).
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Discard(game.state.hands(Alice.ordinal)(1)))
