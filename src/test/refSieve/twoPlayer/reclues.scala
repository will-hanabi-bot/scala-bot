package tests.refSieve.twoPlayer

import scala_bot.refSieve.RefSieve
import scala_bot.basics._
import scala_bot.test.{fullyKnown, hasInfs, hasStatus, Player, preClue, setup, takeTurn}, Player._

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger, LogLevel}

class Reclues extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("interprets a fill-in bluff"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "b4", "y4", "y4", "g3")
		),
			clueTokens = 4,
			init = preClue(Bob, 5, Seq("green"))
		)
		.pipe(takeTurn("Alice clues 3 to Bob"))
		.tap: g =>
			hasStatus(g, Bob, 1, CardStatus.Finessed)
		.pipe(takeTurn("Bob plays b1", "p1"))

		hasStatus(game, Bob, 2, CardStatus.None)

	test("receives a fill-in bluff"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b4", "y4", "y4", "g4")
		),
			starting = Bob,
			clueTokens = 4,
			init = preClue(Alice, 5, Seq("green"))
		)
		.pipe(takeTurn("Bob clues 3 to Alice (slot 5)"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Finessed)
		.pipe(takeTurn("Alice plays b1 (slot 1)"))

		hasStatus(game, Alice, 2, CardStatus.None)

	test("interprets a finesse + prompt"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g1", "b4", "y4", "g2", "g3")
		),
			clueTokens = 4,
			init =
				preClue[RefSieve](Bob, 4, Seq("green")) andThen
				preClue[RefSieve](Bob, 5, Seq("green"))
		)
		.pipe(takeTurn("Alice clues 3 to Bob"))
		.tap: g =>
			hasStatus(g, Bob, 1, CardStatus.Finessed)
		.pipe(takeTurn("Bob plays g1", "p1"))

		hasInfs(game, None, Bob, 4, Vector("g2"))

	test("receives a finesse + prompt"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b4", "y4", "y4", "g4")
		),
			starting = Bob,
			clueTokens = 4,
			init =
				preClue[RefSieve](Alice, 4, Seq("green")) andThen
				preClue[RefSieve](Alice, 5, Seq("green"))
		)
		.pipe(takeTurn("Bob clues 3 to Alice (slot 5)"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Finessed)
			assertEquals(g.takeAction, PerformAction.Play(g.state.hands(Alice.ordinal)(0)))
		.pipe(takeTurn("Alice plays g1 (slot 1)"))

		hasInfs(game, None, Alice, 4, Vector("g2"))

	test("interprets a bluff over finesse"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "b4", "y4", "g2", "g3")
		),
			clueTokens = 4,
			init =
				preClue[RefSieve](Bob, 4, Seq("green")) andThen
				preClue[RefSieve](Bob, 5, Seq("green"))
		)
		.pipe(takeTurn("Alice clues 3 to Bob"))
		.tap: g =>
			hasStatus(g, Bob, 1, CardStatus.Finessed)
		.pipe(takeTurn("Bob plays b1", "p1"))

		hasInfs(game, None, Bob, 4, Vector("g1", "g2", "g4", "g5"))

	test("doesn't prompt in the wrong order"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g2", "b4", "y4", "g1", "g3")
		),
			clueTokens = 4,
			init =
				preClue[RefSieve](Bob, 4, Seq("green")) andThen
				preClue[RefSieve](Bob, 5, Seq("green"))
		)
		.pipe(takeTurn("Alice clues 3 to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("receives a bluff over finesse"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b4", "y4", "y4", "g4")
		),
			starting = Bob,
			clueTokens = 4,
			init =
				preClue[RefSieve](Alice, 4, Seq("green")) andThen
				preClue[RefSieve](Alice, 5, Seq("green"))
		)
		.pipe(takeTurn("Bob clues 3 to Alice (slot 5)"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Finessed)
			assertEquals(g.takeAction, PerformAction.Play(g.state.hands(Alice.ordinal)(0)))
		.pipe(takeTurn("Alice plays b1 (slot 1)"))

		hasInfs(game, None, Alice, 4, Vector("g1", "g2", "g4", "g5"))

	test("receives a complex finesse"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "b4", "y4", "y4", "b4")
		),
			starting = Bob,
			clueTokens = 4,
			init =
				fullyKnown[RefSieve](Alice, 3, "g1") andThen
				preClue[RefSieve](Alice, 4, Seq("green")) andThen
				preClue[RefSieve](Alice, 5, Seq("green"))
		)
		.pipe(takeTurn("Bob clues 4 to Alice (slot 5)"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Finessed)
			assertEquals(g.takeAction, PerformAction.Play(g.state.hands(Alice.ordinal)(2)))
		.pipe(takeTurn("Alice plays g1 (slot 3)"))
		.pipe(takeTurn("Bob discards g4", "r4"))
		.pipe(takeTurn("Alice plays g2 (slot 2)"))

		hasInfs(game, None, Alice, 4, Vector("g3"))

	test("prompts if all possible"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "b4", "y4", "y4", "b4")
		),
			starting = Bob,
			clueTokens = 4,
			init =
				preClue[RefSieve](Alice, 3, Seq("green")) andThen
				preClue[RefSieve](Alice, 4, Seq("green")) andThen
				preClue[RefSieve](Alice, 5, Seq("green"))
		)
		.pipe(takeTurn("Bob clues 3 to Alice (slot 5)"))

		hasInfs(game, None, Alice, 4, Vector("g1"))
		hasInfs(game, None, Alice, 3, Vector("g2"))

	test("doesn't give bad prompts"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g1", "b4", "g1", "g2", "g3")
		),
			clueTokens = 4,
			init =
				preClue[RefSieve](Bob, 3, Seq("green")) andThen
				preClue[RefSieve](Bob, 4, Seq("green")) andThen
				preClue[RefSieve](Bob, 5, Seq("green"))
		)
		.pipe(takeTurn("Alice clues 3 to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("receives a complex bluff over finesse"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "b4", "y4", "y4", "b4")
		),
			starting = Bob,
			clueTokens = 4,
			init =
				fullyKnown[RefSieve](Alice, 3, "g1") andThen
				preClue[RefSieve](Alice, 4, Seq("green")) andThen
				preClue[RefSieve](Alice, 5, Seq("green"))
		)
		.pipe(takeTurn("Bob clues 4 to Alice (slot 5)"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Finessed)
			assertEquals(g.takeAction, PerformAction.Play(g.state.hands(Alice.ordinal)(2)))
		.pipe(takeTurn("Alice plays g1 (slot 3)"))
		.pipe(takeTurn("Bob discards g4", "r4"))
		.pipe(takeTurn("Alice plays b1 (slot 2)"))

		hasInfs(game, None, Alice, 4, Vector("g2", "g3", "g5"))

	test("receives a no-info double bluff"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "b4", "y4", "y4", "b4")
		),
			starting = Bob,
			clueTokens = 4,
			init = preClue[RefSieve](Alice, 5, Seq("green"))
		)
		.pipe(takeTurn("Bob clues green to Alice (slot 5)"))

		hasStatus(game, Alice, 1, CardStatus.CalledToPlay)
		hasStatus(game, Alice, 2, CardStatus.CalledToPlay)

	test("doesn't give a bad no-info bluff"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "b4", "y4", "y4", "g4")
		),
			clueTokens = 4,
			init = preClue[RefSieve](Bob, 5, Seq("green"))
		)
		.pipe(takeTurn("Alice clues green to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))
