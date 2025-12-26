package tests.refSieve.finesses

import scala_bot.refSieve.RefSieve
import scala_bot.basics._
import scala_bot.test.{Colour, hasInfs, Player, preClue, setup, takeTurn, TestClue}, Player._
import scala_bot.logger.{Logger, LogLevel}

import scala.util.chaining.scalaUtilChainingOps

class Finesses extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("recognizes a finesse via ref play"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "r4", "y4", "y4", "g4"),
			Vector("b2", "b4", "r2", "p4", "g4")
		))
		.pipe(takeTurn("Alice clues blue to Cathy"))

		// Bob's b1 should be finessed.
		hasInfs(game, None, Bob, 1, Vector("b1"))
		assertEquals(game.meta(game.state.hands(Bob.ordinal)(0)).status, CardStatus.Finessed)

	test("recognizes a finesse via fill-in"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "b4", "b4", "y4", "g4"),
			Vector("b2", "r4", "y4", "p4", "g4")
		),
			clueTokens = 7,
			init = preClue(Cathy, 1, Vector(TestClue(ClueKind.Colour, Colour.Blue.ordinal, Alice)))
		)
		.pipe(takeTurn("Alice clues 2 to Cathy"))

		// Bob's b1 should be finessed.
		hasInfs(game, None, Bob, 1, Vector("b1"))
		assertEquals(game.meta(game.state.hands(Bob.ordinal)(0)).status, CardStatus.Finessed)

	test("plays into an unknown finesse"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r2", "r4", "g4", "p4", "g4"),
			Vector("y4", "y4", "b4", "r4", "p4")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues red to Bob"))

		// We should be finessed in slot 1 for r1.
		hasInfs(game, None, Alice, 1, Vector("r1"))
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(0)).status, CardStatus.Finessed)

	test("doesn't play into a satisfied finesse"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "y4", "b4", "p4"),
			Vector("r2", "y4", "g4", "r4"),
			Vector("g5", "b4", "r3", "y3")
		),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues yellow to Cathy"))		// finessing Bob's r1

		// We shouldn"t be finessed in slot 1 for r1.
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(0)).status, CardStatus.None)

	test("writes the correct notes on potential finesses"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y4", "r4", "g4", "p4", "g4"),
			Vector("r1", "y4", "b4", "r4", "p4")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues green to Alice (slot 2)"))
		.tap { g =>
			// Alice's slot 1 could be any non-green 1, or r2.
			hasInfs(g, None, Alice, 1, Vector("r1", "r2", "y1", "b1", "p1"))
		}
		.pipe(takeTurn("Cathy discards r1", "g5"))

		// Alice's slot 1 can be any non-green 1.
		hasInfs(game, None, Alice, 1, Vector("r1", "y1", "b1", "p1"))

	test("recognizes a finesse via fill-in"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "r4", "r4", "p4", "g4"),
			Vector("r2", "y4", "b4", "g4", "p4")
		),
			clueTokens = 7,
			init =
				// Cathy's slot 1 is clued with red.
				preClue(Cathy, 1, Vector(TestClue(ClueKind.Colour, Colour.Red.ordinal, Alice)))
		)
		.pipe(takeTurn("Alice clues 2 to Cathy"))

		// Bob's slot 1 should be finessed.
		hasInfs(game, None, Bob, 1, Vector("r1"))
		assertEquals(game.meta(game.state.hands(Bob.ordinal)(0)).status, CardStatus.Finessed)

	test("plays into a finesse via fill-in"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r2", "r4", "r4", "p4", "g4"),
			Vector("y4", "y4", "b4", "g4", "p4")
		),
			starting = Player.Cathy,
			clueTokens = 7,
			init =
				// Bob's slot 1 is clued with red.
				preClue(Bob, 1, Vector(TestClue(ClueKind.Colour, Colour.Red.ordinal, Alice)))
		)
		.pipe(takeTurn("Cathy clues 2 to Bob"))

		// Alice's slot 1 should be finessed.
		hasInfs(game, None, Alice, 1, Vector("r1"))
		assertEquals(game.meta(game.state.hands(Alice.ordinal)(0)).status, CardStatus.Finessed)

	test("understands a prompt + finesse"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "y4", "b4", "p4"),
			Vector("r2", "y4", "g4", "r4"),
			Vector("g5", "b4", "r3", "y3")
		),
			init =
				// Bob's slot 1 is clued with red.
				preClue(Bob, 1, Vector(TestClue(ClueKind.Colour, Colour.Red.ordinal, Alice)))
		)
		.pipe(takeTurn("Alice clues yellow to Donald"))

		// Bob's slot 1 should become known r1.
		hasInfs(game, None, Bob, 1, Vector("r1"))

		// Cathy's slot 1 should be finessed.
		hasInfs(game, None, Cathy, 1, Vector("r2"))
		assertEquals(game.meta(game.state.hands(Cathy.ordinal)(0)).status, CardStatus.Finessed)

	test("understands a prompt + finesse"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "y4", "b4", "p4"),
			Vector("r2", "y4", "g4", "r4"),
			Vector("g5", "b4", "r3", "y3")
		),
			init =
				// Bob's slot 1 is clued with red.
				preClue(Bob, 1, Vector(TestClue(ClueKind.Colour, Colour.Red.ordinal, Alice)))
		)
		.pipe(takeTurn("Alice clues yellow to Donald"))

		// Bob's slot 1 should become known r1.
		hasInfs(game, None, Bob, 1, Vector("r1"))

		// Cathy's slot 1 should be finessed.
		hasInfs(game, None, Cathy, 1, Vector("r2"))
		assertEquals(game.meta(game.state.hands(Cathy.ordinal)(0)).status, CardStatus.Finessed)

	test("doesn't give finesse that looks like prompt"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r2", "r5", "g5", "p2", "p4"),
			Vector("r1", "r3", "p4", "r4", "p5")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			init =
				// Bob has r5 clued with red.
				preClue(Bob, 2, Vector(TestClue(ClueKind.Colour, Colour.Red.ordinal, Cathy)))
		)
		.pipe(takeTurn("Alice clues purple to Cathy"))

		// This clue is nonsensical.
		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("understands a self-finesse"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "r2", "b4", "g4", "p4")
		),
			clueTokens = 7,
			init =
				// Bob's slot 2 is clued with red.
				preClue(Bob, 2, Vector(TestClue(ClueKind.Colour, Colour.Red.ordinal, Alice)))
		)
		.pipe(takeTurn("Alice clues 2 to Bob"))

		// Bob's slot 1 should be finessed.
		hasInfs(game, None, Bob, 1, Vector("r1"))
		assertEquals(game.meta(game.state.hands(Bob.ordinal)(0)).status, CardStatus.Finessed)

	test("doesn't give self-finesses that look direct"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "r2", "b4", "g4", "p4")
		))
		.pipe(takeTurn("Alice clues blue to Bob"))

		// This clue is nonsensical.
		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("doesn't give self-finesses that look direct 2"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "y4", "b4", "p4", "g4"),
			Vector("r2", "r3", "g4", "r4", "p4")
		))
		.pipe(takeTurn("Alice clues green to Cathy"))

		// This clue is nonsensical.
		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("doesn't give self-finesse that looks like prompt"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r2", "r5", "g5", "r3", "p4")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			clueTokens = 7,
			init =
				// Bob has r5 and r3 clued with red.
				preClue[RefSieve](Bob, 2, Vector(TestClue(ClueKind.Colour, Colour.Red.ordinal, Alice))) andThen
				preClue(Bob, 4, Vector(TestClue(ClueKind.Colour, Colour.Red.ordinal, Alice)))
		)
		.pipe(takeTurn("Alice clues 3 to Bob"))

		// This clue is nonsensical.
		assertEquals(game.lastMove, Some(ClueInterp.Mistake))
