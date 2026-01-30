package tests.hgroup.level8

import scala_bot.basics._
import scala_bot.test.{fullyKnown, hasInfs, hasStatus, Player, preClue, setup, takeTurn}, Player._
import scala_bot.hgroup.HGroup
import scala_bot.logger.{Logger, LogLevel}

import scala.util.chaining.scalaUtilChainingOps

class PositionalDiscards extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("plays from a pos dc"):
		val game = setup(HGroup.atLevel(8), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "r1", "r2", "r3", "b1"),
			Vector("b1", "b2", "g1", "r4", "p1")
		),
			starting = Cathy,
			playStacks = Some(Vector(4, 4, 4, 4, 4)),
			discarded = Vector(
				"y1", "y2", "y3", "y4",
				"g1", "g2", "g3", "g4",
				            "b3", "b4",
				"p1", "p2", "p3"
			),	// Missing: r5, y1, y5, g5, b5, p4, p5
			clueTokens = 1,
			init = _.copy(inEarlyGame = false)
		)
		.tap: g =>
			assertEquals(g.state.cardsLeft, 2)
		.pipe(takeTurn("Cathy discards g1", "p4"))

		// Alice's slot 3 should be called to play from a positional discard.
		hasStatus(game, Alice, 3, CardStatus.CalledToPlay)
		assertEquals(game.takeAction, PerformAction.Play(game.state.hands(Alice.ordinal)(2)))

	test("doesn't play from a pos dc to someone after them"):
		val game = setup(HGroup.atLevel(8), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "r1", "p5", "r3", "b1"),
			Vector("b1", "b2", "g1", "r4", "p1")
		),
			starting = Cathy,
			playStacks = Some(Vector(4, 4, 4, 4, 4)),
			discarded = Vector(
				"y1", "y2", "y3", "y4",
				"g1", "g2", "g3", "g4",
				            "b3", "b4",
				"p1", "p2", "p3"
			),	// Missing: r2, r5, y1, y5, g5, b5, p4
			clueTokens = 1,
			init = _.copy(inEarlyGame = false)
		)
		.tap: g =>
			assertEquals(g.state.cardsLeft, 2)
		.pipe(takeTurn("Cathy discards g1", "p4"))

		// Bob's slot 3 should be called to play from a positional discard.
		hasStatus(game, Bob, 3, CardStatus.CalledToPlay)
		hasStatus(game, Alice, 3, CardStatus.None)

	test("doesn't play from a pos dc if someone before them plays into it"):
		val game = setup(HGroup.atLevel(8), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "r1", "r2", "r3", "b1"),
			Vector("b1", "b2", "p5", "g1", "p1")
		),
			starting = Bob,
			playStacks = Some(Vector(4, 4, 4, 4, 4)),
			discarded = Vector(
				"y1", "y2", "y3", "y4",
				"g1", "g2", "g3", "g4",
				            "b3", "b4",
				"p1", "p2", "p3"
			),	// Missing: r4, r5, y1, y5, g5, b5, p4
			clueTokens = 1,
			init = _.copy(inEarlyGame = false)
		)
		.tap: g =>
			assertEquals(g.state.cardsLeft, 2)
		.pipe(takeTurn("Bob discards r2", "p4"))
		.tap: g =>
			// Cathy's slot 3 should be called to play from a positional discard.
			hasStatus(g, Cathy, 3, CardStatus.CalledToPlay)
		.pipe(takeTurn("Cathy plays p5", "y5"))

		hasStatus(game, Alice, 3, CardStatus.None)

	test("plays into a pos dc if someone before them doesn't play into it"):
		val game = setup(HGroup.atLevel(8), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "r1", "r2", "r3", "b1"),
			Vector("b1", "b2", "p5", "g1", "p1")
		),
			starting = Bob,
			playStacks = Some(Vector(4, 4, 4, 4, 4)),
			discarded = Vector(
				"y1", "y2", "y3", "y4",
				"g1", "g2", "g3", "g4",
				            "b3", "b4",
				"p1", "p2", "p3"
			),	// Missing: r4, r5, y1, y5, g5, b5, p4
			clueTokens = 1,
			init = _.copy(inEarlyGame = false)
		)
		.tap: g =>
			assertEquals(g.state.cardsLeft, 2)
		.pipe(takeTurn("Bob discards r2", "p4"))
		.pipe(takeTurn("Cathy discards p1", "y1"))

		hasStatus(game, Alice, 3, CardStatus.CalledToPlay)

	test("doesn't play from a chop discard"):
		val game = setup(HGroup.atLevel(8), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "r1", "p5", "r3", "b1"),
			Vector("b1", "b2", "g1", "r4", "p1")
		),
			starting = Cathy,
			playStacks = Some(Vector(4, 4, 4, 4, 4)),
			discarded = Vector(
				"y1", "y2", "y3", "y4",
				"g1", "g2", "g3", "g4",
				            "b3", "b4",
				"p1", "p2", "p3"
			),	// Missing: r2, r5, y1, y5, g5, b5, p4
			clueTokens = 1,
			init = _.copy(inEarlyGame = false)
		)
		.tap: g =>
			assertEquals(g.state.cardsLeft, 2)
		.pipe(takeTurn("Cathy discards p1", "p4"))

		hasStatus(game, Alice, 5, CardStatus.None)

	test("doesn't play from a trash discard in the correct order"):
		val game = setup(HGroup.atLevel(8), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "r1", "p5", "r3", "b1"),
			Vector("b1", "b2", "g1", "r4", "p1")
		),
			starting = Cathy,
			playStacks = Some(Vector(4, 4, 4, 4, 4)),
			discarded = Vector(
				"y1", "y2", "y3", "y4",
				"g1", "g2", "g3", "g4",
				            "b3", "b4",
				"p1", "p2", "p3"
			),	// Missing: r2, r5, y1, y5, g5, b5, p4
			clueTokens = 1,
			init = (game) =>
				(preClue[HGroup](Cathy, 3, Seq("1")) andThen
				preClue[HGroup](Cathy, 5, Seq("1")))(game)
					.copy(inEarlyGame = false)
		)
		.tap: g =>
			assertEquals(g.state.cardsLeft, 2)
		.pipe(takeTurn("Cathy discards g1", "p4"))

		hasStatus(game, Alice, 3, CardStatus.None)

	test("recognizes a pos dc on the last player"):
		val game = setup(HGroup.atLevel(8), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "r1", "b5", "b1"),
			Vector("b1", "b2", "g5", "g1"),
			Vector("g1", "g3", "r3", "p4")
		),
			starting = Donald,
			playStacks = Some(Vector(4, 4, 4, 4, 5)),
			discarded = Vector(
				"y1", "y2", "y3", "y4",
				      "g2",       "g4",
				            "b3", "b4",
				"p1", "p2", "p3"
			),	// Missing: r2, r4, r5, y1, y5, p1
			clueTokens = 1,
			init = _.copy(inEarlyGame = false)
		)
		.tap: g =>
			assertEquals(g.state.cardsLeft, 2)
		.pipe(takeTurn("Donald discards r3", "r5"))

		// Cathy's slot 3 should be called to play, while Bob's should not.
		hasStatus(game, Bob, 3, CardStatus.None)
		hasStatus(game, Cathy, 3, CardStatus.CalledToPlay)

	test("performs a pos dc"):
		val game = setup(HGroup.atLevel(8), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "r1", "b5", "b1"),
			Vector("b1", "b2", "r2", "g1"),
			Vector("g1", "g3", "r3", "p4")
		),
			starting = Donald,
			playStacks = Some(Vector(5, 5, 5, 4, 5)),
			discarded = Vector(
				"y1", "y2", "y3", "y4",
				      "g2",       "g4",
				            "b3", "b4",
				"p1", "p2"
			),	// Missing: r4, y1, p1, p3
			clueTokens = 0,
			init = _.withState(_.copy(endgameTurns = Some(4)))
				.copy(inEarlyGame = false)
		)
		.tap: g =>
			assertEquals(g.state.cardsLeft, 0)

		// Alice should discard slot 3 as a positional discard.
		assertEquals(game.takeAction, PerformAction.Discard(game.state.hands(Alice.ordinal)(2)))

	test("plays from a pos dc against common good touch"):
		val game = setup(HGroup.atLevel(8), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y1", "b1", "r2", "i1"),
			Vector("b1", "r1", "g1", "g1"),
			Vector("r1", "r5", "i1", "y1")
		),
			playStacks = Some(Vector(4, 4, 5, 5, 5)),
			discarded = Vector(
				"y2", "y3", "y4",
				"g2",       "g4",
				      "b3", "b4",
				"i2", "i3"
			),	// Missing: r3, r4, y5, g3, b2, i4
			clueTokens = 2,
			variant = "Pink (5 Suits)",
			init = _.copy(inEarlyGame = false)
		)
		.tap: g =>
			assertEquals(g.state.cardsLeft, 2)
		.pipe(takeTurn("Alice clues 5 to Donald"))
		.pipe(takeTurn("Bob clues red to Donald"))
		.pipe(takeTurn("Cathy discards r1", "r3"))

		// Alice's slot 2 is called to play.
		hasStatus(game, Alice, 2, CardStatus.CalledToPlay)
		assertEquals(game.takeAction, PerformAction.Play(game.state.hands(Alice.ordinal)(1)))

	test("doesn't bomb from a mistake"):
		val game = setup(HGroup.atLevel(8), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "y4", "g4", "b5", "b4"),
			Vector("g2", "b3", "r5", "p2", "p3")
		),
			playStacks = Some(Vector(4, 5, 5, 4, 5)),
			discarded = Vector(
				"r1", "r2", "r3",
				"y1", "y2", "y3",
				"g1",       "g3",
				"b1", "b2",
				"p1"
			),	// Missing: r1, y1, g1, b1, p1, p4
			clueTokens = 1,
			init = _.copy(inEarlyGame = false)
		)
		.tap: g =>
			assertEquals(g.state.cardsLeft, 1)
		.pipe(takeTurn("Alice clues red to Cathy"))
		.pipe(takeTurn("Bob discards g4", "r1"))

		// Alice should not attempt to play.
		hasStatus(game, Alice, 3, CardStatus.None)

class PositionalMisplays extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("plays from a pos misplay"):
		val game = setup(HGroup.atLevel(8), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "r1", "r5", "r3", "b1"),
			Vector("b1", "b2", "g1", "r4", "p1")
		),
			starting = Cathy,
			playStacks = Some(Vector(4, 4, 4, 4, 4)),
			discarded = Vector(
				"y1", "y2", "y3", "y4",
				"g1", "g2", "g3", "g4",
				            "b3", "b4",
				"p1", "p2", "p3"
			),	// Missing: r2, y1, y5, g5, b5, p4, p5
			clueTokens = 0,
			init = _.copy(inEarlyGame = false)
		)
		.tap: g =>
			assertEquals(g.state.cardsLeft, 2)
		.pipe(takeTurn("Cathy bombs p1", "p4"))

		// Alice's slot 5 should be called to play from a positional misplay.
		hasStatus(game, Alice, 5, CardStatus.CalledToPlay)
		assertEquals(game.takeAction, PerformAction.Play(game.state.hands(Alice.ordinal)(4)))

	test("plays from a double pos misplay"):
		val game = setup(HGroup.atLevel(8), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "r1", "r5", "r3", "b1"),
			Vector("b1", "b2", "g1", "r4", "p1")
		),
			starting = Cathy,
			playStacks = Some(Vector(4, 4, 4, 4, 4)),
			discarded = Vector(
				"y1", "y2", "y3", "y4",
				"g1", "g2", "g3", "g4",
				            "b3", "b4",
				"p1", "p2", "p3"
			),	// Missing: r2, y1, y5, g5, b5, p4, p5
			clueTokens = 0,
			init = _.copy(inEarlyGame = false)
		)
		.tap: g =>
			assertEquals(g.state.cardsLeft, 2)
		.pipe(takeTurn("Cathy bombs g1", "p4"))

		// Alice's slot 3 should be called to play from a positional misplay.
		hasStatus(game, Alice, 3, CardStatus.CalledToPlay)
		assertEquals(game.takeAction, PerformAction.Play(game.state.hands(Alice.ordinal)(2)))

class DistributionClues extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands a distribution clue"):
		val game = setup(HGroup.atLevel(8), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),	// xx xx r5 b4 b5
			Vector("g2", "y4", "r4", "y1", "b1"),
			Vector("b4", "b3", "r1", "p2", "p3")
		),
			starting = Bob,
			playStacks = Some(Vector(4, 5, 5, 3, 5)),
			discarded = Vector(
				"r1", "r2", "r3",
				"y1", "y2", "y3",
				"g1",       "g3",
				"b1", "b2",
				"p1"
			),	// Missing: r5, g1, g4, b4, b5, p1, p4
			clueTokens = 1,
			init = (game) =>
				(fullyKnown[HGroup](Alice, 3, "r5") andThen
				fullyKnown[HGroup](Alice, 4, "b4") andThen
				fullyKnown[HGroup](Alice, 5, "b5"))(game)
				.copy(inEarlyGame = false)
		)
		.tap: g =>
			assertEquals(g.state.cardsLeft, 2)
		.pipe(takeTurn("Bob clues 4 to Cathy"))

		hasInfs(game, None, Cathy, 1, Vector("b4"))
		assertEquals(game.lastMove, Some(ClueInterp.Distribution))
