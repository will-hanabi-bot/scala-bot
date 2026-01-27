package tests.hgroup.level7

import scala_bot.basics._
import scala_bot.test.{hasStatus, Player, setup, takeTurn}, Player._
import scala_bot.hgroup.{DcStatus, HGroup}
import scala_bot.logger.{Logger, LogLevel}

import scala.util.chaining.scalaUtilChainingOps
import scala_bot.test.fullyKnown

class ScreamDiscards extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("performs a sdcm"):
		val game = setup(HGroup.atLevel(7), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r2", "g4", "b4", "r5"),
			Vector("g1", "b3", "r2", "y3", "p3")
		),
			starting = Cathy,
			clueTokens = 1
		)
		.pipe(takeTurn("Cathy clues red to Alice (slot 5)"))
		.tap: g =>
			// Alice should discard slot 4 as a SDCM.
			assertEquals(g.takeAction, PerformAction.Discard(g.state.hands(Alice.ordinal)(3)))
		.pipe(takeTurn("Alice discards y4 (slot 4)"))

		hasStatus(game, Bob, 5, CardStatus.ChopMoved)

	test("doesn't sdcm for non-critical, non-playable"):
		val game = setup(HGroup.atLevel(7), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r2", "g4", "b4", "g3"),
			Vector("g1", "b3", "r2", "y3", "p3")
		),
			starting = Cathy,
			clueTokens = 1
	)
		.pipe(takeTurn("Cathy clues red to Alice (slot 5)"))

		// Alice should play slot 5.
		assertEquals(game.takeAction, PerformAction.Play(game.state.hands(Alice.ordinal)(4)))

	test("doesn't sdcm when target is loaded"):
		val game = setup(HGroup.atLevel(7), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g1", "r3", "g4", "b4", "r5"),
			Vector("r4", "b3", "r2", "y3", "p3")
		),
			starting = Bob,
			clueTokens = 2
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 5)"))
		.pipe(takeTurn("Cathy clues green to Bob"))

		// Alice should play slot 5.
		assertEquals(game.takeAction, PerformAction.Play(game.state.hands(Alice.ordinal)(4)))

	test("stalls after a scream"):
		val game = setup(HGroup.atLevel(7), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r5", "r2", "g4", "b4", "b3"),
			Vector("g1", "b3", "r2", "y3", "p3")
		),
			starting = Bob,
			clueTokens = 1,
			init = _.copy(inEarlyGame = false)
		)
		.pipe(takeTurn("Bob clues green to Cathy"))
		.pipe(takeTurn("Cathy discards p3", "p4"))

		hasStatus(game, Alice, 5, CardStatus.ChopMoved)

		// Alice should 5 stall on Bob.
		assertEquals(game.takeAction, PerformAction.Rank(Bob.ordinal, 5))

	test("screams at 1 clue when the next player will become locked"):
		val game = setup(HGroup.atLevel(7), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "y5", "g5", "b5", "r5"),
			Vector("g3", "b3", "r2", "y3", "p3")
		),
			starting = Bob,
			clueTokens = 3,
			discarded = Vector("r4")
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 1)"))
		.pipe(takeTurn("Cathy clues 5 to Bob"))
		.tap: g =>
			// Alice should discard slot 5 as a SDCM.
			assertEquals(g.takeAction, PerformAction.Discard(g.state.hands(Alice.ordinal)(4)))
		.pipe(takeTurn("Alice discards y3 (slot 5)"))

		hasStatus(game, Bob, 1, CardStatus.ChopMoved)

	test("doesn't mistake a sdcm for a gen discard"):
		val game = setup(HGroup.atLevel(7), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r2", "g4", "p1", "p1"),
			Vector("g1", "b3", "r2", "y3", "r4")
		),
			starting = Bob,
			clueTokens = 1
		)
		.pipe(takeTurn("Bob clues green to Cathy"))
		.pipe(takeTurn("Cathy discards r4", "y3"))

		// Our slot 5 should be chop moved, since p1 isn't a valid gen target.
		hasStatus(game, Alice, 5, CardStatus.ChopMoved)

class ShoutDiscards extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("performs a shout discard"):
		val game = setup(HGroup.atLevel(7), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r2", "g4", "p1", "g3"),
			Vector("g1", "b3", "r2", "y3", "p3")
		),
			starting = Cathy,
			clueTokens = 2,
			playStacks = Some(Vector(1, 1, 1, 1, 0))
		)
		.pipe(takeTurn("Cathy clues 1 to Alice (slots 4,5)"))
		.tap: g =>
			// Alice should discard slot 4 as a Shout Discard.
			assertEquals(g.takeAction, PerformAction.Discard(g.state.hands(Alice.ordinal)(3)))
		.pipe(takeTurn("Alice discards y1 (slot 4)"))

		hasStatus(game, Bob, 5, CardStatus.ChopMoved)

	test("stalls after a shout discard"):
		val game = setup(HGroup.atLevel(7), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r5", "r4", "g4", "b4", "b3"),
			Vector("p1", "g1", "r4", "y3", "p3")
		),
			starting = Bob,
			clueTokens = 1,
			playStacks = Some(Vector(1, 1, 0, 1, 1)),
			init = _.copy(inEarlyGame = false)
		)
		.pipe(takeTurn("Bob clues 1 to Cathy"))
		.pipe(takeTurn("Cathy discards p1", "p4"))

		hasStatus(game, Alice, 5, CardStatus.ChopMoved)

		// Alice should 5 Stall on Bob.
		assertEquals(game.takeAction, PerformAction.Rank(Bob.ordinal, 5))

class GenDiscards extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("performs a gen discard"):
		val game = setup(HGroup.atLevel(7), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r2", "g4", "b4", "p2"),
			Vector("g1", "b3", "r2", "y3", "r5")
		),
			starting = Cathy,
			clueTokens = 1
		)
		.pipe(takeTurn("Cathy clues red to Alice (slot 5)"))
		.tap: g =>
			// Alice should discard slot 4 as a Generation Discard.
			assertEquals(g.takeAction, PerformAction.Discard(g.state.hands(Alice.ordinal)(3)))
		.pipe(takeTurn("Alice discards b4 (slot 4)"))

		// Bob's slot 5 should not be chop moved.
		hasStatus(game, Bob, 5, CardStatus.None)

	test("interprets gen over sdcm"):
		val game = setup(HGroup.atLevel(7), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r2", "g4", "b4", "r1"),
			Vector("g1", "b3", "r2", "y3", "p5")
		),
			clueTokens = 1
		)
		.pipe(takeTurn("Alice clues red to Bob"))
		.pipe(takeTurn("Bob discards b4", "g3"))
		.tap: g =>
			// Could be scream on Cathy or generation on Alice: assume scream for now
			assertEquals(g.dcStatus, DcStatus.Scream)
			hasStatus(g, Cathy, 5, CardStatus.ChopMoved)
		.pipe(takeTurn("Cathy clues 5 to Alice (slot 5)"))

		// Alice now knows it was a Generation Discard.
		hasStatus(game, Cathy, 5, CardStatus.None)

	test("doesn't gen if they can connect"):
		val game = setup(HGroup.atLevel(7), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r2", "g4", "b4", "r1"),
			Vector("r3", "b3", "r5", "y3", "b1")
		),
			starting = Cathy,
			clueTokens = 2,
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Cathy clues red to Alice (slots 4,5)"))
		.pipe(takeTurn("Alice plays r2 (slot 5)"))
		.pipe(takeTurn("Bob clues red to Cathy"))
		.pipe(takeTurn("Cathy plays r3", "p4"))

		// Alice should play slot 5 (r4 -> r5) rather than generating for Cathy.
		assertEquals(game.takeAction, PerformAction.Play(game.state.hands(Alice.ordinal)(4)))

	test("doesn't gen if next player can connect"):
		val game = setup(HGroup.atLevel(7), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r4", "r2", "i1", "g5"),
			Vector("r3", "b3", "r5", "i2"),
			Vector("r3", "y4", "b5", "g4")
		),
			starting = Donald,
			clueTokens = 1,
			playStacks = Some(Vector(0, 0, 1, 0, 0)),
			variant = "Prism (5 Suits)",
			init = fullyKnown[HGroup](Alice, 4, "g2") andThen
				fullyKnown[HGroup](Bob, 3, "i1")
		)
		// Getting i2, but could be y1 or y2.
		.pipe(takeTurn("Donald clues yellow to Cathy"))

		// Alice should play slot 4 (g2) instead of generating for Cathy.
		assertEquals(game.takeAction, PerformAction.Play(game.state.hands(Alice.ordinal)(3)))
