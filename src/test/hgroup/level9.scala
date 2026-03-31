package tests.hgroup.level9

import cats.effect.unsafe.implicits.global

import scala_bot.basics._
import scala_bot.test.{Colour, hasInfs, hasStatus, Player, preClue, setup, takeTurn}, Player._
import scala_bot.hgroup.{HGroup, StallInterp}

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger, LogLevel}

class Stalling extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("doesn't dda stall before level 9"):
		val game = setup(HGroup.atLevel(8), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r5", "g4", "b4", "b3", "g3"),
			Vector("y4", "y4", "r4", "r3", "p3")
		),
			starting = Cathy,
			clueTokens = 4
		)
		.pipe(takeTurn("Cathy discards p3", "g2"))
		.pipe(takeTurn("Alice clues 5 to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("doesn't dda stall in 2p"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r5", "g4", "b4", "b3", "g3")
		),
			starting = Bob,
			clueTokens = 4
		)
		.pipe(takeTurn("Bob discards g3", "p3"))
		.pipe(takeTurn("Alice clues 5 to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("understands a locked hand stall"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b4", "g4", "b4", "b3"),
			Vector("y4", "y4", "r4", "r3"),
			Vector("y5", "r5", "b5", "g5")
		),
			starting = Cathy,
			discarded = Vector("p4")
		)
		.pipe(takeTurn("Cathy clues 5 to Donald"))
		.pipe(takeTurn("Donald clues purple to Alice (slot 4)"))

		hasInfs(game, None, Alice, 4, Vector("p1", "p2", "p3", "p4", "p5"))

	test("understands a fill-in stall on cm'd cards"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b4", "g4", "b4", "b3"),
			Vector("y4", "y4", "r4", "r3"),
			Vector("y5", "r5", "b5", "g5")
		),
			starting = Bob,
			playStacks = Some(Vector(1, 1, 1, 1, 1))
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slot 2)"))
		.pipe(takeTurn("Cathy clues 5 to Donald"))
		.pipe(takeTurn("Donald clues purple to Alice (slot 3)"))

		assertEquals(game.lastMove, Some(ClueInterp.Stall))
		hasInfs(game, None, Alice, 3, Vector("p2", "p3", "p4", "p5"))

	test("understands a save can look like a stall"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b4", "g4", "b4", "b3"),
			Vector("y4", "y4", "r4", "r3"),
			Vector("y5", "r5", "b5", "g5")
		),
			starting = Cathy,
			discarded = Vector("b3")
		)
		.pipe(takeTurn("Cathy clues 5 to Donald"))
		.pipe(takeTurn("Donald clues 3 to Bob"))

		hasInfs(game, None, Bob, 4, Vector("r3", "y3", "g3", "b3", "p3"))

	test("doesn't interpret a stall when a play clue is available"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b1", "g4", "b4", "b3"),
			Vector("y4", "y4", "r4", "r3"),
			Vector("y5", "r5", "b5", "g5")
		),
			starting = Cathy,
		)
		.pipe(takeTurn("Cathy clues 5 to Donald"))
		.pipe(takeTurn("Donald clues purple to Alice (slot 4)"))

		// Can't be LHS, since b1 is available.
		hasInfs(game, None, Alice, 4, Vector("p1"))

	test("doesn't interpret a stall when a save clue is available"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y3", "g4", "b3", "b4"),
			Vector("y4", "y4", "r4", "r3"),
			Vector("y5", "r5", "b5", "g5")
		),
			starting = Cathy,
			discarded = Vector("b4", "p4")
		)
		.pipe(takeTurn("Cathy clues 5 to Donald"))
		.pipe(takeTurn("Donald clues purple to Alice (slot 4)"))

		// Can't be LHS, since b4 is available.
		hasInfs(game, None, Alice, 4, Vector("p1", "p4"))

	test("doesn't interpret a stall when loaded"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b3", "r2", "b4", "g4"),
			Vector("y4", "y4", "r4", "r3"),
			Vector("y5", "r5", "b5", "g5")
		),
			starting = Bob,
			playStacks = Some(Vector(4, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Bob clues 5 to Donald"))
		.pipe(takeTurn("Cathy clues red to Donald"))
		.pipe(takeTurn("Donald clues green to Alice (slot 4)"))

		// Can't be LHS, since Donald can play r5.
		hasInfs(game, None, Alice, 4, Vector("g1"))

	test("interprets a finesse when a play clue is available"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b1", "g4", "b4", "g2"),
			Vector("y4", "y4", "r4", "r3"),
			Vector("y5", "r5", "b5", "g5")
		),
			starting = Cathy,
		)
		.pipe(takeTurn("Cathy clues 5 to Donald"))
		.pipe(takeTurn("Donald clues green to Bob (slot 4)"))

		// Can't be LHS, since b1 is available.
		hasInfs(game, None, Alice, 1, Vector("g1"))
		hasStatus(game, Alice, 1, CardStatus.Finessed)

	test("interprets a finesse when a fill-in is available"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b3", "g4", "g2", "b4"),
			Vector("y4", "y4", "r5", "r4"),
			Vector("r2", "y2", "b5", "g5")
		),
			playStacks = Some(Vector(2, 0, 0, 0, 2)),
			discarded = Vector("r4")
		)
		.pipe(takeTurn("Alice clues 5 to Donald"))
		.pipe(takeTurn("Bob clues red to Cathy"))	// r4 save
		.pipe(takeTurn("Cathy clues 2 to Donald"))
		.pipe(takeTurn("Donald clues red to Cathy"))

		// Can't be a Hard Burn, since filling r5 is available.
		hasInfs(game, None, Alice, 1, Vector("r3"))
		hasStatus(game, Alice, 1, CardStatus.Finessed)

	test("understands a tempo clue when there are better clues available"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b3", "r1", "g2", "b4", "p4"),
			Vector("y4", "y4", "r3", "y2", "g4")
		),
			starting = Cathy,
			playStacks = Some(Vector(2, 0, 0, 0, 0)),
			discarded = Vector("p4"),
			clueTokens = 6,
			init = (game) =>
				preClue[HGroup](Cathy, 3, Seq("red"))(game)
					.copy(inEarlyGame = false)
		)
		.pipe(takeTurn("Cathy discards g4", "p2"))		// g4 dda
		.pipe(takeTurn("Alice clues red to Cathy"))		// p4 save to Bob, y2 save to Cathy

		// Cathy's red card should be known r3, and y2 should be chop moved from TCCM.
		hasInfs(game, None, Cathy, 4, Vector("r3"))
		hasStatus(game, Cathy, 5, CardStatus.ChopMoved)

	test("understands a tempo clue stall"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b3", "r1", "g2", "b4", "g3"),
			Vector("y4", "y4", "r3", "p4", "g4")
		),
			starting = Cathy,
			playStacks = Some(Vector(2, 0, 0, 0, 0)),
			clueTokens = 6,
			init = (game) =>
				preClue[HGroup](Cathy, 3, Seq("red"))(game)
					.copy(inEarlyGame = false)
		)
		.pipe(takeTurn("Cathy discards g4", "p4"))
		.pipe(takeTurn("Alice clues 3 to Cathy"))		// nothing to clue

		// Cathy's red card should be known r3, and p4 shouldn't be chop moved.
		hasInfs(game, None, Cathy, 4, Vector("r3"))
		hasStatus(game, Cathy, 5, CardStatus.None)

	test("finds all clues in a locked hand situation"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "r4", "b2", "b2", "r1"),
			Vector("y4", "y4", "r4", "r3", "g2")
		))
		.pipe(takeTurn("Alice clues 2 to Cathy"))
		.pipe(takeTurn("Bob clues 5 to Alice (slots 1,2,3,4,5)"))
		.pipe(takeTurn("Cathy clues red to Bob"))

		val fillIn1 = takeTurn("Alice clues 3 to Bob")(game)
		assertEquals(fillIn1.lastMove, Some(ClueInterp.Stall))
		assertEquals(fillIn1.stallInterp, Some(StallInterp.FillIn))

		val fillIn2 = takeTurn("Alice clues 4 to Bob")(game)
		assertEquals(fillIn2.lastMove, Some(ClueInterp.Stall))
		assertEquals(fillIn2.stallInterp, Some(StallInterp.FillIn))

		val lhs = takeTurn("Alice clues 3 to Cathy")(game)
		assertEquals(lhs.lastMove, Some(ClueInterp.Stall))
		assertEquals(lhs.stallInterp, Some(StallInterp.Locked))

		// 2 to Cathy is not a valid Hard Burn.
		val illegal = takeTurn("Alice clues 2 to Cathy")(game)
		assertEquals(illegal.lastMove, Some(ClueInterp.Mistake))

	test("gives a bad touch save in stalling situations"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "r4", "b3", "r2", "p2"),
			Vector("y5", "y4", "r4", "g2", "g4")
		),
			starting = Bob,
			playStacks = Some(Vector(2, 2, 2, 0, 0)),
			clueTokens = 4
		)
		.pipe(takeTurn("Bob clues 5 to Cathy"))
		.pipe(takeTurn("Cathy discards g4", "p4"))

		// Alice is in DDA, she should clue 2 to Bob even though it bad touches.
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Rank(Bob.ordinal, 2))

	test("5 stalls to the one closest to chop"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r2", "y4", "r5", "g2", "r3"),
			Vector("y5", "r4", "b3", "b4", "b1")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues blue to Cathy"))
		.pipe(takeTurn("Cathy clues 5 to Alice (slot 5)"))

		// Alice should 5 Stall on Cathy, since Bob's 5 is farther away from chop.
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Rank(Cathy.ordinal, 5))

	test("respects potentially having a clue in their hand when interpreting a stall"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "y4", "y1", "g4", "p4"),
			Vector("r3", "r4", "r4", "p1", "b4")
		),
			starting = Bob,
			playStacks = Some(Vector(2, 3, 0, 0, 0)),
			discarded = Vector("r3")
		)
		.pipe(takeTurn("Bob clues 5 to Alice (slots 2,3)"))	// r5 finesse (Cathy), y5 finesse (self)

		// We can see a clue to Cathy, so it can't be a 5 Stall.

		val redFinesse = takeTurn("Cathy plays r3", "r1")(game)
		hasInfs(redFinesse, None, Alice, 2, Vector("r5"))

		val yellowFinesse = takeTurn("Cathy discards b4", "r1")(game)
		hasInfs(yellowFinesse, None, Alice, 2, Vector("y5"))
		hasStatus(yellowFinesse, Alice, 1, CardStatus.Finessed)

class DoubleDiscardAvoidance extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("will give a 5 stall in dda"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r2", "y5", "b2", "g4"),
			Vector("b3", "p4", "b4", "b2"),
			Vector("y4", "b4", "r4", "r3")
		),
			starting = Donald,
			clueTokens = 0
		)
		.pipe(takeTurn("Donald discards r3", "p3"))
		.tap: g =>
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Rank(Bob.ordinal, 5))
		.pipe(takeTurn("Alice clues 5 to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Stall))

	test("will discard in dda if the copy is visible"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r3", "y5", "b2", "g4"),
			Vector("b3", "p4", "b4", "b2"),
			Vector("y4", "b4", "r4", "r3")
		),
			starting = Donald,
			clueTokens = 0
		)
		.pipe(takeTurn("Donald discards r3", "p3"))

		// Since Alice can see the other copy of r3, she should discard.
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Discard(game.state.hands(Alice.ordinal)(3)))

	test("doesn't trigger dda from a sarcastic discard"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r3", "y5", "b2", "g4"),
			Vector("b3", "b1", "g1", "b3"),
			Vector("b1", "b4", "r4", "r3")
		),
			starting = Bob,
			discarded = Vector("b1"),
			clueTokens = 2
		)
		.pipe(takeTurn("Bob clues 1 to Cathy"))
		.pipe(takeTurn("Cathy clues blue to Donald"))
		.pipe(takeTurn("Donald discards b1", "p3"))

		assertEquals(game.dda, None)

class Anxiety extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("plays into anxiety"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y5", "y2", "b4", "g4"),
			Vector("b1", "g4", "b4", "b2"),
			Vector("y4", "y4", "r4", "r3")
		),
			starting = Cathy,
			playStacks = Some(Vector(4, 0, 0, 0, 0)),
			clueTokens = 2
		)
		.pipe(takeTurn("Cathy clues 5 to Alice (slots 2,3,4)"))
		.pipe(takeTurn("Donald clues 2 to Alice (slot 1)"))

		// Alice should play slot 2 as r5.
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Play(game.state.hands(Alice.ordinal)(1)))

	test("doesn't assume anxiety if there are clues available"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y5", "y2", "b4", "g4"),
			Vector("b1", "g4", "b4", "b2"),
			Vector("y4", "y4", "r4", "r3")
		),
			starting = Donald,
			playStacks = Some(Vector(4, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Donald clues 5 to Alice (slots 1,2,3,4)"))

		// Alice should clue.
		val action = game.takeAction.unsafeRunSync()
		assert(action match
			case _: PerformAction.Rank => true
			case _: PerformAction.Colour => true
			case _ => false)

	test("doesn't play into impossible anxiety"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r5", "y2", "b4", "g4"),
			Vector("b1", "g4", "b4", "b2"),
			Vector("y4", "y4", "r4", "r3")
		),
			starting = Donald,
			playStacks = Some(Vector(4, 0, 0, 0, 0)),
			clueTokens = 1
		)
		.pipe(takeTurn("Donald clues 5 to Alice (slots 1,2,3,4)"))

		// Alice should discard, since it isn't possible to play any card.
		assert(game.takeAction.unsafeRunSync().isInstanceOf[PerformAction.Discard])

	test("forces the next player into anxiety by playing a connecting card"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r5", "y5", "b5", "g5"),
			Vector("b3", "g4", "b4", "b4"),
			Vector("y4", "y4", "r4", "r3")
		),
			starting = Cathy,
			playStacks = Some(Vector(3, 0, 0, 0, 0)),
			clueTokens = 2
		)
		.pipe(takeTurn("Cathy clues 5 to Bob"))
		.pipe(takeTurn("Donald clues red to Alice (slot 1)"))

		// Alice should play slot 1.
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Play(game.state.hands(Alice.ordinal)(0)))

	test("forces the next player into anxiety by playing an unrelated card"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r5", "y5", "b5", "g5"),
			Vector("b3", "g4", "b4", "b4"),
			Vector("y4", "y4", "r4", "r3")
		),
			starting = Cathy,
			playStacks = Some(Vector(4, 0, 0, 0, 0)),
			clueTokens = 2
		)
		.pipe(takeTurn("Cathy clues 5 to Bob"))
		.pipe(takeTurn("Donald clues blue to Alice (slot 1)"))

		// Alice should play slot 1.
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Play(game.state.hands(Alice.ordinal)(0)))

	test("gives an anxiety clue to the next player"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r3", "y5", "b5", "g5"),
			Vector("r1", "g4", "b4", "b4"),
			Vector("y4", "y4", "p4", "p3")
		),
			starting = Cathy,
			playStacks = Some(Vector(2, 0, 0, 0, 0)),
			discarded = Vector("r3", "r4", "b3"),
			clueTokens = 3,
			init = _.copy(inEarlyGame = false)
		)
		.pipe(takeTurn("Cathy clues 5 to Bob"))
		.pipe(takeTurn("Donald clues green to Alice (slot 1)"))

		// Alice should clue red/3 to Bob as anxiety
		val clues = Seq(PerformAction.Rank(Bob.ordinal, 3), PerformAction.Colour(Bob.ordinal, Colour.Red.ordinal))
		val action = game.takeAction.unsafeRunSync()
		assert(clues.contains(action), s"Expected clue, got ${action.fmt(game)}")
