package tests.hgroup.level10

import scala_bot.basics._
import scala_bot.test.{hasInfs, hasStatus, Player, preClue, setup, takeTurn}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger, LogLevel}

class GentlemansDiscards extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands a gentleman's discard"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y4", "g4", "b4", "b2"),
			Vector("r1", "y4", "r4", "y3"),
			Vector("y5", "r5", "b1", "g5")
		))
		.pipe(takeTurn("Alice clues blue to Donald"))	// getting b1
		.pipe(takeTurn("Bob clues red to Cathy"))		// getting r1
		.pipe(takeTurn("Cathy plays r1", "b1"))
		.pipe(takeTurn("Donald discards b1", "b3"))

		// Donald performed a Gentleman's Discard on Cathy.
		hasInfs(game, None, Cathy, 1, Vector("b1"))
		hasStatus(game, Cathy, 1, CardStatus.GentlemansDiscard)

	test("understands a gentleman's discard on us"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y1", "g4", "b4", "b2"),
			Vector("r1", "y4", "r4", "y3"),
			Vector("y5", "r5", "b1", "g5")
		))
		.pipe(takeTurn("Alice clues yellow to Bob"))	// getting y1
		.pipe(takeTurn("Bob discards y1", "y3"))

		// Bob performed a Gentleman's Discard on us.
		hasInfs(game, None, Alice, 1, Vector("y1"))
		assert(game.common.thinksPlayables(game, Alice.ordinal).contains(game.state.hands(Alice.ordinal)(0)))
		hasStatus(game, Alice, 1, CardStatus.GentlemansDiscard)

	test("understands a layered gentleman's discard"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y4", "g4", "b4", "b2"),
			Vector("r1", "y2", "b1", "y3"),
			Vector("y5", "r5", "b1", "g5")
		))
		.pipe(takeTurn("Alice clues blue to Donald"))	// getting b1
		.pipe(takeTurn("Bob clues red to Cathy"))		// getting r1
		.pipe(takeTurn("Cathy plays r1", "y1"))
		.pipe(takeTurn("Donald discards b1", "b3"))
		.tap:
			// Donald performed a Layered Gentleman's Discard on Cathy.
			hasStatus(_, Cathy, 1, CardStatus.GentlemansDiscard)
		.pipe(takeTurn("Alice clues 2 to Bob"))
		.pipe(takeTurn("Bob clues 5 to Donald"))
		.pipe(takeTurn("Cathy plays y1", "p4"))

		hasStatus(game, Cathy, 2, CardStatus.GentlemansDiscard)

	test("understands a layered gentleman's discard on us"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y1", "g4", "b4", "b2"),
			Vector("r1", "y4", "r4", "y5"),
			Vector("y3", "r5", "b1", "g5")
		))
		.pipe(takeTurn("Alice clues yellow to Bob"))	// getting y1
		.pipe(takeTurn("Bob discards y1", "y3"))
		.pipe(takeTurn("Cathy clues 5 to Donald"))
		.pipe(takeTurn("Donald clues 5 to Cathy"))
		.tap:
			// Donald performed a Gentleman's Discard on Alice.
			hasStatus(_, Alice, 1, CardStatus.GentlemansDiscard)
		.pipe(takeTurn("Alice plays b1 (slot 1)"))

		// The layer is revealed.
		hasStatus(game, Alice, 2, CardStatus.GentlemansDiscard)

	test("performs a gentleman's discard"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b3", "g4", "y2", "b4"),
			Vector("r5", "y4", "r4", "y3"),
			Vector("b1", "r4", "b4", "g3")
		),
			starting = Donald,
			clueTokens = 7
		)
		.pipe(takeTurn("Donald clues blue to Alice (slots 3,4)"))	// b1 in slot 4
		.tap: g =>
			assertEquals(g.takeAction, PerformAction.Discard(g.state.hands(Alice.ordinal)(3)))
		.pipe(takeTurn("Alice discards b1 (slot 4)"))

		// We performed a GD on Donald.
		hasStatus(game, Donald, 1, CardStatus.GentlemansDiscard)

	test("performs a layered gentleman's discard"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b3", "g4", "y2", "b4"),
			Vector("g1", "y4", "r4", "y3"),
			Vector("r1", "b1", "b4", "g3")
		),
			starting = Donald,
			clueTokens = 6
		)
		.pipe(takeTurn("Donald clues blue to Alice (slots 3,4)"))	// b1 in slot 4
		.tap: g =>
			assertEquals(g.takeAction, PerformAction.Discard(g.state.hands(Alice.ordinal)(3)))
		.pipe(takeTurn("Alice discards b1 (slot 4)"))

		// We performed a GD on Donald.
		hasStatus(game, Donald, 1, CardStatus.GentlemansDiscard)

	test("understands a gd with finesse on us"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y1", "g4", "b4", "b2"),
			Vector("r1", "y4", "r4", "y5"),
			Vector("y3", "r5", "b1", "g5")
		))
		.pipe(takeTurn("Alice clues yellow to Bob"))	// getting y1
		.pipe(takeTurn("Bob discards y1", "y3"))
		.pipe(takeTurn("Cathy clues 5 to Donald"))
		.pipe(takeTurn("Donald clues blue to Bob"))		// finessing b1
		.tap: g =>
			// Alice should play slot 1.
			val slot1 = g.state.hands(Alice.ordinal)(0)
			assertEquals(g.common.thinksPlayables(g, Alice.ordinal), Seq(slot1))
			assertEquals(g.takeAction, PerformAction.Play(slot1))
		.pipe(takeTurn("Alice plays y1 (slot 1)"))

		// Bob performed a gd on us. b1 is now expected to be in slot 2.
		hasStatus(game, Alice, 2, CardStatus.Finessed)
		hasInfs(game, None, Alice, 2, Vector("b1"))

		// The finesse is still on (allow b34 symmetrically?).
		hasInfs(game, None, Bob, 4, Vector("b2", "b3", "b4"))

	test("understands a layered gd with finesse on us"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y1", "g4", "b4", "b2"),
			Vector("r1", "y4", "r4", "y5"),
			Vector("y3", "r5", "b1", "g5")
		))
		.pipe(takeTurn("Alice clues yellow to Bob"))	// getting y1
		.pipe(takeTurn("Bob discards y1", "y3"))
		.pipe(takeTurn("Cathy clues 5 to Donald"))
		.pipe(takeTurn("Donald clues blue to Bob"))		// finessing b1
		.tap: g =>
			// Alice should play slot 1.
			val slot1 = g.state.hands(Alice.ordinal)(0)
			assertEquals(g.common.thinksPlayables(g, Alice.ordinal), Seq(slot1))
			assertEquals(g.takeAction, PerformAction.Play(slot1))
		.pipe(takeTurn("Alice plays b1 (slot 1)"))

		// Bob performed a layered gd on us. y1 is now expected to be in slot 2.
		hasStatus(game, Alice, 2, CardStatus.GentlemansDiscard)
		hasInfs(game, None, Alice, 2, Vector("y1"))

	test("understands a layered finesse with gd on us"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "g4", "b4", "b4"),
			Vector("y1", "y4", "r4", "y5"),
			Vector("y3", "r5", "b2", "g5")
		))
		.pipe(takeTurn("Alice clues yellow to Cathy"))	// getting y1
		.pipe(takeTurn("Bob clues blue to Donald"))		// reverse finesse on b2
		.pipe(takeTurn("Cathy discards y1", "y3"))
		.pipe(takeTurn("Donald clues 5 to Cathy"))
		.tap: g =>
			// Alice should play slot 1.
			val slot1 = g.state.hands(Alice.ordinal)(0)
			assertEquals(g.common.thinksPlayables(g, Alice.ordinal), Seq(slot1))
			assertEquals(g.takeAction, PerformAction.Play(slot1))
		.pipe(takeTurn("Alice plays y1 (slot 1)"))

		// Bob performed a layered finesse on us. b1 is now expected to be in slot 2.
		hasStatus(game, Alice, 2, CardStatus.Finessed)
		hasInfs(game, None, Alice, 2, Vector("b1"))

		// The finesse is still on (allow b34 symmetrically?).
		hasInfs(game, None, Donald, 3, Vector("b2", "b3", "b4"))

	test("understands a sarcastic into a layered finesse (not gd)"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y1", "r1", "b4", "b4"),
			Vector("g1", "r2", "r3", "y5"),
			Vector("g1", "r4", "r5", "p3")
		))
		.pipe(takeTurn("Alice clues 3 to Cathy"))
		.pipe(takeTurn("Bob plays y1", "r4"))
		.pipe(takeTurn("Cathy clues green to Donald"))
		.pipe(takeTurn("Donald discards g1", "p4"))		// passing g1 back to Cathy

		// This isn't a GD on ALice.
		hasStatus(game, Alice, 1, CardStatus.None)

	test("understands a delayed gd"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y4", "r3", "y4", "g4", "g4"),
			Vector("r1", "b4", "b4", "p4", "r2")
		),
			init = preClue(Cathy, 5, Seq("2"))
		)
		.pipe(takeTurn("Alice clues 3 to Bob"))
		.pipe(takeTurn("Bob clues green to Alice (slot 5)"))
		.pipe(takeTurn("Cathy plays r1", "y5"))

		.pipe(takeTurn("Alice plays g1 (slot 5)"))
		.pipe(takeTurn("Bob discards r3", "p5"))
		.tap: g =>
			// GD for r3 in Alice's slot 1.
			hasInfs(g, None, Alice, 1, Vector("r3"))
			hasStatus(g, Alice, 1, CardStatus.GentlemansDiscard)

		.pipe(takeTurn("Cathy plays r2", "b5"))
		.pipe(takeTurn("Alice plays y1 (slot 1)"))

		// Layered GD revealed, r3 moved to slot 2.
		hasInfs(game, None, Alice, 2, Vector("r3"))
		hasStatus(game, Alice, 2, CardStatus.GentlemansDiscard)

class BatonDiscards extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands a baton discard"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b4", "g4", "y2", "b2"),
			Vector("r1", "y4", "p4", "y3"),
			Vector("y1", "r4", "b4", "b1")
		),
			discarded = Vector("r4")
		)
		.pipe(takeTurn("Alice clues blue to Donald"))	// getting b1
		.pipe(takeTurn("Bob clues 4 to Donald"))		// saving r4
		.pipe(takeTurn("Cathy clues yellow to Donald"))	// getting y1
		.pipe(takeTurn("Donald discards b4", "b3"))

		// Donald performed a Baton Discard on Bob.
		hasInfs(game, None, Bob, 1, Vector("b4"))
		hasStatus(game, Bob, 1, CardStatus.Sarcastic)

	test("understands a baton discard on us"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b3", "g4", "y2", "b2"),
			Vector("r1", "y4", "p4", "y3"),
			Vector("y1", "r4", "b4", "b1")
		),
			discarded = Vector("r4")
		)
		.pipe(takeTurn("Alice clues blue to Donald"))	// getting b1
		.pipe(takeTurn("Bob clues 4 to Donald"))		// saving r4
		.pipe(takeTurn("Cathy clues yellow to Donald"))	// getting y1
		.pipe(takeTurn("Donald discards b4", "b3"))

		// Donald performed a Baton Discard on Alice.
		hasInfs(game, None, Alice, 1, Vector("b4"))
		hasStatus(game, Alice, 1, CardStatus.Sarcastic)

class CompositionFinesses extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands a certain discard"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y3", "g4", "y2", "r4", "b3"),
			Vector("r3", "b1", "r2", "b5", "y3")
		),
			starting = Cathy,
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Cathy clues red to Alice (slots 4,5)"))
		.pipe(takeTurn("Alice plays r2 (slot 5)"))
		.pipe(takeTurn("Bob clues blue to Cathy"))

		.pipe(takeTurn("Cathy clues 5 to Alice (slot 4)"))	// 5 Save on slot 4, [r, !5] in slot 5
		.pipe(takeTurn("Alice clues red to Bob"))			// Composition f, getting r3 on Cathy's finesse
		.pipe(takeTurn("Bob discards b3", "y1"))

		.pipe(takeTurn("Cathy discards r3", "g2"))

		hasInfs(game, None, Alice, 5, Vector("r3"))

	test("performs a certain discard"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p3", "p3", "p5", "r3", "r2"),
			Vector("y3", "g4", "y2", "r4", "b3")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues red to Bob"))
		.pipe(takeTurn("Bob plays r2", "g2"))
		.pipe(takeTurn("Cathy clues 5 to Alice (slot 5)"))

		.pipe(takeTurn("Alice clues 5 to Bob"))
		.pipe(takeTurn("Bob clues red to Cathy"))	// Composition f, getting r3 on Alice's finesse
		.pipe(takeTurn("Cathy discards b3", "y1"))

		// Alice should certain discard slot 1 as r3.
		assertEquals(game.takeAction, PerformAction.Discard(game.state.hands(Alice.ordinal)(0)))

	test("recognizes an illegal layered certain finesse"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y3", "g4", "y2", "r4", "b3"),
			Vector("b1", "r3", "r2", "g3", "b5")
		),
			starting = Cathy,
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Cathy clues red to Alice (slots 4,5)"))
		.pipe(takeTurn("Alice plays r2 (slot 5)"))
		.pipe(takeTurn("Bob clues 5 to Cathy"))

		.pipe(takeTurn("Cathy clues 5 to Alice (slot 4)"))
		.pipe(takeTurn("Alice clues red to Bob"))

		// Cathy can't perform a Certain Discard to Alice's unknown red, so this is illegal.
		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("correctly resolves a complex certain discard"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "b4", "y4", "g4"),
			Vector("g2", "r4", "y3", "b5"),
			Vector("g1", "g3", "r2", "b1")
		),
			starting = Donald,
			playStacks = Some(Vector(4, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Donald clues 1 to Alice (slots 2,4)"))
		.pipe(takeTurn("Alice clues green to Bob"))		// Quadruple finesse
		.pipe(takeTurn("Bob clues 5 to Cathy"))
		.pipe(takeTurn("Cathy clues 5 to Alice (slot 3)"))	// Alice: xx x1 x5 x1

		.pipe(takeTurn("Donald discards g1", "r3"))		// Donald passes g1 to Alice
		.pipe(takeTurn("Alice plays p1 (slot 2)"))
		.tap: g =>
			assert(g.waiting.exists(_.inference == g.state.expandShort("g4")))

		.pipe(takeTurn("Bob clues blue to Donald"))
		.pipe(takeTurn("Cathy discards y3", "p5"))
		.pipe(takeTurn("Donald clues 5 to Alice (slot 3)"))	// Play clue on g5
		.pipe(takeTurn("Alice plays g1 (slot 4)"))
		.tap: g =>
			assert(g.waiting.exists(_.inference == g.state.expandShort("g4")))

		.pipe(takeTurn("Bob discards y4", "y2"))
		.pipe(takeTurn("Cathy plays g2", "p3"))
		.pipe(takeTurn("Donald plays g3", "p1"))

		hasInfs(game, None, Alice, 4, Vector("r5", "g5"))

	test("correctly resolves a fake certain discard"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g2", "r4", "y3", "g5"),
			Vector("g1", "g3", "r2", "b1"),
			Vector("b2", "y5", "b5", "b4")
		),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues blue to Alice (slots 1,2,3)"))
		.pipe(takeTurn("Alice plays b1 (slot 1)"))
		.pipe(takeTurn("Bob clues 4 to Donald"))		// Looks like b2 self-finesse
		.pipe(takeTurn("Cathy clues 5 to Donald"))

		.pipe(takeTurn("Donald discards b2", "g2"))		// Donald proves Alice is prompted for b2,b3

		hasInfs(game, None, Alice, 2, Vector("b2"))
		hasInfs(game, None, Alice, 3, Vector("b3"))
