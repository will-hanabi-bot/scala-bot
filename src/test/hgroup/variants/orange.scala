package tests.hgroup

import cats.effect.unsafe.implicits.global

import scala_bot.basics._
import scala_bot.test.{fullyKnown, hasInfs, hasStatus, Player, preClue, setup, takeTurn, TestVariant}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger, LogLevel}

class Orange extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("discards an orange playable"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "r4")
		),
			starting = Bob,
			variant = TestVariant.Orange5
		)
		.pipe(takeTurn("Bob clues orange to Alice (slot 1)"))

		hasInfs(game, None, Alice, 1, Vector("o1"))
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Discard(game.state.hands(Alice.ordinal)(0)))

	test("discards an orange finesse"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("o2", "y4", "y4", "o4", "r4"),
			Vector("g4", "g4", "b4", "b4", "r4")
		),
			starting = Cathy,
			variant = TestVariant.Orange5
		)
		.pipe(takeTurn("Cathy clues orange to Bob"))

		hasInfs(game, None, Alice, 1, Vector("o1"))
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Discard(game.state.hands(Alice.ordinal)(0)))

	test("discards an orange bluff"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "y4", "b4", "o4", "r4"),
			Vector("g4", "y4", "b4", "o4", "r4")
		),
			starting = Cathy,
			variant = TestVariant.Orange5,
			init = preClue(Alice, 5, Seq("orange"))
		)
		.pipe(takeTurn("Cathy clues 2 to Alice (slot 5)"))

		hasInfs(game, None, Alice, 1, Vector("o1"))
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Discard(game.state.hands(Alice.ordinal)(0)))

	test("discards an orange layered finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("o2", "y4", "y4", "o4", "r4"),
			Vector("g4", "g4", "b4", "b4", "r4")
		),
			starting = Cathy,
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			variant = TestVariant.Orange5
		)
		.pipe(takeTurn("Cathy clues orange to Bob"))
		.pipe(takeTurn("Alice discards r1 (slot 1)"))

		// Alice is still promised for o1.
		hasInfs(game, None, Alice, 2, Vector("o1"))

	test("lets a playable orange card discard"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "o2")
		),
			starting = Bob,
			variant = TestVariant.Orange5
		)
		.pipe(takeTurn("Bob clues orange to Alice (slot 1)"))

		hasInfs(game, None, Alice, 1, Vector("o1"))
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Discard(game.state.hands(Alice.ordinal)(0)))

	test("understands an orange fix clue on chop"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "r4")
		),
			starting = Bob,
			variant = TestVariant.Orange5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues orange to Alice (slot 5)"))

		hasInfs(game, None, Alice, 5, Vector("o3", "o4"))

	test("gives an orange fix clue on chop"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "o4")
		),
			variant = TestVariant.Orange5,
			clueTokens = 7
		)

		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Colour(Bob.ordinal, 4))

	test("gives an orange fix clue on a rank playable card"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "o1", "r1")
		),
			variant = TestVariant.Orange5,
			clueTokens = 7
		)
		.pipe(takeTurn("Alice clues 1 to Bob"))
		.pipe(takeTurn("Bob plays r1", "y4"))

		// Alice should fix with orange so Bob doesn't discard the o1.
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Colour(Bob.ordinal, 4))

	test("understands an orange fix clue with played orange cards"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "r4")
		),
			starting = Bob,
			playStacks = Some(Vector(0, 0, 0, 0, 4)),
			variant = TestVariant.Orange5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues orange to Alice (slot 5)"))

		assertEquals(game.common.thinksTrash(game, Alice.ordinal), Vector(game.state.hands(Alice.ordinal)(4)))

	test("gives an orange fix clue with played orange cards"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "o1")
		),
			playStacks = Some(Vector(0, 0, 0, 0, 4)),
			variant = TestVariant.Orange5,
			clueTokens = 7
		)

		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Colour(Bob.ordinal, 4))

	test("understands an orange gentleman's discard is to chop"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("o1", "g4", "g4", "b4", "b4")
		),
			variant = TestVariant.Orange5,
			clueTokens = 4
		)
		.pipe(takeTurn("Alice clues orange to Bob"))
		.pipe(takeTurn("Bob discards o1", "r4"))

		hasInfs(game, None, Alice, 5, Vector("o1"))
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Discard(0))

	test("understands an orange layered gentleman's discard"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("o1", "g4", "g4", "b4", "b4")
		),
			variant = TestVariant.Orange5,
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			clueTokens = 4
		)
		.pipe(takeTurn("Alice clues orange to Bob"))
		.pipe(takeTurn("Bob discards o1", "r4"))
		.pipe(takeTurn("Alice discards r1 (slot 5)"))

		// o1 is still promised in Alice's new chop.
		hasInfs(game, None, Alice, 5, Vector("o1"))

		// Bob is not shouted for.
		hasStatus(game, Bob, 5, CardStatus.PermissionToDiscard)

	test("understands an orange baton discard"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("o3", "g4", "g4", "b4", "b4")
		),
			starting = Bob,
			variant = TestVariant.Orange5,
			clueTokens = 4,
			init = fullyKnown(Bob, 1, "o3")
		)
		.pipe(takeTurn("Bob discards o3", "r4"))

		// o3 is promised on Alice's chop.
		hasInfs(game, None, Alice, 5, Vector("o3"))

		// o3 is chop moved.
		// hasStatus(game, Alice, 5, CardStatus.ChopMoved)

		assertEquals(game.chop(Alice.ordinal), Some(1))

	test("screams for an unplayable orange on chop"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("o3", "g4", "g4", "r1", "o3")
		),
			variant = TestVariant.Orange5,
			clueTokens = 1,
			init = preClue(Alice, 5, Seq("1"))
		)
		.pipe(takeTurn("Alice clues red to Bob"))
		.pipe(takeTurn("Bob plays r1", "r5"))
		.tap: g =>
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Discard(g.state.hands(Alice.ordinal)(3)))
		.pipe(takeTurn("Alice discards b4 (slot 4)"))

		assertEquals(game.lastMove, Some(DiscardInterp.Emergency))
		hasStatus(game, Bob, 5, CardStatus.ChopMoved)

	test("discarding chop ends early game, even if it plays"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "y4", "g4", "b4", "o1")
		),
			variant = TestVariant.Orange5,
			starting = Bob,
			clueTokens = 2
		)
		.tap: g =>
			assertEquals(g.inEarlyGame, true)
		.pipe(takeTurn("Bob plays o1", "r5"))

		assertEquals(game.inEarlyGame, false)

	test("discarding chop ends early game, even if it bombs"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "y4", "g4", "b4", "o3")
		),
			variant = TestVariant.Orange5,
			starting = Bob,
			clueTokens = 2
		)
		.tap: g =>
			assertEquals(g.inEarlyGame, true)
		.pipe(takeTurn("Bob bombs o3", "r5"))

		assertEquals(game.inEarlyGame, false)

	test("doesn't try to insert an orange card into a layered finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r2", "o2", "g4", "b4"),
			Vector("r4", "y4", "g4", "b4"),
			Vector("r3", "y3", "g3", "b3"),
		),
			variant = TestVariant.Orange5,
			starting = Cathy,
			clueTokens = 4
		)
		.pipe(takeTurn("Cathy clues red to Bob"))
		.tap: g =>
			hasInfs(g, None, Alice, 1, Vector("r1"))
		.pipe(takeTurn("Donald clues orange to Bob"))

		hasInfs(game, None, Alice, 1, Vector("r1"))
		hasInfs(game, None, Alice, 2, Vector("o1"))

		// Still, only slot 1 is playable, because it might be layered.
		assertEquals(game.common.thinksPlayables(game, Alice.ordinal), Vector(game.state.hands(Alice.ordinal)(0)))

	test("doesn't try to insert a layer into an orange finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r2", "o2", "g4", "b4"),
			Vector("r4", "y4", "g4", "b4"),
			Vector("r3", "y3", "g3", "b3"),
		),
			variant = TestVariant.Orange5,
			starting = Cathy,
			clueTokens = 4
		)
		.pipe(takeTurn("Cathy clues orange to Bob"))
		.tap: g =>
			hasInfs(g, None, Alice, 1, Vector("o1"))
		.pipe(takeTurn("Donald clues red to Bob"))

		hasInfs(game, None, Alice, 1, Vector("o1"))
		hasInfs(game, None, Alice, 2, Vector("r1"))

		// Still, only slot 1 is playable, because it might be layered.
		assertEquals(game.common.thinksPlayables(game, Alice.ordinal), Vector(game.state.hands(Alice.ordinal)(0)))

	test("discards an orange card from a pos misplay"):
		val game = setup(HGroup.atLevel(8), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r5", "y1", "g1", "b1", "o1"),
			Vector("r1", "y1", "g1", "b1", "o1")
		),
			variant = TestVariant.Orange5,
			starting = Cathy,
			playStacks = Some(Vector(4, 5, 5, 5, 4)),
			clueTokens = 0,
			discarded = Vector(
				"r2", "r3", "r4",
				"y2", "y3", "y4",
				"g2", "g3", "g4",
				"b2", "b3"
			)	// Missing: r1, b4, o2, o3, o4, o5
		)
		.tap: g =>
			assertEquals(g.state.cardsLeft, 1)
		.pipe(takeTurn("Cathy bombs r1", "b4"))

		// Alice should discard slot 1 as o5.
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Discard(game.state.hands(Alice.ordinal)(0)))

	test("recognizes an orange misplay as a pos dc"):
		val game = setup(HGroup.atLevel(8), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r5", "y1", "g1", "b1", "o1"),
			Vector("o1", "y1", "g1", "b1", "r1")
		),
			variant = TestVariant.Orange5,
			starting = Cathy,
			playStacks = Some(Vector(4, 5, 5, 5, 4)),
			clueTokens = 0,
			discarded = Vector(
				"r2", "r3", "r4",
				"y2", "y3", "y4",
				"g2", "g3", "g4",
				"b2", "b3"
			)	// Missing: r1, b4, o2, o3, o4, o5
		)
		.tap: g =>
			assertEquals(g.state.cardsLeft, 1)
		.pipe(takeTurn("Cathy bombs o1", "b4"))

		// Alice doesn't have o5 in slot 1.
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Discard(game.state.hands(Alice.ordinal)(4)))
