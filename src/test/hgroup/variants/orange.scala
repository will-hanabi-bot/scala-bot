package tests.hgroup

import cats.effect.unsafe.implicits.global

import scala_bot.basics._
import scala_bot.test.{fullyKnown, hasInfs, hasStatus, Player, preClue, setup, takeTurn, TestVariant}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.pipe
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
		hasStatus(game, Bob, 5, CardStatus.None)

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
