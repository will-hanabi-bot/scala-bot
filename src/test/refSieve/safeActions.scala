package tests.refSieve

import cats.effect.unsafe.implicits.global

import scala_bot.refSieve.RefSieve
import scala_bot.basics._
import scala_bot.test.{fullyKnown, hasInfs, hasStatus, Player, setup, takeTurn}, Player._

import scala_bot.utils.pipe
import scala_bot.logger.{Logger, LogLevel}

class SafeActions extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("prefers to give direct ranks"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y4", "g1", "b1", "g3", "g4")
		))

		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Rank(Bob.ordinal, 1))

	test("understands direct ranks are not referential"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b5", "y4", "g2", "r4", "y3")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slots 2,3)"))

		hasStatus(game, Alice, 4, CardStatus.None)

	test("eliminates direct ranks from focus"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b5", "y4", "g2", "r4", "y3")
		),
			starting = Bob,
			playStacks = Some(Vector(1, 1, 0, 1, 1))
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slots 2,3)"))

		hasStatus(game, Alice, 4, CardStatus.None)
		hasInfs(game, None, Alice, 2, Vector("g1"))

		// Alice's slot 3 should be trash
		val trash = game.common.thinksTrash(game, Alice.ordinal)
		assert(trash.contains(game.state.hands(Alice.ordinal)(2)))

	test("understands playable fill-ins are not referential"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b5", "y4", "g2", "r4", "y3")
		),
			starting = Bob,
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Bob clues red to Alice (slots 2,3)"))
		.pipe(takeTurn("Alice plays b1 (slot 1)"))
		// Bob reveals r2 as a safe action.
		.pipe(takeTurn("Bob clues 2 to Alice (slots 1,3)"))

		hasStatus(game, Alice, 4, CardStatus.None)

	test("doesn't give unloaded clues that connect through own hand"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g2", "y5", "g5", "r2", "p3")
		),
			init = fullyKnown(Alice, 2, "r1")
		)

		// Alice should not give purple.
		assert(game.takeAction.unsafeRunSync() match
			case PerformAction.Colour(1, 4) => false
			case _ => true
		)

	test("it interprets a gd"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y1", "r2", "g1", "g2", "p2"),
			Vector("r3", "p4", "g5", "y4", "r4"),
		),
			init = fullyKnown(Bob, 1, "y1"),
			starting = Bob,
			clueTokens = 6
		)
		.pipe(takeTurn("Bob discards y1", "y4"))

		hasStatus(game, Alice, 5, CardStatus.GentlemansDiscard)
		hasInfs(game, None, Alice, 5, Vector("y1"))
		assert(game.common.obviousPlayables(game, Alice.ordinal).contains(0))

	test("it interprets a layered gd"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "r2", "g1", "g2", "p2"),
			Vector("y1", "p4", "g5", "y4", "r4"),
		),
			init = fullyKnown(Cathy, 1, "y1"),
			starting = Cathy,
			clueTokens = 6
		)
		.pipe(takeTurn("Cathy discards y1", "y4"))
		.pipe(takeTurn("Alice plays b1 (slot 5)"))

		// The card originally in slot 4 has moved to slot 5.
		hasStatus(game, Alice, 5, CardStatus.GentlemansDiscard)
		hasInfs(game, None, Alice, 5, Vector("y1"))
		assert(game.common.obviousPlayables(game, Alice.ordinal).contains(1))
