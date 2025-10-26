package tests.refSieve.safeActions

import scala_bot.refSieve.RefSieve
import scala_bot.basics._
import scala_bot.test.{fullyKnown, hasInfs, Player, setup, takeTurn}, Player._
import scala_bot.logger.{Logger, LogLevel}

import scala.util.chaining.scalaUtilChainingOps

class SafeActions extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("prefers to give direct ranks") {
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y4", "g1", "b1", "g3", "g4")
		))

		assertEquals(game.takeAction, PerformAction.Rank(Bob.ordinal, 1))
	}

	test("understands direct ranks are not referential") {
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b5", "y4", "g2", "r4", "y3")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slots 2,3)"))

		assertEquals(game.meta(game.state.hands(Alice.ordinal)(3)).status, CardStatus.None)
	}

	test("eliminates direct ranks from focus") {
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b5", "y4", "g2", "r4", "y3")
		),
			starting = Bob,
			playStacks = Some(Vector(1, 1, 0, 1, 1))
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slots 2,3)"))

		assertEquals(game.meta(game.state.hands(Alice.ordinal)(3)).status, CardStatus.None)
		hasInfs(game, None, Alice, 2, Vector("g1"))

		// Alice's slot 3 should be trash
		val trash = game.common.thinksTrash(game, Alice.ordinal)
		assert(trash.contains(game.state.hands(Alice.ordinal)(2)))
	}

	test("understands playable fill-ins are not referential") {
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

		// Alice's slot 4 should not be called to discard.
		val slot4 = game.meta(game.state.hands(Alice.ordinal)(3))
		assertEquals(slot4.status, CardStatus.None)
	}

	test("doesn't give unloaded clues that connect through own hand") {
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g2", "y5", "g5", "r2", "p3")
		),
			init = fullyKnown(Alice, 2, "r1")
		)

		// Alice should not give purple.
		assert(game.takeAction match {
			case PerformAction.Colour(1, 4) => false
			case _ => true
		})
	}
