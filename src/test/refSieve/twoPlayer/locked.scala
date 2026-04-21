package tests.refSieve.twoPlayer

import cats.effect.unsafe.implicits.global

import scala_bot.refSieve.RefSieve
import scala_bot.basics._
import scala_bot.test.{Colour, hasInfs, hasStatus, Player, preClue, setup, takeTurn}, Player._

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger, LogLevel}

class Locked extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("gives a colour clue to playable slot 1"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "p4", "y5", "y4", "b2")
		))
		.pipe(takeTurn("Alice clues 5 to Bob"))
		.pipe(takeTurn("Bob clues 5 to Alice (slot 5)"))
		.tap: g =>
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Colour(Bob.ordinal, Colour.Blue.ordinal))
		.pipe(takeTurn("Alice clues blue to Bob"))

		hasInfs(game, None, Bob, 1, Vector("b1"))

	test("gives a referential discard to save lh ptd"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y5", "y3", "g3", "y1", "r4")
		))
		.pipe(takeTurn("Alice clues red to Bob"))
		.pipe(takeTurn("Bob clues 5 to Alice (slot 5)"))
		.tap: g =>
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Rank(Bob.ordinal, 5))
		.pipe(takeTurn("Alice clues 5 to Bob"))

		hasStatus(game, Bob, 2, CardStatus.CalledToDiscard)

	test("understands a colour stall"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p3", "y1", "b4", "r3", "g4")
		))
		.pipe(takeTurn("Alice clues blue to Bob"))
		.pipe(takeTurn("Bob clues 5 to Alice (slot 5)"))
		.pipe(takeTurn("Alice clues green to Bob"))

		hasStatus(game, Bob, 4, CardStatus.None)
		hasStatus(game, Bob, 1, CardStatus.PermissionToDiscard)

	test("doesn't get locked hand ptd on a known playable"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g1", "y3", "r5", "b5", "g5")
		))
		.pipe(takeTurn("Alice clues yellow to Bob"))
		.pipe(takeTurn("Bob clues 5 to Alice (slot 5)"))
		.pipe(takeTurn("Alice clues 5 to Bob"))

		hasStatus(game, Bob, 1, CardStatus.CalledToPlay)

class UnlockPromise extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("unlocks a directly connecting card"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "p4", "y5", "y4", "g2")
		))
		.pipe(takeTurn("Alice clues 5 to Bob"))
		.pipe(takeTurn("Bob clues 2 to Alice (slots 4,5)"))
		.pipe(takeTurn("Alice clues blue to Bob"))
		.pipe(takeTurn("Bob plays b1", "p1"))

		hasInfs(game, None, Alice, 5, Vector("b2"))

	test("unlocks an unclued card"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "p4", "y5", "y4", "g2")
		))
		.pipe(takeTurn("Alice clues 5 to Bob"))
		.pipe(takeTurn("Bob clues 5 to Alice (slot 5)"))
		.pipe(takeTurn("Alice clues blue to Bob"))
		.pipe(takeTurn("Bob plays b1", "p1"))

		assert(game.common.thinksPlayables(game, Alice.ordinal).contains(game.state.hands(Alice.ordinal)(3)))

	test("unlocks a directly connecting card after a shift"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "p4", "y5", "y4", "g2")
		))
		.pipe(takeTurn("Alice clues 5 to Bob"))
		.pipe(takeTurn("Bob clues 2 to Alice (slots 4,5)"))

		.pipe(takeTurn("Alice clues blue to Bob"))
		.pipe(takeTurn("Bob discards y4", "p1"))

		.pipe(takeTurn("Alice clues purple to Bob"))
		.pipe(takeTurn("Bob plays b1", "b4"))

		hasInfs(game, None, Alice, 4, Vector("b2"))

	test("unlocks after shifting beyond all directly connecting cards"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "p4", "y5", "y4", "g2")
		),
			init = preClue(Alice, 4, Seq("blue"))
		)
		.pipe(takeTurn("Alice clues 5 to Bob"))
		.pipe(takeTurn("Bob clues 2 to Alice (slots 3,5)"))

		.pipe(takeTurn("Alice clues blue to Bob"))
		.pipe(takeTurn("Bob discards y4", "r1"))		// delay 1

		.pipe(takeTurn("Alice clues red to Bob"))
		.pipe(takeTurn("Bob discards p4", "b4"))		// delay 2

		.pipe(takeTurn("Alice clues 2 to Bob"))
		.pipe(takeTurn("Bob plays b1", "y3"))

		// Shifted to slot 2.
		assert(game.common.thinksPlayables(game, Alice.ordinal).contains(game.state.hands(Alice.ordinal)(1)))

	test("doesn't unlock when playing oldest playable with no discard"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "p5", "y5", "y4", "b4")
		))
		.pipe(takeTurn("Alice clues 5 to Bob"))
		.pipe(takeTurn("Bob clues 2 to Alice (slots 4,5)"))

		.pipe(takeTurn("Alice clues blue to Bob"))
		.pipe(takeTurn("Bob discards y4", "p1"))

		.pipe(takeTurn("Alice clues purple to Bob"))
		.pipe(takeTurn("Bob plays b1", "b4"))

		assert(game.common.thinksPlayables(game, Alice.ordinal).isEmpty)

	test("doesn't play if won't unlock"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g5", "p2", "r2", "g4", "r5")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 5 to Alice (slot 3)"))
		.pipe(takeTurn("Alice clues 2 to Bob"))

		.pipe(takeTurn("Bob discards g4", "p5"))
		.pipe(takeTurn("Alice clues 5 to Bob"))

		.pipe(takeTurn("Bob clues blue to Alice (slot 1)"))

		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Discard(game.state.hands(Alice.ordinal)(3)))

	test("prefers unlocking over discarding trash"):
		val game = setup(RefSieve.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g5", "p2", "r2", "g4", "r5")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slots 3,4)"))
		.pipe(takeTurn("Alice clues 2 to Bob"))

		.pipe(takeTurn("Bob discards g4", "p5"))
		.pipe(takeTurn("Alice clues 5 to Bob"))

		.pipe(takeTurn("Bob clues red to Alice (slots 3,4)"))

		assert(game.takeAction.unsafeRunSync().isInstanceOf[PerformAction.Play])
