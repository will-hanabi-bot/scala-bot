package tests.reactor.stable

import scala_bot.reactor.Reactor
import scala_bot.basics._
import scala_bot.test.{hasInfs, hasStatus, Player, preClue, setup, takeTurn}, Player._
import scala.util.chaining.scalaUtilChainingOps
import scala_bot.logger.{Logger, LogLevel}

class Stable extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("it understands a ref play"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "g2", "r2", "r3", "g5"),
			Vector("p4", "b5", "p2", "b1", "g4")
		))
		.pipe(takeTurn("Alice clues green to Bob"))

		hasStatus(game, Bob, 1, CardStatus.CalledToPlay)
		hasInfs(game, None, Bob, 1, Vector("r1", "y1", "b1", "p1"))

	test("it understands a gapped ref play"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p4", "b1", "p2", "b5", "g4"),
			Vector("b1", "g2", "r2", "r3", "g5"),
		))
		.pipe(takeTurn("Alice clues purple to Bob"))

		hasStatus(game, Bob, 2, CardStatus.CalledToPlay)
		hasInfs(game, None, Bob, 2, Vector("r1", "y1", "g1", "b1"))

	test("it understands a chop ref play"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "b2", "p2", "b5", "g4"),
			Vector("b1", "g2", "r2", "r3", "g5"),
		))
		.pipe(takeTurn("Alice clues blue to Bob"))

		hasInfs(game, None, Bob, 1, Vector("b1"))

	test("it understands a ref discard"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p4", "p2", "p2", "b5", "g3"),
			Vector("b1", "g2", "r2", "r3", "g5"),
		))
		.pipe(takeTurn("Alice clues 4 to Bob"))

		hasStatus(game, Bob, 2, CardStatus.CalledToDiscard)

	test("it gives a ref discard"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p4", "p2", "p2", "b5", "g3"),
			Vector("b3", "g2", "r2", "r3", "g5"),
		))

		// Alice should clue 4 to Bob.
		assertEquals(game.takeAction, PerformAction.Rank(Bob.ordinal, 4))

	test("eliminates direct ranks from focus"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p4", "p2", "p2", "r5", "g3"),
			Vector("b5", "y4", "g2", "r4", "y3")
		),
			starting = Cathy,
			playStacks = Some(Vector(1, 1, 0, 1, 1))
		)
		.pipe(takeTurn("Cathy clues 1 to Alice (slots 2,3)"))

		hasStatus(game, Alice, 4, CardStatus.None)
		hasInfs(game, None, Alice, 2, Vector("g1"))

		// Alice's slot 3 should be trash
		val trash = game.common.thinksTrash(game, Alice.ordinal)
		assert(trash.contains(game.state.hands(Alice.ordinal)(2)))

	test("it understands a lock"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p4", "p2", "p2", "b5", "g4"),
			Vector("b1", "g2", "r2", "r3", "g5"),
		))
		.pipe(takeTurn("Alice clues 4 to Bob"))

		assert(game.common.obviousLocked(game, Bob.ordinal))

	test("doesn't lock an already-locked player"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p4", "p2", "p2", "b5", "g4"),
			Vector("b1", "g2", "r2", "r3", "g5"),
		))
		.pipe(takeTurn("Alice clues 4 to Bob"))		// lock
		.pipe(takeTurn("Bob clues green to Cathy"))
		.pipe(takeTurn("Cathy plays b1", "y5"))
		.pipe(takeTurn("Alice clues 5 to Bob"))		// lock again??

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("it doesn't focus the wrong card for the last id"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "y1", "g1", "b1", "p1"),
			Vector("r1", "y1", "g1", "b1", "p1"),
		),
			playStacks = Some(Vector(3, 3, 3, 3, 2)),
			starting = Cathy,
			init = preClue(Alice, 5, Seq("purple"))
		)
		// Although Alice could play slot 2, she should play slot 5 first.
		.pipe(takeTurn("Cathy clues 3 to Alice (slots 2,5)"))

		assertEquals(game.takeAction, PerformAction.Play(0))

	test("it interprets a fix"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "p2", "p2", "b5", "g4"),
			Vector("r1", "g2", "r2", "r3", "g5"),
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 2 to Bob"))
		.pipe(takeTurn("Alice plays y1 (slot 1)"))
		.pipe(takeTurn("Bob clues green to Cathy"))
		.pipe(takeTurn("Cathy plays r1", "g3"))
		.pipe(takeTurn("Alice clues red to Bob"))

		// Bob's slot 1 should be known trash.
		assert(game.common.thinksTrash(game, Player.Bob.ordinal).contains(game.state.hands(Player.Bob.ordinal)(0)))
		hasInfs(game, None, Bob, 1, Vector("r1"))

	test("it interprets a reveal through an unknown card"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r2", "y2", "g2", "b2", "b4"),
			Vector("r4", "y4", "g4", "b4", "p4"),
		),
			init = preClue(Bob, 5, Seq("4"))
		)
		.pipe(takeTurn("Alice clues 2 to Bob"))					// locked
		.pipe(takeTurn("Bob clues purple to Alice (slot 5)"))	// ref play on slot 4
		.pipe(takeTurn("Cathy clues green to Bob"))

		// We should play slot 4 as g1, instead of trying to play slot 3 to sacrifice b4.
		hasStatus(game, Player.Alice, 3, CardStatus.None)
		hasInfs(game, None, Alice, 4, Vector("g1"))

	test("it interprets a 1-away reveal through an unknown card"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "y3", "g2", "b2", "b4"),
			Vector("r4", "y4", "g4", "b4", "p4"),
		),
			init = preClue(Bob, 5, Seq("4"))
		)
		.pipe(takeTurn("Alice clues 2 to Bob"))					// locked
		.pipe(takeTurn("Bob clues purple to Alice (slot 5)"))	// ref play on slot 4
		.pipe(takeTurn("Cathy clues green to Bob"))

		// We should play slot 4 as g1, instead of trying to play slot 3 to sacrifice b4.
		hasStatus(game, Player.Alice, 3, CardStatus.None)
		hasInfs(game, None, Alice, 4, Vector("g1"))

	test("doesn't give a false 1-away reveal"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "y4", "g1", "b4", "p4"),
			Vector("r4", "y4", "g4", "b4", "p2"),
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 2 to Cathy"))				// locked
		.pipe(takeTurn("Cathy clues 1 to Bob"))				// direct 1
		.pipe(takeTurn("Alice clues purple to Cathy"))

		// Bob thinks they can connect by playing p1, but it's actually g1.
		assertEquals(game.lastMove, Some(ClueInterp.Mistake))
