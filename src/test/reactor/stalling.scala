package tests.reactor

import scala_bot.reactor.Reactor
import scala_bot.basics.CardStatus
import scala_bot.test.{hasStatus, Player, setup, takeTurn}, Player._

import scala_bot.utils.pipe
import scala_bot.logger.{Logger, LogLevel}

class Stalling extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("it understands a bad play"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g4"),
			Vector("g1", "p4", "p4", "b4", "g4"),
		))
		.pipe(takeTurn("Alice clues blue to Cathy"))

		// Bob's slot 3 should be called to discard, as p4 is not playable.
		hasStatus(game, Bob, 3, CardStatus.CalledToDiscard)

	test("it doesnt react to a cathy play"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g4"),
			Vector("g4", "p4", "p1", "b4", "g5"),
		))
		.pipe(takeTurn("Alice clues blue to Cathy"))

		// Bob's slot 1 should not called to discard, as this is an allowable play clue on turn 1.
		hasStatus(game, Bob, 1, CardStatus.None)

	test("it reacts to cathy 1s"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g1"),
			Vector("g4", "p4", "p1", "b1", "g5"),
		))
		.pipe(takeTurn("Alice clues 1 to Cathy"))

		// Bob's slot 5 is called to play, since colour can be given to Cathy.
		hasStatus(game, Bob, 5, CardStatus.CalledToPlay)

	test("it doesnt react to untargetable cathy 1s"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g1"),
			Vector("g4", "p4", "p1", "p3", "g5"),
		))
		.pipe(takeTurn("Alice clues 1 to Cathy"))

		// Bob's slot 5 is not called to play, since colour can't given to Cathy.
		hasStatus(game, Bob, 5, CardStatus.None)

	test("it reacts when a direct play is available"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "g5", "y4", "y1", "g4"),
			Vector("r3", "r3", "y3", "y3", "b3"),
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 5 to Bob"))

		// Even though green to Bob is unavailable, 1 to Bob is, so Alice should react.
		hasStatus(game, Alice, 3, CardStatus.CalledToPlay)

	test("it reacts at 8 clues when giver is loaded"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "g5", "r3", "y4", "g4"),
			Vector("r1", "r3", "y3", "y3", "b3"),
		))
		.pipe(takeTurn("Alice clues blue to Cathy"))
		.pipe(takeTurn("Bob discards y4", "p1"))		// Cathy loaded on r1
		.pipe(takeTurn("Cathy clues 5 to Bob"))

		// Even though Cathy is at 8 clues, she is loaded, so Alice should react.
		hasStatus(game, Alice, 2, CardStatus.CalledToPlay)
