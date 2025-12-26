package tests.reactor.stalling

import scala_bot.reactor.Reactor
import scala_bot.basics.CardStatus
import scala_bot.test.{Player, setup, takeTurn}, Player._
import scala_bot.logger.{Logger, LogLevel}
import scala.util.chaining.scalaUtilChainingOps

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
		assertEquals(game.meta(game.state.hands(Bob.ordinal)(2)).status, CardStatus.CalledToDiscard)

	test("it doesnt react to a cathy play"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g4"),
			Vector("g4", "p4", "p1", "b4", "g5"),
		))
		.pipe(takeTurn("Alice clues blue to Cathy"))

		// Bob's slot 1 should not called to discard, as this is an allowable play clue on turn 1.
		assertEquals(game.meta(game.state.hands(Bob.ordinal)(0)).status, CardStatus.None)

	test("it reacts to cathy 1s"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g1"),
			Vector("g4", "p4", "p1", "b1", "g5"),
		))
		.pipe(takeTurn("Alice clues 1 to Cathy"))

		// Bob's slot 5 is called to play, since colour can be given to Cathy.
		assertEquals(game.meta(game.state.hands(Bob.ordinal)(4)).status, CardStatus.CalledToPlay)

	test("it doesnt react to untargetable cathy 1s"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g1"),
			Vector("g4", "p4", "p1", "p3", "g5"),
		))
		.pipe(takeTurn("Alice clues 1 to Cathy"))

		// Bob's slot 5 is not called to play, since colour can't given to Cathy.
		assertEquals(game.meta(game.state.hands(Bob.ordinal)(4)).status, CardStatus.None)
