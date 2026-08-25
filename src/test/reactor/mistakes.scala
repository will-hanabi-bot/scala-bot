package tests.reactor

import scala_bot.reactor.Reactor
import scala_bot.basics._
import scala_bot.test.{hasStatus, Player, setup, takeTurn}, Player._

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger, LogLevel}

class Mistakes extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("it cancels a missed reaction 1"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g1", "r1", "g4", "b4", "b4"),
			Vector("b1", "r3", "r4", "y4", "y4"),
		))
		.pipe(takeTurn("Alice clues 4 to Cathy"))
		.tap: g =>
			// Bob is called to play r1 (slot 2) -> Cathy plays b1 (slot 1).
			hasStatus(g, Bob, 2, CardStatus.DragToPlay)
		.pipe(takeTurn("Bob discards g1", "y3"))

		val bobS2 = game.state.hands(Bob.ordinal)(1)

		// Bob is no longer called to play r1, and that card can be anything.
		assertEquals(game.meta(bobS2).status, CardStatus.None)
		assertEquals(game.common.thoughts(bobS2).inferred.length, game.common.thoughts(bobS2).possible.length)

		// Cathy is not called to play slot 1 (Cathy might have some wrong priority elim notes).
		hasStatus(game, Cathy, 1, CardStatus.None)

	test("it cancels a missed reaction 2"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g1", "r1", "g4", "b4", "b4"),
			Vector("b1", "r1", "r4", "y4", "y4"),
		))
		.pipe(takeTurn("Alice clues 4 to Cathy"))
		.tap: g =>
			// Bob is called to play r1 (slot 2) -> Cathy plays b1 (slot 1).
			hasStatus(g, Bob, 2, CardStatus.DragToPlay)
		.pipe(takeTurn("Bob plays g1", "y3"))

		val bobS2 = game.state.hands(Bob.ordinal)(1)

		// Bob is no longer called to play r1, and that card can be anything.
		assertEquals(game.meta(bobS2).status, CardStatus.None)
		assertEquals(game.common.thoughts(bobS2).inferred.length, game.common.thoughts(bobS2).possible.length)

		// Cathy is not called to play slot 1 (Cathy might have some wrong priority elim notes).
		hasStatus(game, Cathy, 1, CardStatus.None)

	test("it resets ctd after a slot 1 discard"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "r1", "b4", "b4"),
			Vector("r3", "b5", "r4", "y4", "y4"),
		))
		.pipe(takeTurn("Alice clues blue to Cathy"))
		.pipe(takeTurn("Bob plays r1", "p5"))		// 3+4 = 2
		.tap: g =>
			hasStatus(g, Cathy, 4, CardStatus.CalledToDiscard)
		.pipe(takeTurn("Cathy discards r3", "y3"))

		// Since Cathy demonstrated she doesn't know, remove the dc note.
		hasStatus(game, Cathy, 4, CardStatus.None)
