package tests.reactor.ptd

import scala_bot.basics._
import scala_bot.test.{Player, setup, takeTurn}, Player._
import scala_bot.reactor.Reactor
import scala_bot.logger.{Logger,LogLevel}

import scala.util.chaining.scalaUtilChainingOps

class Ptd extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("it recognizes ptd"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y4", "y4", "b4", "b4", "p4"),
			Vector("y1", "r4", "r4", "g4", "g4"),
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Cathy"))
		.pipe(takeTurn("Cathy plays y1", "b5"))

		// Alice has ptd.
		assert(game.hasPtd)

	test("it recognizes loaded ptd"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p5", "r4", "y1", "g4", "g4"),
			Vector("r5", "b5", "y4", "y3", "p4"),
		),
			starting = Cathy,
			playStacks = Some(Vector(0, 1, 0, 0, 0))
		)
		.pipe(takeTurn("Cathy clues red to Bob"))
		.pipe(takeTurn("Alice plays r1 (slot 4)"))	// targeting Bob's trash in slot 3
		.pipe(takeTurn("Bob clues 5 to Cathy"))
		.pipe(takeTurn("Cathy discards y4", "b4"))

		// Since Bob is loaded on y1 dc, Alice has ptd.
		assert(game.hasPtd)

	test("it recognizes ptd when Bob has trash on chop"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p1", "y4", "b4", "b4", "p4"),
			Vector("y1", "r4", "r4", "g4", "g4"),
		),
			starting = Bob,
			playStacks = Some(Vector(0, 0, 0, 0, 1))
		)
		.pipe(takeTurn("Bob clues red to Cathy"))
		.pipe(takeTurn("Cathy plays y1", "b5"))

		assert(game.hasPtd)

	test("it recognizes no ptd when Bob has a critical on chop"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y5", "y4", "b4", "b4", "p4"),
			Vector("y1", "r4", "r4", "g4", "g4"),
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Cathy"))
		.pipe(takeTurn("Cathy plays y1", "b5"))

		assert(!game.hasPtd)

	test("it recognizes no ptd when Bob has a duplicated 2 on chop"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p2", "y4", "b4", "b4", "p4"),
			Vector("y1", "r4", "r4", "g4", "p2"),
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Cathy"))
		.pipe(takeTurn("Cathy plays y1", "b5"))

		assert(!game.hasPtd)

	test("it recognizes no ptd when Bob has a copy of Zelda's unknown play"):
		val game = setup(Reactor.apply, Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y1", "y4", "b4", "b4", "p4"),
			Vector("y1", "r4", "r4", "g4", "p2"),
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Cathy"))
		.pipe(takeTurn("Cathy plays y1", "b5"))

		assert(!game.hasPtd)
