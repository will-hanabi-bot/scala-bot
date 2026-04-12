package tests.hgroup

import cats.effect.unsafe.implicits.global

import scala_bot.basics._
import scala_bot.test.{fullyKnown, hasInfs, hasStatus, Player, setup, takeTurn}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger, LogLevel}

class DupeTech extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands a simple duplicitous layered finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "r1"),
			Vector("r1", "y1", "b4", "b4", "g4")
		),
			starting = Bob,
			init = fullyKnown(Bob, 5, "r1")
		)
		.pipe(takeTurn("Bob clues yellow to Alice (slot 5)"))
		.pipe(takeTurn("Cathy plays r1", "g4"))

		hasInfs(game, None, Alice, 5, Vector("y2"))

	test("receives a simple duplicitous layered finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p4", "p4", "b4", "b4", "y2"),
			Vector("r4", "r4", "y4", "y4", "r1")
		),
			starting = Cathy,
			init = fullyKnown(Cathy, 5, "r1")
		)
		.pipe(takeTurn("Cathy clues yellow to Bob"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Finessed)
		.pipe(takeTurn("Alice plays r1 (slot 1)"))

		hasStatus(game, Alice, 2, CardStatus.Finessed)

	test("gives a simple duplicitous layered finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "y1", "y4", "y4", "g4"),
			Vector("r4", "r4", "b4", "b4", "y2")
		),
			init = fullyKnown(Alice, 5, "r1")
		)
		.pipe(takeTurn("Alice clues yellow to Cathy (slot 5)"))

		assertEquals(game.lastMove, Some(ClueInterp.Play))
		hasStatus(game, Bob, 1, CardStatus.Finessed)

	test("understands a duplicitous layered finesse where the giver's card is unknown"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b4", "r4", "r4", "r1"),
			Vector("r1", "y1", "y4", "y4", "g4")
		))
		.pipe(takeTurn("Alice clues red to Bob"))
		.pipe(takeTurn("Bob clues yellow to Alice (slot 5)"))
		.pipe(takeTurn("Cathy plays r1", "g4"))

		hasInfs(game, None, Alice, 5, Vector("y2"))

	test("doesn't save a potentially duplicated 2"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "b4", "r4", "r4", "r2"),
			Vector("g4", "g4", "y4", "y4", "p4")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 2 to Alice (slots 4,5)"))
		.pipe(takeTurn("Cathy clues blue to Bob"))

		assert(game.takeAction.unsafeRunSync().isInstanceOf[PerformAction.Discard])

	test("doesn't save a potentially duplicated 1"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "b4", "r4", "r4", "r1"),
			Vector("g4", "g4", "y4", "y4", "p4")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slots 4,5)"))
		.pipe(takeTurn("Cathy clues blue to Bob"))

		assertEquals(game.common.hypoStacks, Vector(0, 0, 0, 1, 0))
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Play(game.state.hands(Alice.ordinal)(4)))
