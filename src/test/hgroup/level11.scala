package tests.hgroup.level11

import cats.effect.unsafe.implicits.global

import scala_bot.basics._
import scala_bot.test.{hasInfs, hasStatus, Player, setup, takeTurn, TestVariant}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger, LogLevel}

class General extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands a bluff"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b3", "r1", "b1", "g5", "p2"),
			Vector("p1", "r4", "b5", "b2", "y4")
		),
			playStacks = Some(Vector(2, 2, 2, 2, 2))
		)
		.pipe(takeTurn("Alice clues red to Cathy"))
		.tap: g =>
			hasStatus(g, Bob, 1, CardStatus.Bluffed)
		.pipe(takeTurn("Bob plays b3", "y5"))

		hasInfs(game, None, Cathy, 2, Vector("r4"))

	test("understands a bluff even if duplicating"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g3", "r2", "b4", "b2"),
			Vector("y1", "r4", "p4", "r4"),
			Vector("b2", "g1", "p3", "r3")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues 2 to Bob"))
		.pipe(takeTurn("Bob clues red to Donald"))
		.tap: g =>
			// hasStatus(g, Cathy, 1, CardStatus.MaybeBluffed)
			hasStatus(g, Cathy, 1, CardStatus.Bluffed)
		.pipe(takeTurn("Cathy plays y1", "p5"))

		hasInfs(game, None, Donald, 4, Vector("r3"))

	test("plays into a bluff"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "r1", "y1", "g5", "p2"),
			Vector("p1", "r4", "b5", "b2", "y4")
		),
			starting = Cathy,
			playStacks = Some(Vector(2, 2, 2, 2, 2))
		)
		.pipe(takeTurn("Cathy clues blue to Bob"))
		.tap: g =>
			// hasStatus(g, Alice, 1, CardStatus.FMaybeBluffed)
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
			// hasInfs(g, None, Alice, 1, Vector("b3"))
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Play(g.state.hands(Alice.ordinal)(0)))
		.pipe(takeTurn("Alice plays r3 (slot 1)"))

		hasInfs(game, None, Bob, 1, Vector("b4"))

	test("plays into a known bluff"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y2", "g2", "g2", "b2"),
			Vector("p4", "b1", "b1", "b1", "y3")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues blue to Bob"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
			hasInfs(g, None, Alice, 1, Vector("r1", "y1", "g1", "b1", "p1"))
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Play(g.state.hands(Alice.ordinal)(0)))
		.pipe(takeTurn("Alice plays r1 (slot 1)"))

		hasInfs(game, None, Bob, 5, Vector("b2"))

	test("receives a simple bluff with rank disconnect"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "r1", "y1", "g5", "p2"),
			Vector("p1", "r4", "b5", "b2", "y4")
		),
			starting = Bob,
			playStacks = Some(Vector(1, 2, 1, 0, 0))
		)
		.pipe(takeTurn("Bob clues 3 to Alice (slot 2)"))
		.pipe(takeTurn("Cathy plays p1", "y5"))

		hasInfs(game, None, Alice, 2, Vector("r3", "g3"))

	test("receives a bluff with rank disconnect in pink"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y2", "g2", "g2", "b2"),
			Vector("i1", "b4", "r4", "g3", "y3")
		),
			starting = Bob,
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			variant = TestVariant.Pink5
		)
		.pipe(takeTurn("Bob clues 3 to Alice (slot 3)"))
		.pipe(takeTurn("Cathy plays i1", "b5"))

		hasInfs(game, None, Alice, 3, Vector("r3"))

	test("doesn't perform a certain discard when possibly bluffed"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "r4", "g2", "p5"),
			Vector("r2", "g4", "b4", "b4"),
			Vector("g1", "r2", "y4", "y4")
		),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues red to Cathy"))	// finessing r1
		.pipe(takeTurn("Alice clues 5 to Bob"))
		.pipe(takeTurn("Bob plays r1", "p4"))
		.pipe(takeTurn("Cathy discards r2", "y5"))		// layered gd on Donald

		.pipe(takeTurn("Donald clues green to Bob"))

		// hasStatus(game, Alice, 1, CardStatus.FMaybeBluffed)
		hasStatus(game, Alice, 1, CardStatus.Bluffed)
		assert(!game.findDiscardable(Alice.ordinal).contains(game.state.hands(Alice.ordinal)(0)))

class IndirectBluffs extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("infers the identity of indirect bluffs"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "r1", "y1", "g5", "g3"),
			Vector("p1", "r4", "b5", "b2", "y3")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 4)"))
		.pipe(takeTurn("Cathy plays p1", "p2"))
		.tap: g =>
			hasInfs(g, None, Alice, 4, Vector("r2"))

		.pipe(takeTurn("Alice discards g4 (slot 5)"))
		.pipe(takeTurn("Bob clues red to Alice (slots 1,5)"))
		.pipe(takeTurn("Cathy plays p2", "p3"))

		hasInfs(game, None, Alice, 1, Vector("r3"))

	test("infers the identity of bluffed connections through self"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b4", "r1", "y1", "g5"),
			Vector("p1", "r4", "b5", "b2"),
			Vector("g4", "r4", "r5", "g3")
		),
			starting = Donald,
			clueTokens = 7
		)
		.pipe(takeTurn("Donald clues red to Alice (slots 3,4)"))
		.pipe(takeTurn("Alice plays r1 (slot 4)"))
		.pipe(takeTurn("Bob clues red to Donald"))
		.pipe(takeTurn("Cathy plays p1", "g1"))

		hasInfs(game, None, Alice, 4, Vector("r3"))

	test("infers the identity of bluffed connections through others"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b4", "r1", "y1", "g5"),
			Vector("p1", "r4", "b5", "b2"),
			Vector("g4", "b2", "r3", "r1")
		),
			starting = Cathy,
			clueTokens = 7
		)
		.pipe(takeTurn("Cathy clues red to Donald"))
		.pipe(takeTurn("Donald plays r1", "p5"))
		.pipe(takeTurn("Alice discards y4 (slot 4)"))
		.pipe(takeTurn("Bob clues red to Alice (slots 2,3)"))

		.pipe(takeTurn("Cathy plays p1", "p2"))

		hasInfs(game, None, Alice, 2, Vector("r4"))
		hasInfs(game, None, Donald, 4, Vector("r3"))

	test("connects on clued cards not in prompt position"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b4", "r1", "y1", "g5"),
			Vector("p1", "r4", "b5", "b2"),
			Vector("g4", "r5", "r3", "r1")
		),
			starting = Cathy,
			clueTokens = 7
		)
		.pipe(takeTurn("Cathy clues red to Donald"))
		.pipe(takeTurn("Donald plays r1", "p5"))
		.pipe(takeTurn("Alice discards y4 (slot 4)"))
		.pipe(takeTurn("Bob clues red to Alice (slots 2,3)"))

		.pipe(takeTurn("Cathy plays p1", "p2"))

		hasInfs(game, None, Alice, 2, Vector("r4"))
		assert(game.common.links.exists {
			case Link.Promised(orders, id, _) =>
				id == game.state.expandShort("r3") &&
				orders.length == 2 && orders.contains(13) && orders.contains(14)
			case _ => false
		})

	test("doesn't connect if unncessary"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r3", "r1", "y1", "r4"),
			Vector("b4", "r4", "b1", "b3"),
			Vector("g1", "r5", "r3", "r1")
		),
			starting = Donald,
			playStacks = Some(Vector(1, 1, 0, 0, 2))
		)
		.pipe(takeTurn("Donald clues 3 to Bob"))
		.pipe(takeTurn("Alice plays b1 (slot 1)"))
		.tap: g =>
			hasInfs(g, None, Bob, 1, Vector("r3", "y3"))
		.pipe(takeTurn("Bob clues 5 to Donald"))
		.pipe(takeTurn("Cathy clues 4 to Bob"))
		.pipe(takeTurn("Donald plays g1", "y3"))

		// p4 is a valid bluff target, without requiring the 3 to be red.
		hasInfs(game, None, Bob, 1, Vector("r3", "y3"))

	test("writes the correct inferences after receiving a bluff"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("p2", "y2", "g3", "b1"),
			Vector("r4", "b1", "g5", "r2"),
			Vector("p2", "r4", "r1", "b3")
		),
			starting = Donald,
			playStacks = Some(Vector(1, 1, 0, 0, 1))
		)
		.pipe(takeTurn("Donald clues 1 to Bob"))
		.pipe(takeTurn("Alice clues yellow to Bob"))
		.pipe(takeTurn("Bob clues red to Cathy"))
		.pipe(takeTurn("Cathy clues 2 to Alice (slot 2)"))

		.pipe(takeTurn("Donald plays p2", "b2"))

		// If the 2 was blue, Donald wouldn't have played.
		hasInfs(game, None, Alice, 2, Vector("g2"))


class GuidePrinciple extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("plays over saving a playable"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p3", "r4", "p1", "b2", "y4"),
			Vector("y2", "y3", "y5", "p1", "g4")
		),
			starting = Bob,
			playStacks = Some(Vector(0, 1, 0, 0, 1))
		)
		.pipe(takeTurn("Bob clues yellow to Cathy"))
		.pipe(takeTurn("Cathy clues blue to Bob"))

		// Alice should play instead of saving y4.
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Play(game.state.hands(Alice.ordinal)(0)))

	test("understands a bluff isn't deferred by another bluff"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b4", "p2", "b5", "b3"),
			Vector("y1", "g2", "p5", "b2"),
			Vector("g1", "y5", "b1", "g5")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 2)"))	// bluff on Cathy?
		.pipe(takeTurn("Cathy clues purple to Bob"))	// demonstrates not bluff

		hasInfs(game, None, Alice, 2, Vector("r1"))
		// hasStatus(game, Donald, 1, CardStatus.MaybeBluffed)
		hasStatus(game, Donald, 1, CardStatus.Bluffed)

	test("understands a bluff isn't deferred by a finesse"):
		val game = setup(HGroup.atLevel(11), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b4", "p2", "b5", "b3"),
			Vector("y1", "g2", "p5", "b2"),
			Vector("p1", "y5", "b1", "g5")		// Same as above, but p1 in slot 1
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 2)"))	// bluff on Cathy?
		.pipe(takeTurn("Cathy clues purple to Bob"))	// demonstrates not bluff

		hasInfs(game, None, Alice, 2, Vector("r1"))
		hasStatus(game, Donald, 1, CardStatus.Finessed)
