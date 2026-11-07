package tests.hgroup.level13

import cats.effect.unsafe.implicits.global

import scala_bot.basics._
import scala_bot.test.{hasInfs, hasStatus, Player, setup, takeTurn, TestVariant}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger, LogLevel}

class General extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands a 3-bluff"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "r1", "b1", "g5", "p2"),
			Vector("p1", "r3", "b5", "b2", "y4")
		))
		.pipe(takeTurn("Alice clues red to Cathy"))
		.tap: g =>
			hasStatus(g, Bob, 1, CardStatus.Bluffed)
			hasInfs(g, None, Cathy, 2, Vector("r1", "r2", "r3"))
		.pipe(takeTurn("Bob plays b1 (slot 1)", "y5"))

		hasInfs(game, None, Cathy, 2, Vector("r2", "r3"))

	test("plays into a 3-bluff"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b3", "r1", "y1", "g5", "p2"),
			Vector("p1", "r4", "b5", "b2", "y4")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues blue to Bob"))
		.tap: g =>
			// hasStatus(g, Alice, 1, CardStatus.FMaybeBluffed)
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
			// hasInfs(g, None, Alice, 1, Vector("b1"))
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Play(g.state.hands(Alice.ordinal)(0)))
		.pipe(takeTurn("Alice plays r1 (slot 1)"))

		hasInfs(game, None, Bob, 1, Vector("b2", "b3"))

	test("plays into a known 3-bluff"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y2", "g2", "g2", "b3"),
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

		hasInfs(game, None, Bob, 5, Vector("b2", "b3"))

	test("receives a simple 3-bluff with rank disconnect"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "r1", "y1", "g5", "p2"),
			Vector("p1", "r4", "b5", "b2", "y4")
		),
			starting = Bob,
			playStacks = Some(Vector(1, 2, 1, 0, 0))
		)
		.pipe(takeTurn("Bob clues 3 to Alice (slot 2)"))
		.pipe(takeTurn("Cathy plays p1", "y5"))

		hasInfs(game, None, Alice, 2, Vector("r3", "g3", "b3", "p3"))

	test("understands an unknown 3-bluff by colour"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y2", "g2", "g2", "b3"),
			Vector("p4", "b4", "r2", "b1", "y3")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues blue to Bob"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
		.pipe(takeTurn("Alice plays y1 (slot 1)"))

		// After playing a non-connected card, we know it must be a bluff.
		hasInfs(game, None, Bob, 5, Vector("b2", "b3"))
		hasStatus(game, Alice, 2, CardStatus.None)

	test("understands an unknown 3-bluff by rank"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y2", "g2", "g2", "b3"),
			Vector("p4", "b4", "r2", "b1", "y3")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 3 to Bob"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
		.pipe(takeTurn("Alice plays y1 (slot 1)"))

		hasInfs(game, None, Bob, 5, Vector("r3", "y3", "g3", "b3", "p3"))
		hasStatus(game, Alice, 2, CardStatus.None)

	test("understands a known hard self 3-bluff"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y2", "g2", "g4", "b4"),
			Vector("p4", "b4", "r2", "b1", "y3")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 3 to Alice (slot 2)"))
		.tap: g =>
			// Assume a bluff over two blind finesse plays.
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
			hasInfs(g, None, Alice, 2, Vector("r3", "y3", "g3", "b3", "p3"))
		.pipe(takeTurn("Alice plays y1 (slot 1)"))

		// Since we assume a bluff, we don't know which 3 it is.
		hasInfs(game, None, Alice, 2, Vector("r3", "y3", "g3", "b3", "p3"))
		hasStatus(game, Alice, 3, CardStatus.None)

	test("understands an unknown hard 3-bluff"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y2", "g2", "g4", "b3"),
			Vector("p4", "b4", "r2", "b1", "y3")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 3 to Bob"))
		.tap: g =>
			// Assume a bluff over two blind finesse plays.
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
			hasInfs(g, None, Bob, 5, Vector("r3", "y3", "g3", "b3", "p3"))
		.pipe(takeTurn("Alice plays b1 (slot 1)"))

		// Since we assume a bluff, we don't know which 3 it is.
		hasInfs(game, None, Bob, 5, Vector("r3", "y3", "g3", "b3", "p3"))

	test("understands an unknown critical colour bluff"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y2", "g2", "b4", "g3"),
			Vector("p4", "b1", "b3", "b1", "y3")
		),
			starting = Cathy,
			discarded = Vector("b4")
		)
		.pipe(takeTurn("Cathy clues blue to Bob"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
		.pipe(takeTurn("Alice plays y1 (slot 1)"))

		hasInfs(game, None, Bob, 4, Vector("b2", "b3", "b4"))

	test("understands a critical colour finesse when already clued"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y2", "b4", "r3", "p4"),
			Vector("b3", "g4", "p2", "y4"),
			Vector("p5", "b1", "b1", "y3")
		),
			starting = Cathy,
			discarded = Vector("b4", "p4")
		)
		// Critical save.
		.pipe(takeTurn("Cathy clues 4 to Bob"))
		.pipe(takeTurn("Donald clues blue to Bob"))

		// Since the critical card was not newly touched, this must be a finesse.
		hasStatus(game, Alice, 1, CardStatus.Finessed)
		hasInfs(game, None, Alice, 1, Vector("b1"))
		hasInfs(game, None, Alice, 2, Vector("b2"))
		hasInfs(game, None, Cathy, 1, Vector("b3"))

	test("understands an unknown critical colour bluff (black suit)"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y2", "g2", "k4", "g3"),
			Vector("b4", "b1", "b3", "b1", "y3")
		),
			starting = Cathy,
			variant = TestVariant.Black5
		)
		.pipe(takeTurn("Cathy clues black to Bob"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
		.pipe(takeTurn("Alice plays y1 (slot 1)"))

		hasInfs(game, None, Bob, 4, Vector("k2", "k3", "k4"))

	test("understands a critical rank finesse"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y2", "g2", "b4", "g3"),
			Vector("p4", "b1", "b3", "b1", "y3")
		),
			starting = Cathy,
			discarded = Vector("b4")
		)
		.pipe(takeTurn("Cathy clues 4 to Bob"))

		// A critical bluff cannot be given by rank. This must be a finesse.
		hasInfs(game, None, Alice, 1, Vector("b1"))
		hasStatus(game, Alice, 1, CardStatus.Finessed)
		hasInfs(game, None, Alice, 1, Vector("b1"))
		hasInfs(game, None, Alice, 2, Vector("b2"))
		hasInfs(game, None, Alice, 3, Vector("b3"))


	test("understands giving a critical colour bluff"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "r1", "b4", "g5", "p2"),
			Vector("p1", "r4", "b5", "b2", "y4")
		),
			discarded = Vector("r4")
		)
		.pipe(takeTurn("Alice clues red to Cathy"))
		.tap: g =>
			hasStatus(g, Bob, 1, CardStatus.Bluffed)
			hasInfs(g, None, Cathy, 2, Vector("r1", "r2", "r3", "r4"))
		.pipe(takeTurn("Bob plays b1", "y5"))

		hasInfs(game, None, Cathy, 2, Vector("r2", "r3", "r4"))

	test("still understands a 3-finesse by colour when the play connects"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y2", "g2", "g2", "b3"),
			Vector("p4", "b1", "b1", "b4", "y3")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues blue to Bob"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
		.pipe(takeTurn("Alice plays b1 (slot 1)"))

		// After playing a connecting card, we know it must be a finesse.
		hasInfs(game, None, Alice, 2, Vector("b2"))
		hasStatus(game, Alice, 2, CardStatus.Finessed)
		hasInfs(game, None, Bob, 5, Vector("b3"))

	test("still understands a 3-finesse by rank when the play connects"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b3", "y2", "g2", "g2", "r3"),
			Vector("p4", "b1", "y1", "b4", "y3")
		),
			starting = Cathy,
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Cathy clues 3 to Bob"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
			hasInfs(g, None, Bob, 5, Vector("r3", "y3", "g3", "b3", "p3"))
		.pipe(takeTurn("Alice plays r2 (slot 1)"))

		hasInfs(game, None, Bob, 5, Vector("r3"))

	test("understands a 3-bluff when the play doesn't connect"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b3", "y2", "g2", "g2", "r3"),
			Vector("p4", "b1", "y1", "b4", "y3")
		),
			starting = Cathy,
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Cathy clues 3 to Bob"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
		.pipe(takeTurn("Alice plays g1 (slot 1)"))

		hasInfs(game, None, Bob, 5, Vector("r3", "y3", "g3", "b3", "p3"))
		hasStatus(game, Alice, 2, CardStatus.None)

	test("still understands a self 3-finesse when the play connects"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b3", "y2", "g2", "g2", "b3"),
			Vector("p4", "b1", "y1", "b4", "y3")
		),
			starting = Cathy,
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Cathy clues 3 to Alice (slot 2)"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
		.pipe(takeTurn("Alice plays r2 (slot 1)"))

		hasInfs(game, None, Alice, 2, Vector("r3"))

	test("understands a self 3-bluff when the possible finesse doesn't connect"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b3", "y2", "g2", "g2", "b3"),
			Vector("p4", "b1", "y1", "b4", "y3")
		),
			starting = Cathy,
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Cathy clues 3 to Alice (slot 2)"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
		.pipe(takeTurn("Alice plays y1 (slot 1)"))

		hasInfs(game, None, Alice, 2, Vector("r3", "y3", "g3", "b3", "p3"))
		hasStatus(game, Alice, 3, CardStatus.None)

	test("still understands a clandestine 3-finesse when the play connects"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b3", "g4", "g2", "g2", "y3"),
			Vector("p4", "b1", "y1", "b4", "y3")
		),
			starting = Cathy,
			playStacks = Some(Vector(1, 1, 0, 0, 0))
		)
		.pipe(takeTurn("Cathy clues 3 to Bob"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
		.pipe(takeTurn("Alice plays r2 (slot 1)"))

		hasInfs(game, None, Alice, 2, Vector("y2"))

	test("still understands a clandestine 3-finesse when the play connects (2 away)"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b3", "g4", "g2", "g2", "y3"),
			Vector("p4", "b1", "y1", "b4", "y3")
		),
			starting = Cathy,
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Cathy clues 3 to Bob"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
		.pipe(takeTurn("Alice plays r2 (slot 1)"))

		hasInfs(game, None, Alice, 2, Vector("y1"))
		hasInfs(game, None, Alice, 3, Vector("y2"))

	test("still understands receiving a clandestine 3-finesse when the play connects"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p3", "p4", "g2", "g2", "p4"),
			Vector("r2", "y1", "y2", "g4", "b4")
		),
			starting = Bob,
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Bob clues 3 to Alice (slot 2)"))
		.pipe(takeTurn("Cathy plays r2", "r5"))

		hasInfs(game, None, Alice, 2, Vector("r3", "y3"))

	test("does not interpret colour connect with rank disconnect as a 3-bluff"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "g2", "g4", "y4", "b4"),
			Vector("g5", "y5", "b3", "p3", "y3")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues red to Bob"))
		.pipe(takeTurn("Alice plays r1 (slot 1)"))

		// Since red connects to red, Alice must assume a double finesse (she also has r2 on slot 2), not a 3-bluff.
		hasStatus(game, Alice, 2, CardStatus.Finessed)
		hasInfs(game, None, Alice, 2, Vector("r2"))
		hasInfs(game, None, Bob, 1, Vector("r3"))

	test("still understands receiving colour connect with rank disconnect"):
		val game = setup(HGroup.atLevel(13), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r5", "g2", "g4", "y4", "b4"),
			Vector("r1", "r2", "b3", "p3", "y3")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 1)"))
		.pipe(takeTurn("Cathy plays r1", "b4"))

		// Alice must wait for a double finesse.
		hasStatus(game, Cathy, 2, CardStatus.Finessed)
		hasInfs(game, None, Cathy, 2, Vector("r2"))
		hasInfs(game, None, Alice, 1, Vector("r2", "r3"))
