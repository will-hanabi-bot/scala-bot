package tests.hgroup.level2

import scala_bot.basics._
import scala_bot.test.{Colour, hasInfs, hasStatus, Player, setup, takeTurn}, Player._
import scala_bot.hgroup.HGroup
import scala_bot.logger.{Logger, LogLevel}

import scala.util.chaining.scalaUtilChainingOps

class General extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands a continuing finesse"):
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y5", "r5", "b1", "p3"),
			Vector("p4", "b3", "r2", "p1"),
			Vector("r3", "r1", "y4", "p1"))
		)
		.pipe(takeTurn("Alice clues 1 to Donald"))
		.pipe(takeTurn("Bob clues red to Cathy"))				// r2 connection
		.pipe(takeTurn("Cathy clues 4 to Alice (slots 2,3)"))	// r4, finessing Donald
		.pipe(takeTurn("Donald clues red to Bob"))				// r5

		assert(game.common.thoughts(game.state.hands(Alice.ordinal)(0)).inferred.nonEmpty)
		assertEquals(game.common.hypoStacks(Colour.Red.ordinal), 5)

	test("self-prompts if impossible to be direct"):
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "g1", "y2", "g4", "b4"),
			Vector("r1", "g3", "b3", "r5", "m3")
		),
			starting = Cathy,
			playStacks = Some(Vector(0, 0, 1, 0, 0)),
			variant = "Rainbow (5 Suits)"
		)
		.pipe(takeTurn("Cathy clues 2 to Alice (slots 1,2)"))
		.pipe(takeTurn("Alice plays g2 (slot 1)"))
		.pipe(takeTurn("Bob clues red to Alice (slots 2,5)"))	// slot 5 is !2, thus [r1,m1,r3]
		.tap: g =>
			hasInfs(g, None, Alice, 5, Vector("r1", "m1", "r3"))
		.pipe(takeTurn("Cathy plays r1", "b1"))

		hasInfs(game, None, Alice, 5, Vector("r3"))

	test("recognizes known finesses"):
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "r4", "r4", "g4", "g4"),
			Vector("y4", "y4", "b4", "b4", "r3")
		),
			starting = Cathy,
			discarded = Vector("r1", "r1")
		)
		.pipe(takeTurn("Cathy clues red to Alice (slot 1)"))

		// Bob's r1 is definitely finessed.
		assert(!game.xmeta(game.state.hands(Bob.ordinal)(0)).maybeFinessed)
		hasStatus(game, Bob, 1, CardStatus.Finessed)

		// Alice knows that her card is exactly r2.
		hasInfs(game, Some(Alice), Alice, 1, Vector("r2"))

	test("doesn't self-prompt when a clue could be direct"):
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g5", "b4", "r4", "r5", "g2"),
			Vector("b2", "y3", "r4", "p2", "p3")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues blue to Alice (slots 4,5)"))
		.pipe(takeTurn("Alice plays b1 (slot 5)"))
		.pipe(takeTurn("Bob clues blue to Alice (slots 1,5)"))

		// Alice's slot 1 should only be [b2,b3], not [b2,b3,b4].
		hasInfs(game, Some(Alice), Alice, 1, Vector("b2", "b3"))

	test("waits for a reverse finesse even when it could be direct"):
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g1", "b2", "y4", "r3", "r5"),
			Vector("g5", "y3", "r4", "p2", "r3")
		),
			starting = Cathy,
			playStacks = Some(Vector(0, 2, 0, 0, 0))
		)
		.pipe(takeTurn("Cathy clues 3 to Alice (slot 2)"))

		// While Alice's slot 2 could be y3, it could also be g3 (reverse finesse on Bob + self-finesse).
		hasInfs(game, None, Alice, 2, Vector("y3", "g3"))

	test("assumes direct play over a 'stomped' finesse involving a self-component"):
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r5", "b2", "y4", "r3"),
			Vector("g5", "y3", "r4", "p2"),
			Vector("g1", "r3", "y2", "b3")
		),
			starting = Bob,
			playStacks = Some(Vector(0, 2, 0, 0, 0))
		)
		.pipe(takeTurn("Bob clues 3 to Alice (slot 4)"))
		.pipe(takeTurn("Cathy clues green to Donald"))

		// Alice should assume the simpler explanation that she doesn't have to play g2.
		hasInfs(game, None, Alice, 4, Vector("y3"))
		hasStatus(game, Alice, 1, CardStatus.None)

	test("allows connecting through multiple possibilities"):
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g2", "b4", "g3", "r4"),
			Vector("g4", "y3", "r4", "p2"),
			Vector("g1", "g5", "y1", "b4")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 1 to Donald"))
		.pipe(takeTurn("Cathy clues 3 to Bob"))

		// There should be y2 -> y3 and g2 -> g3 waiting connections.
		assert(game.waiting.exists(_.inference == Identity(Colour.Yellow.ordinal, 3)))
		assert(game.waiting.exists(_.inference == Identity(Colour.Green.ordinal, 3)))

	test("prefers not starting with self, even with known playables before"):
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y3", "b2", "y4", "r3"),
			Vector("g1", "y3", "r4", "p2"),
			Vector("g2", "p4", "y5", "b4")
		),
			playStacks = Some(Vector(0, 2, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues green to Donald"))
		.pipe(takeTurn("Bob clues 4 to Alice (slot 2)"))
		.pipe(takeTurn("Cathy plays g1", "r1"))
		.pipe(takeTurn("Donald plays g2", "b1"))

		hasInfs(game, None, Alice, 2, Vector("y4"))

		// Alice's slot 1 is not playable (yet).
		assert(game.common.thinksPlayables(game, Alice.ordinal).isEmpty)

	test("prefers not starting with self (symmetrically), even with known playables before"):
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g2", "y2", "r4", "p2"),
			Vector("g5", "p4", "y5", "y3"),
			Vector("b2", "b2", "y4", "r3")
		),
			starting = Bob,
			playStacks = Some(Vector(0, 2, 1, 0, 0)),
			discarded = Vector("r3", "y3")
		)
		.pipe(takeTurn("Bob clues 3 to Cathy"))
		.pipe(takeTurn("Cathy clues 5 to Alice (slot 4)"))
		.pipe(takeTurn("Donald clues green to Bob"))
		.pipe(takeTurn("Alice clues 4 to Donald"))

		// g4 (g2 known -> g3 finesse, self) requires a self-component, compared to y3 (prompt) which does not.
		hasInfs(game, None, Donald, 3, Vector("y4"))

	test("includes the correct interpretation, even if it requires more blind plays"):
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g2", "g3", "g4", "r3"),
			Vector("g4", "y3", "r4", "p2"),
			Vector("g1", "b4", "y5", "y2")
		),
			starting = Bob,
			playStacks = Some(Vector(0, 1, 0, 0, 0))
		)
		.pipe(takeTurn("Bob clues 2 to Donald"))
		.pipe(takeTurn("Cathy clues 4 to Bob"))

		// There should be y2 -> y3 and g2 -> g3 -> g4 waiting connections.
		assert(game.waiting.exists(_.inference == Identity(Colour.Yellow.ordinal, 4)))
		assert(game.waiting.exists(_.inference == Identity(Colour.Green.ordinal, 4)))

	test("connects when a card plays early"):
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("p5", "y4", "r1", "b3"),
			Vector("p1", "g5", "y3", "y1"),
			Vector("g1", "p2", "y1", "b5")
		),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues 2 to Alice (slot 4)"))
		.pipe(takeTurn("Alice clues 1 to Donald"))
		.pipe(takeTurn("Bob clues 3 to Cathy"))
		.pipe(takeTurn("Cathy clues 5 to Donald"))

		.pipe(takeTurn("Donald plays y1", "r4"))

		// Note at level 5, Alice can't play y2 since it could be a hidden finesse.
		.pipe(takeTurn("Alice plays y2 (slot 4)"))

		hasInfs(game, None, Cathy, 3, Vector("y3"))

	test("connects to a finesse after a fake finesse was just disproven"):
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b2", "y4", "r1", "b3", "y1"),
			Vector("g1", "y3", "p4", "y1", "b5")
		),
			starting = Cathy,
			playStacks = Some(Vector(2, 1, 1, 1, 2))
		)
		.pipe(takeTurn("Cathy clues 3 to Alice (slots 2,5)"))
		.tap:
			hasInfs(_, None, Alice, 5, Vector("r3", "b3", "p3"))
		.pipe(takeTurn("Alice clues 5 to Cathy"))
		.pipe(takeTurn("Bob clues purple to Cathy"))
		.tap: g =>
			// Alice's slot 2 can be any 3 (not prompted to be p3).
			hasInfs(g, None, Alice, 5, Vector("p3"))
			hasInfs(g, None, Alice, 2, Vector("r3", "y3", "g3", "b3", "p3"))
		.pipe(takeTurn("Cathy discards y1", "p1"))
		.pipe(takeTurn("Alice plays r3 (slot 5)"))

		// p3 must be in slot 3 (previously slot 2).
		hasInfs(game, None, Alice, 3, Vector("p3"))

	test("doesn't consider already-finessed possibilities"):
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r2", "y4", "r1", "b5"),
			Vector("r3", "y3", "p4", "y1"),
			Vector("y1", "b3", "b2", "r4"),
			Vector("p4", "p1", "r1", "y2")
		),
			starting = Donald,
			playStacks = Some(Vector(0, 1, 1, 0, 0))
		)
		.pipe(takeTurn("Donald clues red to Emily"))	// known r1
		.pipe(takeTurn("Emily clues red to Donald"))	// r4 double finesse
		.pipe(takeTurn("Alice clues 5 to Bob"))
		.pipe(takeTurn("Bob clues 2 to Alice (slot 2)"))

		// Alice's slot 2 shouldn't be r2.
		hasInfs(game, None, Alice, 2, Vector("y2", "g2"))

	test("doesn't give continuation clues when cards are in a superposition"):
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "r1", "p5", "r2"),
			Vector("r3", "g3", "p2", "p1"),
			Vector("b4", "y2", "r4", "p1")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues red to Alice (slots 3,4)"))
		.pipe(takeTurn("Donald clues purple to Cathy"))
		.pipe(takeTurn("Alice clues red to Donald"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	/** TODO: Investigate more. Currently Alice isn't considering having b2. */
	test("correctly identifies the simplest connection when a prompt makes the difference"):
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b3", "y3", "p4", "g1"),
			Vector("g1", "r3", "p3", "r5"),
			Vector("b1", "g3", "y2", "p4")
		),
			starting = Donald,
			playStacks = Some(Vector(0, 0, 0, 1, 0))
		)
		.pipe(takeTurn("Donald clues 2 to Alice (slots 1,3)"))	// g2, b2
		.pipe(takeTurn("Alice clues 5 to Cathy"))
		.pipe(takeTurn("Bob clues 3 to Donald"))	// confirming we have g2
		.pipe(takeTurn("Cathy plays g1", "b1"))
		.pipe(takeTurn("Donald clues 4 to Alice (slot 2)"))

		hasInfs(game, None, Alice, 2, Vector("g4"))
