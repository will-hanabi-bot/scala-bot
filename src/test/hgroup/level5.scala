package tests.hgroup.level5

import scala_bot.basics._
import scala_bot.test.{hasInfs, hasStatus, Player, preClue, setup, takeTurn}, Player._
import scala_bot.hgroup.{FStatus, HGroup}
import scala_bot.logger.{Logger, LogLevel}

import scala.util.chaining.scalaUtilChainingOps

class General extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands a fake finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "g4", "r5", "b4"),
			Vector("g1", "b3", "r2", "y3", "p3")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues green to Alice (slot 2)"))
		.tap: g =>
			hasInfs(g, None, Alice, 2, Vector("g1", "g2"))
			assertEquals(g.meta(g.state.hands(Cathy.ordinal)(0)).reasoning, List(1))
		.pipe(takeTurn("Cathy discards p3", "r1"))

		hasInfs(game, None, Alice, 2, Vector("g1"))
		assertEquals(game.meta(game.state.hands(Cathy.ordinal)(0)).reasoning, List.empty)

	test("doesn't eliminate from a possibly-fake finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b3", "y1", "p1", "r4"),
			Vector("g4", "r1", "r3", "r2"),
			Vector("b1", "b2", "p4", "p3")
		),
			starting = Bob,
			playStacks = Some(Vector(5, 5, 5, 0, 1)),
		)
		.pipe(takeTurn("Bob clues 2 to Alice (slot 4)"))	// b2, p2 save
		.pipe(takeTurn("Cathy clues blue to Bob"))
		.pipe(takeTurn("Donald plays b1", "b1"))

		hasInfs(game, None, Alice, 4, Vector("b2", "p2"))
		hasInfs(game, None, Bob, 1, Vector("b2", "b3"))

	test("understands a self-connecting play clue"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "g4", "r5", "b4"),
			Vector("g3", "b3", "r2", "y3", "p3")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slot 4)"))	// b2, p2 save
		.pipe(takeTurn("Cathy clues 2 to Alice (slot 3)"))
		.pipe(takeTurn("Alice plays g1 (slot 4)"))

		// Note: slot 3 has moved to slot 4
		hasInfs(game, None, Alice, 4, Vector("g2"))

	test("understands a delayed finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p4", "r4", "g4", "r5", "b4"),
			Vector("r3", "b3", "r2", "y3", "p3")
		),
			playStacks = Some(Vector(1, 0, 1, 1, 0))
		)
		.pipe(takeTurn("Alice clues 2 to Cathy"))		// r2, g2, b2
		.pipe(takeTurn("Bob clues red to Alice (slot 3)"))
		.tap: g =>
			hasInfs(g, None, Alice, 3, Vector("r3", "r4"))
		.pipe(takeTurn("Cathy plays r2", "y1"))
		.tap: g =>
			hasInfs(g, None, Alice, 3, Vector("r3", "r4"))
		.pipe(takeTurn("Alice discards b1 (slot 5)"))
		.pipe(takeTurn("Bob discards b4", "r1"))
		.pipe(takeTurn("Cathy plays r3", "g1"))

		// Note: slot 3 has moved to slot 4
		hasInfs(game, None, Alice, 4, Vector("r4"))

	test("understands a fake delayed finesse"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p4", "r4", "g4", "r5", "b4"),
			Vector("r2", "b3", "r1", "y3", "p3")
		),
			clueTokens = 7
		)
		.pipe(takeTurn("Alice clues 1 to Cathy"))
		.pipe(takeTurn("Bob clues red to Alice (slot 3)"))
		.pipe(takeTurn("Cathy plays r1", "y1"))

		.pipe(takeTurn("Alice discards b1 (slot 5)"))
		.pipe(takeTurn("Bob discards b4", "r1"))
		.pipe(takeTurn("Cathy discards p3", "g1"))

		// Note: slot 3 has moved to slot 4
		hasInfs(game, None, Alice, 4, Vector("r2"))

	test("understands that a self-finesse may not be ambiguous"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g2", "p4", "r2", "r3", "g4"),
			Vector("p2", "p1", "b3", "y3", "b4")
		),
			starting = Bob,
			clueTokens = 4
		)
		.pipe(takeTurn("Bob clues 2 to Cathy"))
		.tap: g =>
			hasStatus(g, Cathy, 2, CardStatus.Finessed)
		.pipe(takeTurn("Cathy discards b4", "r4"))

		// Alice can deduce that she has a playable card on finesse position, but shouldn't play it.
		hasStatus(game, Alice, 1, CardStatus.None)

		// Cathy should still be finessed (slot 2 has moved to slot 3).
		hasStatus(game, Cathy, 3, CardStatus.Finessed)

	test("still finesses if cards in the finesse are clued, as long as they weren't the original finesse target"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "b1", "g3", "r1"),
			Vector("p5", "g3", "p1", "b3"),
			Vector("p1", "b1", "r3", "g1")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues blue to Cathy"))		// p1, b1 layer -> b2 on us
		.pipe(takeTurn("Cathy clues 5 to Alice (slot 4)"))
		.pipe(takeTurn("Donald plays p1", "b4"))
		.pipe(takeTurn("Alice clues 1 to Donald"))		// getting g1, but touches b1

		.pipe(takeTurn("Bob clues blue to Donald"))		// focusing b4, but filling in b1
		.pipe(takeTurn("Cathy discards p1", "g4"))
		.pipe(takeTurn("Donald plays b1", "b5"))

		// Alice's b2 in slot 1 should still be finessed.
		hasStatus(game, Alice, 1, CardStatus.Finessed)
		hasInfs(game, None, Alice, 1, Vector("b2"))

		hasInfs(game, None, Donald, 2, Vector("b4"))

	test("cancels finesse connections when clued elsewhere"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "y5", "g3", "y3"),
			Vector("p5", "g3", "p1", "b3"),
			Vector("p4", "y2", "r3", "g1")
		),
			playStacks = Some(Vector(1, 1, 0, 2, 0))
		)
		.pipe(takeTurn("Alice clues yellow to Donald"))
		.pipe(takeTurn("Bob clues 4 to Alice (slot 2)"))	// Could be y4, b4
		.pipe(takeTurn("Cathy clues yellow to Bob"))		// Focusing y3

		// Alice's slot 1 should be [r2, b3, p1] (not g1 since both g3s visible).
		hasStatus(game, Alice, 1, CardStatus.Finessed)
		hasInfs(game, None, Alice, 1, Vector("r2", "b3", "p1"))

	test("doesn't confirm symmetric finesses after a stomped play"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "p2", "b2", "p2"),
			Vector("y1", "g4", "g3", "y4"),
			Vector("y1", "g1", "y3", "r4")
		),
			starting = Cathy,
			clueTokens = 7,
			playStacks = Some(Vector(0, 0, 3, 1, 0))
		)
		.pipe(takeTurn("Cathy clues yellow to Donald"))
		.pipe(takeTurn("Donald plays y1", "p1"))

		.pipe(takeTurn("Alice discards g1 (slot 4)"))
		.pipe(takeTurn("Bob clues 4 to Cathy"))			// y2 finesse on us, y3 prompt, y4 (p4 symmetric)
		.pipe(takeTurn("Cathy clues 1 to Donald"))
		.pipe(takeTurn("Donald plays p1", "p1"))

		// Alice's y2 should still be finessed
		hasStatus(game, Alice, 1, CardStatus.Finessed)
		hasInfs(game, None, Alice, 1, Vector("y2"))

	test("eliminates all finesse possibilities when a player doesn't play"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b5", "r4", "r1", "r1"),
			Vector("y4", "m4", "b4", "b2"),
			Vector("m3", "g1", "y1", "b1")
		),
			starting = Donald,
			clueTokens = 7,
			playStacks = Some(Vector(0, 2, 0, 0, 1)),
			variant = "Rainbow (5 Suits)"
		)
		.pipe(takeTurn("Donald clues green to Alice (slots 2,3,4)"))
		.pipe(takeTurn("Alice plays m2 (slot 4)"))
		.pipe(takeTurn("Bob discards r1 (slot 4)", "b1"))
		.pipe(takeTurn("Cathy clues blue to Donald"))
		.tap: g =>
			// Clue could be b1 finesse (Bob) -> b2 (Donald)
			assert(g.waiting.exists(wc => wc.inference.matches(Identity(3, 2)) && wc.target == Donald.ordinal))
		.pipe(takeTurn("Donald clues 3 to Alice (slots 2,3)"))	// b1 reverse + self composition, or y3 direct
		.tap: g =>
			hasInfs(g, None, Alice, 2, Vector("y3", "b3"))
		.pipe(takeTurn("Alice clues green to Donald"))
		.pipe(takeTurn("Bob clues red to Cathy"))

		// After Bob doesn't play, both b1 and y3 should be known.
		hasInfs(game, None, Alice, 2, Vector("y3"))
		hasInfs(game, None, Donald, 4, Vector("b1"))

	test("interprets a delayed play through rainbow identities"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g2", "r1", "b2", "y3"),
			Vector("r2", "y4", "b3", "r3"),
			Vector("m4", "m2", "r4", "g1")
		),
			playStacks = Some(Vector(0, 0, 0, 0, 1)),
			variant = "Rainbow (5 Suits)"
		)
		.pipe(takeTurn("Alice clues 2 to Donald"))		// m2
		.pipe(takeTurn("Bob clues green to Donald"))	// g1, could be m3
		.pipe(takeTurn("Cathy clues green to Bob"))		// g2

		// Alice should not be finessed.
		hasStatus(game, Alice, 1, CardStatus.None)

	test("identifies maybe finessed cards"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "y4", "g4", "b4"),
			Vector("r4", "y4", "g4", "b4"),
			Vector("r4", "g5", "p4", "p4")
		),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues 2 to Alice (slot 2)"))

		// Bob might be finessed for r1, but we aren't sure.
		assertEquals(game.xmeta(game.state.hands(Bob.ordinal)(0)).fStatus, Some(FStatus.PossiblyOn))

	test("recognizes certainly finessed cards"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "r2", "g4", "b4"),
			Vector("r4", "y4", "g4", "b4"),
			Vector("r4", "g5", "p4", "p4")
		),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues 3 to Alice (slots 1,2,3,4)"))

		// Bob must be finessed for r1.
		assertEquals(game.xmeta(game.state.hands(Bob.ordinal)(0)).fStatus, None)
		hasInfs(game, None, Alice, 4, Vector("r3"))

	test("cancels a prompt after the reacting player stalls"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p5", "y4", "p4", "p2", "g3"),
			Vector("b4", "y2", "y1", "b3", "r2")
		),
			starting = Bob,
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			clueTokens = 4
		)
		.pipe(takeTurn("Bob clues 2 to Cathy"))
		.pipe(takeTurn("Cathy clues yellow to Alice (slots 4,5)"))
		.pipe(takeTurn("Alice plays y1 (slot 5)"))

		.pipe(takeTurn("Bob clues 3 to Alice (slots 1,5)"))		// x3 xx xx xx y3
		.tap: g =>
			hasStatus(g, Alice, 2, CardStatus.None)
		.pipe(takeTurn("Cathy clues 5 to Bob"))

		// After Cathy stalls, Alice's slot 2 should be finessed.
		hasStatus(game, Alice, 2, CardStatus.Finessed)

	test("can give a finesse while finessed"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r2", "g2", "g3", "p4"),
			Vector("p1", "b2", "p3", "y4"),
			Vector("p2", "y2", "b3", "r4")
		),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues blue to Cathy"))
		.pipe(takeTurn("Alice clues purple to Bob"))

		// Alice's b1 should still be finessed.
		hasStatus(game, Alice, 1, CardStatus.Finessed)

	test("understands a critical save while finessed, when other potential givers are finessed"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("p1", "p2", "p3", "g3"),
			Vector("p4", "y5", "b3", "p5"),
			Vector("b4", "y2", "p4", "r4")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues purple to Donald"))		// finessing p1, p2, p3
		.pipe(takeTurn("Donald clues blue to Cathy"))		// finesses b1, b2 in our hand
		.pipe(takeTurn("Alice clues 5 to Cathy"))

		// Alice's b1 should still be finessed.
		hasStatus(game, Alice, 1, CardStatus.Finessed)

	// test("understands a critical save where other players only have a play if we play"):
	// 	val game = setup(HGroup.atLevel(5), Vector(
	// 		Vector("xx", "xx", "xx", "xx"),
	// 		Vector("b1", "p2", "g4", "g3"),
	// 		Vector("y4", "y5", "p3", "b5"),
	// 		Vector("b4", "y2", "p4", "r4")
	// 	),
	// 		starting = Donald,
	// 		clueTokens = 7
	// 	)
	// 	.pipe(takeTurn("Donald clues purple to Cathy"))		// finessing p1 (Alice), b1 -> p2 (Bob)
	// 	.pipe(takeTurn("Alice clues 5 to Cathy"))	// If we play, Bob will think p2 will play ito

	// 	// Alice's b1 should still be finessed.
	// 	hasStatus(game, Alice, 1, CardStatus.Finessed)

	test("understands a finesse on top of an in-progress connection on other"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("p4", "r3", "g3", "b3"),
			Vector("p1", "r3", "p1", "y2"),
			Vector("b2", "y3", "r4", "b5")
		),
			playStacks = Some(Vector(1, 1, 1, 1, 0))
		)
		.pipe(takeTurn("Alice clues 3 to Bob"))			// finessing b2 (Donald)
		.pipe(takeTurn("Bob clues 5 to Donald"))
		.pipe(takeTurn("Cathy clues 5 to Donald"))		// should finesse b4 (Alice)

		hasStatus(game, Alice, 1, CardStatus.Finessed)
		hasInfs(game, None, Alice, 1, Vector("b4"))

	test("understands a finesse on top of an in-progress connection on us"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("p1", "r3", "p1", "y2"),
			Vector("b2", "y3", "r4", "b5"),
			Vector("y2", "b4", "b3", "g4")
		),
			starting = Donald,
			playStacks = Some(Vector(1, 1, 1, 1, 0))
		)
		.pipe(takeTurn("Donald clues 3 to Alice (slots 2,3,4)"))	// finessing b2 (Donald)
		.pipe(takeTurn("Alice clues 5 to Cathy"))
		.pipe(takeTurn("Bob clues 5 to Cathy"))					// should finesse b4 (Donald)

		hasStatus(game, Donald, 1, CardStatus.Finessed)
		hasInfs(game, None, Donald, 2, Vector("b4"))

	test("understands a layered player won't play if their promised card is unplayable"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("p2", "p3", "g4", "g3"),
			Vector("y1", "p4", "b5", "r3"),
			Vector("b4", "y5", "p5", "r4"),
			Vector("y3", "r1", "r3", "g4")
		),
			starting = Emily,
			init = _.copy(inEarlyGame = false)
		)
		.pipe(takeTurn("Emily clues purple to Donald"))		// finesses p1 (Alice), p2 (Bob), p3 (Bob), y1 (Cathy), p4 (Cathy)
		.pipe(takeTurn("Alice clues 5 to Donald"))

		// Alice doesn't need to save Donald's 5, since Cathy won't be able to play on her turn.

		assert(!game.importantAction(Alice.ordinal))

class Rainbow extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("prompts correctly in rainbow"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r2", "m3", "r1", "r3"),
			Vector("g4", "g2", "b3", "m5"),
			Vector("b1", "y2", "r3", "r4")
		),
			playStacks = Some(Vector(0, 0, 0, 2, 0)),
			variant = "Rainbow (5 Suits)",
			init = preClue(Bob, 2, Seq("blue"))
		)
		.pipe(takeTurn("Alice clues 2 to Bob"))

		// m3 will bomb as prompt into m2.
		assertEquals(game.lastMove, Some(ClueInterp.Mistake))
