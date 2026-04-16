package tests.hgroup.pink

import cats.effect.unsafe.implicits.global

import scala_bot.basics._
import scala_bot.test.{fullyKnown, hasInfs, hasStatus, Player, preClue, setup, takeTurn, TestVariant}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger, LogLevel}

class PinkPromise extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("interprets pink promise on a play clue"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "r4")
		),
			starting = Bob,
			variant = TestVariant.Pink5,
			playStacks = Some(Vector(2, 0, 0, 0, 0)),
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues 3 to Alice (slot 5)"))

		// This is exactly [r3], not [r3, i1, i2, i5].
		hasInfs(game, None, Alice, 5, Vector("r3"))

	test("gives play clues with pink promise"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("i1", "g4", "b4", "b4", "r4")
		),
			variant = TestVariant.Pink5,
			clueTokens = 7
		)
		.pipe(takeTurn("Alice clues 2 to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("pink promises rightmost 5 in a 5 stall"):
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "r4")
		),
			starting = Bob,
			variant = TestVariant.Pink5,
			clueTokens = 7,
			init = (game) =>
				game.withMeta(game.state.hands(Alice.ordinal)(4)):
					_.copy(status = CardStatus.ChopMoved)
		)
		.pipe(takeTurn("Bob clues 5 to Alice (slots 1,2,3,5)"))

		// Slot 3 is promised to be a 5.
		hasInfs(game, None, Alice, 3, Vector("r5", "y5", "g5", "b5", "i5"))

	test("doesn't 5 stall on a pink card"):
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "i2", "b4", "y4", "b3")
		),
			clueTokens = 7,
			variant = TestVariant.Pink5
		)
		.pipe(takeTurn("Alice clues 5 to Bob"))

		assert(game.lastMove == Some(ClueInterp.Mistake))

	test("understands a pink trash cm"):
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "r4")
		),
			starting = Bob,
			variant = TestVariant.Pink5,
			playStacks = Some(Vector(1, 1, 1, 1, 1)),
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slot 3)"))

		Seq(4, 5).foreach(slot => hasStatus(game, Alice, slot, CardStatus.ChopMoved))
		assertEquals(game.common.thinksTrash(game, Alice.ordinal),
			Vector(game.state.hands(Alice.ordinal)(2)))

	test("doesn't give a 5cm with wrong rank"):
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("i5", "b1", "r2", "i1", "g2")
		),
			variant = TestVariant.Pink5,
			playStacks = Some(Vector(0, 0, 0, 0, 2)),
			clueTokens = 4,
			init = _.copy(inEarlyGame = false)
		)
		.pipe(takeTurn("Alice clues 5 to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

class Pink1sAssumption extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("plays with pink 1s assumption"):
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "r4")
		),
			starting = Bob,
			variant = TestVariant.Pink5,
			clueTokens = 7,
			init = _.copy(inEarlyGame = false)
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slots 4,5)"))
		.pipe(takeTurn("Alice plays r1 (slot 5)"))
		.pipe(takeTurn("Bob discards r4", "g5"))

		assertEquals(game.common.thinksPlayables(game, Alice.ordinal),
			Vector(game.state.hands(Alice.ordinal)(4)))

	test("fixes a pink 1s assumption without fix promise"):
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r2", "i4", "i3", "y1", "g5"),
			Vector("g3", "b4", "r5", "y3", "i1")
		),
			starting = Cathy,
			variant = TestVariant.Pink5,
			clueTokens = 7
		)
		.pipe(takeTurn("Cathy clues 1 to Bob"))
		.pipe(takeTurn("Alice clues pink to Cathy"))
		.pipe(takeTurn("Bob plays y1", "b4"))
		.pipe(takeTurn("Cathy clues 5 to Alice (slot 5)"))
		.tap: g =>
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Rank(Bob.ordinal, 5))
		.pipe(takeTurn("Alice clues 5 to Bob"))

		hasInfs(game, None, Bob, 4, Vector("i2", "i3", "i4", "i5"))

	test("fixes a pink 1s assumption with fix promise"):
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r2", "i4", "i3", "y1", "g4"),
			Vector("g3", "b4", "r5", "y3", "i1")
		),
			starting = Cathy,
			variant = TestVariant.Pink5,
			clueTokens = 7
		)
		.pipe(takeTurn("Cathy clues 1 to Bob"))
		.pipe(takeTurn("Alice clues pink to Cathy"))
		.pipe(takeTurn("Bob plays y1", "b4"))
		.pipe(takeTurn("Cathy clues 5 to Alice (slot 5)"))

		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Rank(Bob.ordinal, 3))

		val fixHypo = takeTurn("Alice clues 3 to Bob")(game)
		assertEquals(fixHypo.lastMove, Some(ClueInterp.Fix))
		hasInfs(fixHypo, None, Bob, 4, Vector("i3"))

		val badHypo = takeTurn("Alice clues 4 to Bob")(game)
		assertEquals(badHypo.lastMove, Some(ClueInterp.Mistake))

	test("doesn't perform OCMs in pink"):
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "r4")
		),
			starting = Bob,
			variant = TestVariant.Pink5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slots 4,5)"))

		// Alice must play slot 5 (not allowed to OCM by playing slot 4).
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Play(game.state.hands(Alice.ordinal)(4)))

	test("plays 1s in order even after a certain discard"):
		val game = setup(HGroup.atLevel(10), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "g2", "r3", "i4"),
			Vector("g1", "g3", "i1", "i1"),
			Vector("r4", "r4", "b3", "r5")
		),
			starting = Cathy,
			variant = TestVariant.Pink5,
			clueTokens = 7
		)
		.pipe(takeTurn("Cathy clues 1 to Alice (slots 1,2,3,4)"))
		.pipe(takeTurn("Donald clues red to Bob"))
		.pipe(takeTurn("Alice clues green to Bob"))
		.pipe(takeTurn("Bob plays r1", "b1"))

		.pipe(takeTurn("Cathy discards g1", "y2"))

		// Only slot 4 is playable.
		assertEquals(game.common.thinksPlayables(game, Alice.ordinal), Vector(game.state.hands(Alice.ordinal)(3)))

	test("doesn't play filled-in pink cards as 1s"):
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "r4")
		),
			starting = Bob,
			variant = TestVariant.Pink5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slots 4,5)"))
		.pipe(takeTurn("Alice plays r1 (slot 5)"))
		.pipe(takeTurn("Bob clues pink to Alice (slots 1,5)"))

		// This is a pink fix.
		assertEquals(game.common.thinksPlayables(game, Alice.ordinal), Seq.empty)

	test("doesn't play filled-in omni cards as 1s"):
		val game = setup(HGroup.atLevel(4), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "b4", "r4")
		),
			starting = Bob,
			variant = TestVariant.Omni5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slots 3,4,5)"))
		.pipe(takeTurn("Alice plays r1 (slot 5)"))
		.pipe(takeTurn("Bob clues red to Alice (slots 1,4)"))
		.pipe(takeTurn("Alice plays y1 (slot 5)"))
		.pipe(takeTurn("Bob clues green to Alice (slots 1,5)"))

		// This is an omni fix (only r1 is playable).
		assertEquals(game.common.thinksPlayables(game, Alice.ordinal), Vector(game.state.hands(Alice.ordinal)(1)))

class PinkPrompts extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("prompts leftmost when rank matches"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "i3", "i1", "r4", "g5"),
			Vector("g4", "y3", "r2", "b3", "b1")
		),
			starting = Bob,
			variant = TestVariant.Pink5,
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues 2 to Alice (slots 3,4,5)"))
		.pipe(takeTurn("Cathy clues red to Bob"))

		// Alice should prompt slot 3 as r2.
		hasInfs(game, None, Alice, 3, Vector("r2"))

	test("finesses when rank mismatches"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y3", "r1", "r4", "g5"),
			Vector("g4", "i3", "r2", "b3", "b1")
		),
			starting = Cathy,
			variant = TestVariant.Pink5,
			playStacks = Some(Vector(0, 0, 0, 0, 1)),
			clueTokens = 7
		)
		.pipe(takeTurn("Cathy clues 5 to Alice (slots 3,4,5)"))
		.pipe(takeTurn("Alice discards g4 (slot 2)"))
		.pipe(takeTurn("Bob clues pink to Cathy"))

		// Alice should finesse slot 1 as i2.
		hasInfs(game, None, Alice, 1, Vector("i2"))
		hasStatus(game, Alice, 1, CardStatus.Finessed)

	test("prompts with two rank mismatches"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "i2", "r4", "r4", "g5"),
			Vector("g4", "y3", "r2", "b3", "b1")
		),
			starting = Bob,
			variant = TestVariant.Pink5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues 2 to Alice (slots 3,4,5)"))
		.pipe(takeTurn("Cathy clues 5 to Alice (slots 2,3)"))	// Alice: [xx ?5 i? ?2 ?2]
		.pipe(takeTurn("Alice discards b3 (slot 1)"))

		.pipe(takeTurn("Bob clues blue to Cathy"))
		.pipe(takeTurn("Cathy clues pink to Bob"))

		// Alice should prompt slot 3 as i1.
		hasInfs(game, None, Alice, 3, Vector("i1"))

	test("prompts even with rank mismatch if no finesse position"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("i3", "b3", "r1", "r4", "g5"),
			Vector("g4", "y3", "r2", "b3", "b1")
		),
			starting = Bob,
			variant = TestVariant.Pink5,
			playStacks = Some(Vector(0, 0, 0, 0, 1)),
			discarded = Vector("y4"),
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues yellow to Alice (slots 3,4,5)"))
		.pipe(takeTurn("Cathy clues 5 to Alice (slots 1,2)"))
		.pipe(takeTurn("Alice clues red to Bob"))

		.pipe(takeTurn("Bob plays r1", "r1"))
		.pipe(takeTurn("Cathy clues pink to Bob"))

		// Alice should prompt slot 1 as i2.
		hasInfs(game, None, Alice, 1, Vector("i2"))

	test("understands a mismatched rank prompt"):
		val game = setup(HGroup.atLevel(2), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b3", "i3", "r1", "r4", "g5"),
			Vector("i2", "b5", "y2", "y3", "y4")
		),
			variant = TestVariant.Pink5,
			playStacks = Some(Vector(0, 0, 0, 0, 1)),
			discarded = Vector("y4"),
			clueTokens = 7
		)
		.pipe(takeTurn("Alice clues yellow to Cathy"))
		.pipe(takeTurn("Bob clues 5 to Cathy"))
		.pipe(takeTurn("Cathy clues red to Alice (slot 3)"))

		.pipe(takeTurn("Alice plays r1 (slot 3)"))
		.pipe(takeTurn("Bob clues 3 to Alice (slots 2,3)"))

		// Alice should understand Cathy's i2 is prompted, and not self-finesse.
		hasStatus(game, Alice, 1, CardStatus.None)
		hasInfs(game, None, Cathy, 1, Vector("i2"))

	// test("understands a known bluff when rank mismatches"):
	// 	val game = setup(HGroup.atLevel(2), Vector(
	// 		Vector("xx", "xx", "xx", "xx"),
	// 		Vector("y1", "r4", "r4", "y4"),
	// 		Vector("y4", "g4", "g4", "b4"),
	// 		Vector("y2", "b1", "i5", "b4")
	// 	),
	// 		variant = TestVariant.Pink5,
	// 		clueTokens = 7
	// 	)
	// 	.pipe(takeTurn("Alice clues 2 to Donald"))
	// 	.pipe(takeTurn("Bob plays y1", "i4"))
	// 	.pipe(takeTurn("Cathy clues pink to Alice (slot 4)"))
	// 	.tap: g =>
	// 		// This could be a known bluff on Donald's b1.
	// 		hasStatus(g, Donald, 2, CardStatus.Finessed)
	// 	.pipe(takeTurn("Donald plays b1", "i4"))

	// 	hasInfs(game, None, Alice, 4, Vector("i2"))

class PinkChoiceTempo extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("gives a pink choice tempo clue"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "y3", "r2", "b3", "b1"),
			Vector("r1", "i1", "i3", "i2", "g5")
		),
			starting = Bob,
			variant = TestVariant.Pink5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues pink to Cathy"))
		.pipe(takeTurn("Cathy plays i1", "b2"))
		.pipe(takeTurn("Alice clues 4 to Cathy"))

		hasInfs(game, None, Cathy, 4, Vector("i2"))

	test("receives a pink choice tempo clue"):
		val game = setup(HGroup.atLevel(6), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "y3", "r2", "b3", "b1"),
			Vector("r3", "b1", "b3", "b2", "g5")
		),
			starting = Cathy,
			variant = TestVariant.Pink5,
			clueTokens = 7
		)
		.pipe(takeTurn("Cathy clues 5 to Alice (slots 3,4,5)"))
		.pipe(takeTurn("Alice clues blue to Bob"))
		.pipe(takeTurn("Bob clues 4 to Alice (slots 3,4,5)"))

		hasInfs(game, None, Alice, 4, Vector("i1"))
		hasStatus(game, Alice, 2, CardStatus.None)	// Not TCCM

	test("interprets a pink choice tempo clue over a pink fix"):
		val game = setup(HGroup.atLevel(3), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y3", "r1", "r4", "g5"),
			Vector("g4", "i3", "r2", "b3", "b1")
		),
			starting = Cathy,
			variant = TestVariant.Pink5,
			playStacks = Some(Vector(0, 0, 0, 0, 1)),
			clueTokens = 7
		)
		.pipe(takeTurn("Cathy clues pink to Alice (slots 1,5)"))
		.pipe(takeTurn("Alice plays i2 (slot 5)"))
		.pipe(takeTurn("Bob clues 2 to Alice (slot 2)"))

		// Slot 2 should be i3, not trash.
		hasInfs(game, None, Alice, 2, Vector("i3"))

	test("doesn't self-prompt a pink positional"):
		val game = setup(HGroup.atLevel(3), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y3", "r1", "i4", "i3"),
			Vector("i2", "g4", "r2", "b3", "b1")
		),
			starting = Cathy,
			variant = TestVariant.Pink5,
			playStacks = Some(Vector(0, 0, 0, 0, 1)),
			discarded = Vector("i3", "i4"),
			clueTokens = 7
		)
		.pipe(takeTurn("Cathy clues pink to Bob"))		// i3, i4 save
		.pipe(takeTurn("Alice clues 4 to Bob"))

		// Not a valid self-prompt, even if Cathy finesses.
		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("doesn't self-prompt with a rewinded pink positional"):
		val game = setup(HGroup.atLevel(3), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("o3", "r3", "y4", "g4", "b4"),
			Vector("o4", "r4", "y4", "g4", "b4")
		),
			starting = Cathy,
			variant = TestVariant.Omni5,
			playStacks = Some(Vector(0, 0, 0, 0, 1)),
			clueTokens = 7,
			init =
				// Alice's slots 4 and 5 are known omni.
				preClue[HGroup](Alice, 4, Seq("1", "5")) andThen
				preClue[HGroup](Alice, 5, Seq("1", "5"))
		)
		.pipe(takeTurn("Cathy clues 4 to Alice (slots 4,5)"))	// positional o2
		.pipe(takeTurn("Alice clues yellow to Cathy"))			// selfish finesse
		.pipe(takeTurn("Bob discards b4", "r1"))

		.pipe(takeTurn("Cathy clues red to Alice (slots 1,4,5)"))	// reverse f
		.pipe(takeTurn("Alice plays o2 (slot 4)"))
		.pipe(takeTurn("Bob plays o3", "y1"))

		.pipe(takeTurn("Cathy plays o4", "g1"))
		.tap: g =>
			// Bob hasn't demonstrated a red finesse yet.
			hasInfs(g, None, Alice, 2, Vector("r1", "r2"))

		.pipe(takeTurn("Alice bombs o2 (slot 5)"))	// good touch bomb
		.pipe(takeTurn("Bob plays r1", "b1"))

		// Bob demonstrated the finesse.
		hasInfs(game, None, Alice, 3, Vector("r2"))

	test("plays into complex pink tempo clues"):
		val game = setup(HGroup.atLevel(3), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b4", "y4", "r4", "g1"),
			Vector("i1", "g4", "g4", "r4", "y4")
		),
			starting = Bob,
			variant = TestVariant.PinkLPink6,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slots 2,3,4)"))
		.pipe(takeTurn("Cathy clues green to Bob"))
		.pipe(takeTurn("Alice plays l1 (slot 4)"))

		.pipe(takeTurn("Bob plays g1", "y3"))
		.pipe(takeTurn("Cathy clues 2 to Alice (slots 1,3,4)"))
		.tap: g =>
			// Pink fix promise
			hasInfs(g, None, Alice, 4, Vector("i2"))

		.pipe(takeTurn("Alice clues pink to Cathy"))
		.pipe(takeTurn("Bob clues 3 to Alice (slots 1,3,4)"))	// Pink choice tempo on slot 3
		.pipe(takeTurn("Cathy plays i1", "r3"))
		.tap: g =>
			// Slot 4 as [i2,l2] must be played first.
			assertEquals(g.common.thinksPlayables(g, Alice.ordinal), Vector(g.state.hands(Alice.ordinal)(3)))

		.pipe(takeTurn("Alice plays i2 (slot 4)"))
		.pipe(takeTurn("Bob discards r4", "g3"))
		.pipe(takeTurn("Cathy clues 2 to Alice (slots 2,4)"))	// Pink choice tempo on slot 2

		// Slot 4 as [i3, l2] must be played first.
		assertEquals(game.common.thinksPlayables(game, Alice.ordinal), Vector(game.state.hands(Alice.ordinal)(3)))

class PinkFixes extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("gives a pink trash fix"):
		val game = setup(HGroup.atLevel(3), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "i3", "i1", "r5", "g4"),
			Vector("g4", "y3", "r2", "i1", "i2")
		),
			starting = Bob,
			variant = TestVariant.Pink5,
			playStacks = Some(Vector(0, 0, 0, 0, 1)),
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues pink to Cathy"))
		.pipe(takeTurn("Cathy plays i2", "b3"))
		.tap: g =>
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Rank(Cathy.ordinal, 1))
		.pipe(takeTurn("Alice clues 1 to Cathy"))

		assertEquals(game.common.thinksTrash(game, Cathy.ordinal),
			Vector(game.state.hands(Cathy.ordinal)(4)))

	test("interprets a pink trash fix"):
		val game = setup(HGroup.atLevel(3), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y3", "r1", "r5", "g4"),
			Vector("g4", "i3", "r2", "b3", "b1")
		),
			starting = Cathy,
			variant = TestVariant.Pink5,
			playStacks = Some(Vector(0, 0, 0, 0, 1)),
			clueTokens = 7
		)
		.pipe(takeTurn("Cathy clues pink to Alice (slots 4,5)"))
		.pipe(takeTurn("Alice plays i2 (slot 5)"))
		.pipe(takeTurn("Bob clues 1 to Alice (slot 5)"))

		assertEquals(game.common.thinksTrash(game, Alice.ordinal),
			Vector(game.state.hands(Alice.ordinal)(4)))

	test("doesn't interpret a pink trash fix after a prompt"):
		val game = setup(HGroup.atLevel(3), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "y3", "r4", "y4", "i2"),
			Vector("g4", "i4", "g3", "b3", "b4")
		),
			starting = Bob,
			variant = TestVariant.Pink5,
			clueTokens = 7,
			init =
				fullyKnown[HGroup](Alice, 1, "b1") andThen
				preClue[HGroup](Bob, 5, Seq("pink"))
		)
		.pipe(takeTurn("Bob clues 5 to Alice (slots 4,5)"))
		.pipe(takeTurn("Cathy clues 2 to Bob"))			// prompt Alice's slot 4
		.tap: g =>
			hasInfs(g, None, Alice, 4, Vector("i1"))

		.pipe(takeTurn("Alice plays b1 (slot 1)"))		// Alice draws a new 5
		.pipe(takeTurn("Bob clues 5 to Alice (slots 1,4,5)"))	// 5 Stall (not fix)

		hasInfs(game, None, Alice, 4, Vector("i1"))
