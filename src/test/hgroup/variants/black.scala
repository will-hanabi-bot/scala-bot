package tests.hgroup

import scala_bot.basics._
import scala_bot.test.{hasInfs, hasStatus, Player, preClue, setup, takeTurn, TestVariant}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger, LogLevel}

class Black extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	private val RainbowBlack6 = Variant(
		1000,
		"Black & Rainbow (6 Suits)",
		Vector("Red", "Yellow", "Green", "Blue", "Black", "Rainbow"),
		shorts = Some(Vector('r', 'y', 'g', 'b', 'k', 'm'))
	)

	test("understands k3/4 save with black (considering possible bad-touch k2)"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g4")
		),
			starting = Bob,
			variant = TestVariant.Black5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues black to Alice (slot 5)"))

		// Alice's unclued cards could contain duplicate 2s, so k2 must be considered until disproven.
		hasInfs(game, None, Alice, 5, Vector("k1", "k2", "k3", "k4"))

	test("doesn't save k2 with black"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "k2")
		),
			variant = TestVariant.Black5,
			clueTokens = 7
		)
		.pipe(takeTurn("Alice clues black to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("doesn't save k5 with black"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "k5")
		),
			variant = TestVariant.Black5,
			clueTokens = 7
		)
		.pipe(takeTurn("Alice clues black to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("relaxes save restrictions when multiple cards are newly clued"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g4")
		),
			starting = Bob,
			variant = TestVariant.Black5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues black to Alice (slots 4,5)"))

		hasInfs(game, None, Alice, 5, Vector("k1", "k2", "k3", "k4", "k5"))

	test("doesn't relax save restrictions when only 1 card is newly clued"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g4")
		),
			starting = Bob,
			variant = TestVariant.Black5,
			clueTokens = 4,
			init = preClue(Alice, 5, Seq("black"))
		)
		.pipe(takeTurn("Bob clues black to Alice (slots 4,5)"))
		.tap: g =>
			// k5 is excluded, while k2 remains possible until self-dupe 2s are disproven.
			hasInfs(g, None, Alice, 4, Vector("k1", "k2", "k3", "k4"))
		.pipe(takeTurn("Alice discards b4 (slot 3)"))
		.pipe(takeTurn("Bob discards g4", "g3"))
		.pipe(takeTurn("Alice discards g4 (slot 3)"))

		// Only 1 original unclued card remains, so self-dupe 2s are impossible.
		hasInfs(game, None, Alice, 4, Vector("k1", "k3", "k4"))

	test("relaxes save restrictions when filling in"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g4")
		),
			starting = Bob,
			variant = TestVariant.Black5,
			clueTokens = 4,
			init = preClue(Alice, 5, Seq("5"))
		)
		.pipe(takeTurn("Bob clues black to Alice (slots 4,5)"))
		.tap: g =>
			hasInfs(g, None, Alice, 4, Vector("k1", "k2", "k3", "k4"))
		.pipe(takeTurn("Alice discards b4 (slot 3)"))
		.pipe(takeTurn("Bob discards g4", "g3"))
		.pipe(takeTurn("Alice discards g4 (slot 3)"))

		// Fill-in exception is unconditional, so k2 stays even after discarding other cards.
		hasInfs(game, None, Alice, 4, Vector("k1", "k2", "k3", "k4"))

	test("relaxes save restrictions when avoiding bad touch"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "b4", "b4", "r2", "k2"),
			Vector("y3", "y3", "y4", "y4", "g4"),
		),
			starting = Cathy,
			variant = TestVariant.Black5,
			playStacks = Some(Vector(4, 0, 0, 0, 0)),
			clueTokens = 7
		)
		.pipe(takeTurn("Cathy clues black to Bob"))

		hasStatus(game, Alice, 1, CardStatus.None)
		assertEquals(game.lastMove, Some(ClueInterp.Save))

	test("doesn't relax save restrictions when no bad touch"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "b4", "b4", "r3", "k2"),
			Vector("y3", "y3", "y4", "y4", "g4"),
		),
			starting = Cathy,
			variant = TestVariant.Black5,
			playStacks = Some(Vector(4, 0, 0, 0, 0)),
			clueTokens = 7
		)
		.pipe(takeTurn("Cathy clues black to Bob"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Finessed)
			// Common knowledge still tracks that Bob thinks slot 5 could be a save until Alice plays k1.
			hasInfs(g, None, Bob, 5, Vector("k1", "k2", "k3", "k4"))
		.pipe(takeTurn("Alice plays k1 (slot 1)"))

		hasInfs(game, None, Bob, 5, Vector("k2"))

	test("understands receiver side of black k2 finesse when partner blind plays k1"):
		val base = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y3", "y3", "y4", "y4", "g4"),
			Vector("k1", "g4", "b4", "b4", "r3")
		),
			starting = Bob,
			variant = TestVariant.Black5,
			playStacks = Some(Vector(4, 0, 0, 0, 0)),
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues black to Alice (slot 5)"))

		hasStatus(base, Cathy, 1, CardStatus.None)
		hasInfs(base, None, Alice, 5, Vector("k1", "k2", "k3", "k4"))

		// If Cathy blind plays k1, Alice realizes it was a finesse for k2.
		val played = takeTurn("Cathy plays k1", "b3")(base)
		hasInfs(played, None, Alice, 5, Vector("k2"))

		// If Cathy does not play k1 (e.g. Alice had r2 trash), k2 stays in Alice's save inferences.
		val retained = takeTurn("Cathy discards r3", "b3")(base)
		hasInfs(retained, None, Alice, 5, Vector("k1", "k2", "k3", "k4"))

	test("narrows k2 save after discarding cards disproves self-dupe 2s"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g4")
		),
			starting = Bob,
			variant = TestVariant.Black5,
			clueTokens = 3
		)
		.pipe(takeTurn("Bob clues black to Alice (slot 5)"))
		.tap(hasInfs(_, None, Alice, 5, Vector("k1", "k2", "k3", "k4")))
		.pipe(takeTurn("Alice discards b4 (slot 4)"))
		.pipe(takeTurn("Bob discards g4", "g3"))
		.pipe(takeTurn("Alice discards g4 (slot 4)"))
		.tap: g =>
			// 2 original unclued cards remain (slots 3 and 4), so self-dupe 2s are still possible.
			hasInfs(g, None, Alice, 5, Vector("k1", "k2", "k3", "k4"))
		.pipe(takeTurn("Bob discards g3", "b3"))
		.pipe(takeTurn("Alice discards y3 (slot 4)"))

		// Only 1 original unclued card remains, so self-dupe 2s are impossible.
		hasInfs(game, None, Alice, 5, Vector("k1", "k3", "k4"))

	test("narrows k2 save after discarding cards disproves basic trash 2"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g4")
		),
			starting = Bob,
			variant = TestVariant.Black5,
			playStacks = Some(Vector(2, 0, 0, 0, 0)),
			clueTokens = 1
		)
		.pipe(takeTurn("Bob clues black to Alice (slot 5)"))
		.pipe(takeTurn("Alice discards b4 (slot 4)"))
		.pipe(takeTurn("Bob discards g4", "g3"))
		.pipe(takeTurn("Alice discards g4 (slot 4)"))
		.pipe(takeTurn("Bob discards g3", "b3"))
		.pipe(takeTurn("Alice discards y3 (slot 4)"))
		.tap: g =>
			// 1 original unclued card still remains and could be basic trash r2.
			hasInfs(g, None, Alice, 5, Vector("k1", "k2", "k3", "k4"))
		.pipe(takeTurn("Bob discards b3", "y3"))
		.pipe(takeTurn("Alice discards r3 (slot 4)"))

		// All 4 original unclued cards were discarded and none were r2.
		hasInfs(game, None, Alice, 5, Vector("k1", "k3", "k4"))

	test("keeps k2 inference when discarding reveals basic trash 2"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g4")
		),
			starting = Bob,
			variant = TestVariant.Black5,
			playStacks = Some(Vector(2, 0, 0, 0, 0)),
			clueTokens = 1
		)
		.pipe(takeTurn("Bob clues black to Alice (slot 5)"))
		.pipe(takeTurn("Alice discards r2 (slot 4)"))
		.pipe(takeTurn("Bob discards g4", "g3"))
		.pipe(takeTurn("Alice discards g4 (slot 4)"))
		.pipe(takeTurn("Bob discards g3", "b3"))
		.pipe(takeTurn("Alice discards y3 (slot 4)"))
		.pipe(takeTurn("Bob discards b3", "y3"))
		.pipe(takeTurn("Alice discards b4 (slot 4)"))

		hasInfs(game, None, Alice, 5, Vector("k1", "k2", "k3", "k4"))

	test("keeps k2 inference when discarding reveals duplicate 2s in hand"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g4")
		),
			starting = Bob,
			variant = TestVariant.Black5,
			clueTokens = 1
		)
		.pipe(takeTurn("Bob clues black to Alice (slot 5)"))
		.pipe(takeTurn("Alice discards r2 (slot 4)"))
		.pipe(takeTurn("Bob discards g4", "g3"))
		.pipe(takeTurn("Alice discards r2 (slot 4)"))
		.pipe(takeTurn("Bob discards g3", "b3"))
		.pipe(takeTurn("Alice discards y3 (slot 4)"))
		.pipe(takeTurn("Bob discards b3", "y3"))
		.pipe(takeTurn("Alice discards b4 (slot 4)"))

		hasInfs(game, None, Alice, 5, Vector("k1", "k2", "k3", "k4"))

	test("narrows k2 save after discarding single useful 2 and other non-2 cards"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g4")
		),
			starting = Bob,
			variant = TestVariant.Black5,
			clueTokens = 1
		)
		.pipe(takeTurn("Bob clues black to Alice (slot 5)"))
		.pipe(takeTurn("Alice discards r2 (slot 4)"))
		.pipe(takeTurn("Bob discards g4", "g3"))
		.pipe(takeTurn("Alice discards g4 (slot 4)"))
		.pipe(takeTurn("Bob discards g3", "b3"))
		.pipe(takeTurn("Alice discards y3 (slot 4)"))
		.tap: g =>
			// The last original unclued card could still be the second r2.
			hasInfs(g, None, Alice, 5, Vector("k1", "k2", "k3", "k4"))
		.pipe(takeTurn("Bob discards b3", "y3"))
		.pipe(takeTurn("Alice discards b4 (slot 4)"))

		// Having only a single useful r2 at clue time was not bad touch.
		hasInfs(game, None, Alice, 5, Vector("k1", "k3", "k4"))

	test("narrows k2 save when other cards are clued as useful (disproving self-dupe)"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g4")
		),
			starting = Bob,
			variant = TestVariant.Black5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues black to Alice (slot 5)"))
		.pipe(takeTurn("Alice discards b4 (slot 4)"))
		.tap(hasInfs(_, None, Alice, 5, Vector("k1", "k2", "k3", "k4")))
		.pipe(takeTurn("Bob clues 1 to Alice (slots 2,3)"))

		// Slots 2 and 3 are clued as 1s (leaving only 1 original unclued card, disproving self-dupe 2s),
		// and slot 5 also receives negative 1 (eliminating k1).
		hasInfs(game, None, Alice, 5, Vector("k3", "k4"))

	test("narrows k2 save when last remaining card is clued as useful (disproving trash 2)"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g4")
		),
			starting = Bob,
			variant = TestVariant.Black5,
			playStacks = Some(Vector(2, 0, 0, 0, 0)),
			clueTokens = 2
		)
		.pipe(takeTurn("Bob clues black to Alice (slot 5)"))
		.pipe(takeTurn("Alice discards b4 (slot 4)"))
		.pipe(takeTurn("Bob discards g4", "g3"))
		.pipe(takeTurn("Alice discards g4 (slot 4)"))
		.pipe(takeTurn("Bob discards g3", "b3"))
		.pipe(takeTurn("Alice discards y3 (slot 4)"))
		.tap: g =>
			// The last original unclued card (slot 4) could still be basic trash r2.
			hasInfs(g, None, Alice, 5, Vector("k1", "k2", "k3", "k4"))
		.pipe(takeTurn("Bob clues red to Alice (slot 4)"))

		// Slot 4 is clued as useful r3 (not trash r2), so k2 is narrowed out.
		hasInfs(game, None, Alice, 5, Vector("k1", "k3", "k4"))

	test("narrows k2 save when remaining cards are clued with different colours"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g4")
		),
			starting = Bob,
			variant = TestVariant.Black5,
			clueTokens = 4
		)
		.pipe(takeTurn("Bob clues black to Alice (slot 5)"))
		.pipe(takeTurn("Alice discards b4 (slot 4)"))
		.pipe(takeTurn("Bob discards g4", "g3"))
		.pipe(takeTurn("Alice discards g4 (slot 4)"))
		.tap(hasInfs(_, None, Alice, 5, Vector("k1", "k2", "k3", "k4")))
		.pipe(takeTurn("Bob clues red to Alice (slot 3)"))

		// Slot 3 is focused as r1 and only slot 4 can still be a 2.
		hasInfs(game, None, Alice, 5, Vector("k1", "k3", "k4"))

	test("personally narrows k2 save when all 2s are visible in partner's hand"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r2", "y2", "g2", "b2", "g4")
		),
			starting = Bob,
			variant = TestVariant.Black5,
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues black to Alice (slot 5)"))

		// Bob doesn't know Bob holds all four 2s, so common knowledge keeps k2.
		hasInfs(game, None, Alice, 5, Vector("k1", "k2", "k3", "k4"))
		// Alice sees one copy of every 2 in Bob's hand, so Alice knows she can't hold duplicate 2s.
		hasInfs(game, Some(Alice), Alice, 5, Vector("k1", "k3", "k4"))

	test("relaxes save restrictions when avoiding duplicate 2s in receiver's hand"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r2", "r2", "y4", "y4", "k2")
		),
			variant = TestVariant.Black5,
			clueTokens = 7
		)
		.pipe(takeTurn("Alice clues black to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Save))
		hasInfs(game, None, Bob, 5, Vector("k1", "k2", "k3", "k4"))

	test("relaxes save restrictions when avoiding duplicating a clued 2"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r2", "b4", "y4", "y4", "k2"),
			Vector("g4", "g4", "b4", "y3", "r2")
		),
			variant = TestVariant.Black5,
			clueTokens = 7,
			init = preClue(Cathy, 5, Seq("2"))
		)
		.pipe(takeTurn("Alice clues black to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Save))
		hasInfs(game, None, Bob, 5, Vector("k1", "k2", "k3", "k4"))

	test("relaxes save restrictions for k5 when avoiding dead 5 bad touch and narrows when disproven"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y3", "y4", "y4", "g4", "g4")
		),
			starting = Bob,
			variant = TestVariant.Black5,
			discarded = Vector("r4", "r4"),
			clueTokens = 1
		)
		.pipe(takeTurn("Bob clues black to Alice (slot 5)"))
		.tap: g =>
			// r5 is dead (basic trash), so k5 is also a valid save.
			hasInfs(g, None, Alice, 5, Vector("k1", "k2", "k3", "k4", "k5"))
		.pipe(takeTurn("Alice discards b4 (slot 4)"))
		.pipe(takeTurn("Bob discards g4 (slot 5)", "g3"))
		.pipe(takeTurn("Alice discards b4 (slot 4)"))
		.pipe(takeTurn("Bob discards g4 (slot 5)", "b3"))
		.pipe(takeTurn("Alice discards y3 (slot 4)"))
		.tap: g =>
			// k2 is now disproven (only 1 unclued card left), but k5 remains because that card could be dead r5.
			hasInfs(g, None, Alice, 5, Vector("k1", "k3", "k4", "k5"))
		.pipe(takeTurn("Bob discards b3", "r3"))
		.pipe(takeTurn("Alice discards g3 (slot 4)"))

		// Dead r5 is now also disproven.
		hasInfs(game, None, Alice, 5, Vector("k1", "k3", "k4"))

	test("relaxes save restrictions when giving important negative info"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "y4", "y4", "g4")
		),
			starting = Bob,
			variant = TestVariant.Black5,
			playStacks = Some(Vector(1, 1, 1, 1, 0)),
			clueTokens = 7,
			init = preClue(Alice, 1, Seq("2"))
		)
		.pipe(takeTurn("Bob clues black to Alice (slot 5)"))

		// Negative black on slot 1 makes it playable (not k2), relaxing black save restrictions unconditionally.
		hasInfs(game, None, Alice, 5, Vector("k1", "k2", "k3", "k4", "k5"))

	test("relaxes save restrictions when touching a critical rainbow card"):
		val critGame = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("m3", "r4", "y4", "g4", "k2")
		),
			variant = RainbowBlack6,
			discarded = Vector("m3"),
			clueTokens = 7
		)
		.pipe(takeTurn("Alice clues black to Bob"))

		assertEquals(critGame.lastMove, Some(ClueInterp.Save))
		hasInfs(critGame, None, Bob, 5, Vector("k1", "k2", "k3", "k4", "k5", "m1", "m3"))

		val nonCritGame = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("m3", "r4", "y4", "g4", "k2")
		),
			variant = RainbowBlack6,
			clueTokens = 7
		)
		.pipe(takeTurn("Alice clues black to Bob"))

		assertEquals(nonCritGame.lastMove, Some(ClueInterp.Mistake))

	test("understands Florrat Save on k4 with 4 clue and narrows when disproven"):
		val validFlorrat = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "g3", "y3", "b3", "k4")
		),
			variant = TestVariant.Black5,
			discarded = Vector("r4"),
			clueTokens = 7
		)
		.pipe(takeTurn("Alice clues 4 to Bob"))

		assertEquals(validFlorrat.lastMove, Some(ClueInterp.Save))
		hasInfs(validFlorrat, None, Bob, 5, Vector("r4", "k4"))

		val invalidFlorrat = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "g3", "y3", "b3", "k4")
		),
			variant = TestVariant.Black5,
			discarded = Vector("r4"),
			clueTokens = 7
		)
		.pipe(takeTurn("Alice clues 4 to Bob"))

		assertEquals(invalidFlorrat.lastMove, Some(ClueInterp.Mistake))

		val narrowed = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "y3", "b3", "y3", "g3")
		),
			starting = Bob,
			variant = TestVariant.Black5,
			discarded = Vector("r4"),
			clueTokens = 7
		)
		.pipe(takeTurn("Bob clues 4 to Alice (slots 1,5)"))
		.tap(hasInfs(_, None, Alice, 5, Vector("r4", "k4")))
		.pipe(takeTurn("Alice discards b3 (slot 4)"))
		.pipe(takeTurn("Bob clues blue to Alice (slot 2)"))

		// Filling in the ancillary 4 as non-critical b4 disproves Florrat Save, narrowing slot 5 to r4.
		hasInfs(narrowed, None, Alice, 5, Vector("r4"))
