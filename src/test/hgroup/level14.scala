package tests.hgroup.level14

import cats.effect.unsafe.implicits.global

import scala_bot.basics._
import scala_bot.test.{fullyKnown, hasInfs, hasStatus, Player, preClue, setup, takeTurn, TestVariant}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger, LogLevel}

class TOCM extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("will find a tocm to the next player"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "r4", "g4", "r3", "r5")
		),
			playStacks = Some(Vector(1, 1, 1, 1, 1)),
			starting = Bob,
			clueTokens = 1,
			init = _.copy(inEarlyGame = false)
		)
		// With all 1's already played, the clue saves the last 2 cards
		// and gives Alice two trash cards.
		.pipe(takeTurn("Bob clues 1 to Alice (slots 2,3)"))

		// Alice can save the 5 by discarding out of order.
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Discard(game.state.hands(Alice.ordinal)(2)))

	test("will find a tocm to cathy"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "r3", "y3", "y4", "g4"),
			Vector("r4", "r4", "g4", "r3", "r5")
		),
			playStacks = Some(Vector(0, 5, 0, 0, 0)),
			starting = Bob,
			clueTokens = 1,
			init = _.copy(inEarlyGame = false)
		)
		// With all yellow's played, the clue saves slot 5,
		// and gives Alice three trash cards.
		.pipe(takeTurn("Bob clues yellow to Alice (slots 2,3,4)"))

		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Discard(game.state.hands(Alice.ordinal)(3)))

	test("doesn't do a bad sdcm"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "r3", "y3", "y4", "y1"),
			Vector("r4", "r4", "g4", "r3", "r5")
		),
			playStacks = Some(Vector(1, 1, 1, 1, 1)),
			clueTokens = 1,
			starting = Cathy,
			init = _.copy(inEarlyGame = false)
		)
		.pipe(takeTurn("Cathy clues 1 to Alice (slots 3,4)"))

		// Alice should discard slot 3 (normal trash discard) rather than slot 2 (scream) or slot 4 (tocm on Bob's trash).
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Discard(game.state.hands(Alice.ordinal)(2)))

	test("can still discard chop as a scream to the next player"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "r3", "y3", "y4", "r5"),
			Vector("r4", "r4", "g4", "r3", "y1")
		),
			playStacks = Some(Vector(1, 1, 1, 1, 1)),
			clueTokens = 1,
			starting = Cathy,
			init = _.copy(inEarlyGame = false)
		)
		.pipe(takeTurn("Cathy clues 1 to Alice (slot 4)"))

		// Alice has known trash, but should signal to Bob that he cannot discard his chop.
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Discard(game.state.hands(Alice.ordinal)(2)))

	test("will interpret a tocm to the next player"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b1", "g1", "r1", "r5"),
			Vector("y4", "r4", "g4", "r4", "b5")
		),
			playStacks = Some(Vector(1, 1, 1, 1, 1))
		)
		.pipe(takeTurn("Alice clues 1 to Bob"))
		.pipe(takeTurn("Bob discards g1", "r1"))

		// Cathy's slot 5 should be chop moved.
		hasStatus(game, Cathy, 5, CardStatus.ChopMoved)

	test("will interpret a tocm skipping a player"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b1", "g1", "r1", "r5"),
			Vector("y4", "r4", "g4", "r4", "b5")
		),
			starting = Alice,
			playStacks = Some(Vector(1, 1, 1, 1, 1))
		)
		.pipe(takeTurn("Alice clues 1 to Bob"))
		.pipe(takeTurn("Bob discards r1", "r1"))		// OCM on Alice

		// Alice's slot 5 should be chop moved.
		hasStatus(game, Alice, 5, CardStatus.ChopMoved)

	test("shouldn't confuse a scream discard for a tocm"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b5", "g2", "r3", "r5"),
			Vector("y1", "r4", "g4", "r4", "y3")
		),
			starting = Bob,
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			clueTokens = 1
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slot 4)"))
		.pipe(takeTurn("Cathy plays y1", "r2"))
		.tap: g =>
			// Alice's slot 5 should be chop moved from the trash finesse.
			hasStatus(g, Alice, 5, CardStatus.ChopMoved)
			// Alice's slot 4 should be known trash.
			assert(g.meta(g.state.hands(Alice.ordinal)(3)).trash)
			hasInfs(g, None, Alice, 4, Vector("r1"))
		// If Alice discards slot 3 with known trash on zero clues, must intend an SDCM.
		.pipe(takeTurn("Alice discards p4 (slot 3)"))

		// Bob is chop moved once from the scream discard.
		hasStatus(game, Bob, 4, CardStatus.None)
		hasStatus(game, Bob, 5, CardStatus.ChopMoved)

class SDOCM extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("will interpret a sdocm to the next player"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b1", "g1", "r1", "r5"),
			Vector("y4", "r4", "g4", "r4", "b5")
		),
			playStacks = Some(Vector(0, 1, 1, 1, 1))
		)
		.pipe(takeTurn("Alice clues 1 to Bob"))
		.pipe(takeTurn("Bob discards b1", "r1"))

		// Cathy's slot 5 should be chop moved.
		hasStatus(game, Cathy, 5, CardStatus.ChopMoved)

	test("will interpret a sdocm to the player after next"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b1", "g1", "r1", "r5"),
			Vector("y4", "r4", "g4", "r4", "b3")
		),
			playStacks = Some(Vector(0, 1, 1, 1, 1))
		)
		.pipe(takeTurn("Alice clues 1 to Bob"))
		.pipe(takeTurn("Bob discards g1", "r1"))

		// Alice's slot 5 should be chop moved.
		hasStatus(game, Alice, 5, CardStatus.ChopMoved)

class TrashPush extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("will interpret a trash push"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r2", "r2", "b1", "g1", "r1")
		),
			playStacks = Some(Vector(1, 1, 1, 1, 1))
		)
		.pipe(takeTurn("Alice clues 1 to Bob"))

		// Bob's slot 2 should be finessed.
		hasStatus(game, Bob, 2, CardStatus.Finessed)
		hasInfs(game, None, Bob, 2, Vector("r2", "y2", "g2", "b2", "p2"))

	test("will interpret receiving a trash push"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "g3", "b5", "b4", "y3")
		),
			playStacks = Some(Vector(5, 1, 3, 2, 1)),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slots 3,4,5)"))

		// Alice's slot 2 should be finessed.
		hasStatus(game, Alice, 2, CardStatus.Finessed)

	test("will interpret receiving a trash push by colour in rainbow"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "g3", "b5", "b4", "y3")
		),
			playStacks = Some(Vector(5, 0, 0, 0, 5)),
			starting = Bob,
			variant = TestVariant.Rainbow5
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 5)"))

		// Alice's slot 4 should be finessed.
		hasStatus(game, Alice, 4, CardStatus.Finessed)

	test("will interpret receiving a trash push finesse"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "g3", "b5", "r4", "y1"),
			Vector("r2", "g5", "p5", "p2", "y3")
		),
			playStacks = Some(Vector(2, 1, 3, 2, 1)),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 1 to Bob (slot 5)"))

		// Alice's slot 1 should be finessed as r3.
		hasStatus(game, Alice, 1, CardStatus.Finessed)
		hasInfs(game, None, Alice, 1, Vector("r3"))

	test("understands a trash push must be playable by its turn"):
		// Based on https://hanab.live/replay/1797088#52
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y3", "p1", "g1", "y2"),
			Vector("p1", "r2", "g4", "g1"),
			Vector("b2", "r1", "p3", "y3")
		),
			playStacks = Some(Vector(4, 1, 5, 1, 5)),
			starting = Cathy,
			init =
				preClue[HGroup](Donald, 1, Seq("2")) andThen
				preClue(Donald, 4, Seq("yellow")) andThen
				preClue(Bob, 4, Seq("2")) andThen
				(_.copy(inEarlyGame = false))
		)
		.pipe(takeTurn("Cathy clues 1 to Alice (slot 4)"))

		// Alice's slot 3 should be finessed as r5,b3, but not y4 as that cannot be playable on time.
		hasStatus(game, Alice, 3, CardStatus.Finessed)
		hasInfs(game, None, Alice, 3, Vector("r5", "b3"))

	test("understands others must make trash push playable by our turn"):
		// Based on https://hanab.live/replay/1797088#52
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p1", "r2", "g4", "g1", "p1"),
			Vector("b2", "r1", "p3", "y3", "g1")
		),
			playStacks = Some(Vector(4, 2, 5, 1, 5)),
			starting = Bob,
			init =
				preClue[HGroup](Cathy, 1, Seq("2")) andThen
				preClue(Cathy, 4, Seq("yellow")) andThen
				(_.copy(inEarlyGame = false))
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slot 5)"))
		.tap: g =>
			// Alice's slot 4 should be finessed as r5,b3,y4.
			hasStatus(g, Alice, 4, CardStatus.Finessed)
			hasInfs(g, None, Alice, 4, Vector("r5", "y4", "b3"))
		.pipe(takeTurn("Cathy plays y3", "y5"))

		// Alice should play her pushed card.
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Play(game.state.hands(Alice.ordinal)(3)))

	test("understands it must make trash push playable by target's turn"):
		// Based on https://hanab.live/replay/1797088#52
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b4", "b5", "r5", "y4", "p1"),
			Vector("r1", "r2", "g4", "g1", "p1")
		),
			playStacks = Some(Vector(4, 2, 5, 1, 5)),
			starting = Cathy,
			init =
				preClue[HGroup](Alice, 1, Seq("2")) andThen
				preClue(Alice, 4, Seq("3")) andThen
				(_.copy(inEarlyGame = false))
		)
		.pipe(takeTurn("Cathy clues 1 to Bob"))
		.tap: g =>
			// Bob's slot 4 should be finessed.
			hasStatus(g, Bob, 4, CardStatus.Finessed)

		// Alice should play the connecting card.
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Play(game.state.hands(Alice.ordinal)(3)))

class TrashFinesse extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("will interpret a rank trash finesse"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "p3", "b4", "g4", "g1"),
			Vector("y5", "g2", "g1", "p2", "b3")
		),
			playStacks = Some(Vector(0, 1, 2, 0, 0))
		)
		.pipe(takeTurn("Alice clues 1 to Cathy"))

		// Bob's slot 1 should be blind played.
		assert(game.isBlindPlaying(game.state.hands(Bob.ordinal)(0)))
		// Cathy's slots 4 and 5 should be chop moved.
		hasStatus(game, Cathy, 4, CardStatus.ChopMoved)
		hasStatus(game, Cathy, 5, CardStatus.ChopMoved)

	test("will interpret a rank trash finesse touching multiple cards"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "p3", "b4", "g4", "g1"),
			Vector("y5", "g2", "g1", "y1", "b3")
		),
			playStacks = Some(Vector(0, 1, 2, 0, 0))
		)
		.pipe(takeTurn("Alice clues 1 to Cathy"))

		// Bob's slot 1 should be blind played.
		assert(game.isBlindPlaying(game.state.hands(Bob.ordinal)(0)))
		// Both touched cards are marked as trash.
		assert(game.meta(game.state.hands(Cathy.ordinal)(2)).trash)
		assert(game.meta(game.state.hands(Cathy.ordinal)(3)).trash)
		// Cathy's slot 5 should be chop moved.
		hasStatus(game, Cathy, 5, CardStatus.ChopMoved)

	test("will interpret a colour trash finesse"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "p3", "b4", "g4", "g1"),
			Vector("y1", "g2", "b3", "p3", "y5")
		),
			playStacks = Some(Vector(0, 1, 3, 0, 0)),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 5 to Cathy"))
		.pipe(takeTurn("Cathy clues 5 to Alice (slot 5)"))
		// The clue looks like Alice is telling Cathy to play g4.
		// If Bob plays slot 1, it will prove that Alice's clued card was in fact trash, and
		// saves several useful cards in cathy's hand.
		.pipe(takeTurn("Alice clues green to Cathy"))

		// Bob's slot 1 should be blind played.
		assert(game.isBlindPlaying(game.state.hands(Bob.ordinal)(0)))
		// Cathy's slot 4 should be chop moved.
		hasStatus(game, Cathy, 4, CardStatus.ChopMoved)

	test("will interpret an ambiguous rank direct play"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "p3", "b4", "g4", "g1"),
			Vector("y5", "g2", "b1", "p2", "b3")
		),
			playStacks = Some(Vector(0, 1, 2, 0, 0))
		)
		.pipe(takeTurn("Alice clues 1 to Cathy"))
		.pipe(takeTurn("Bob discards g1", "p3"))

		// Cathy's slot 3 should be assumed playable.
		hasInfs(game, None, Cathy, 3, Vector("r1", "b1", "p1"))
		hasStatus(game, Cathy, 4, CardStatus.None)

	test("will interpret an ambiguous colour direct play"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "p3", "b4", "g4", "g1"),
			Vector("y5", "g4", "b1", "p2", "b3")
		),
			playStacks = Some(Vector(0, 1, 3, 0, 0)),
			starting = Alice
		)
		.pipe(takeTurn("Alice clues green to Cathy"))
		.pipe(takeTurn("Bob discards g1", "p3"))

		// Cathy's slot 2 should be assumed playable.
		hasInfs(game, None, Cathy, 2, Vector("g4"))
		hasStatus(game, Cathy, 4, CardStatus.None)

	test("will interpret a colour 5 direct play"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "p3", "b4", "g4", "g1"),
			Vector("y5", "g5", "b1", "p2", "b3")
		),
			playStacks = Some(Vector(0, 1, 4, 0, 0))
		)
		.pipe(takeTurn("Alice clues green to Cathy"))
		.pipe(takeTurn("Bob discards g1", "p3"))

		// Cathy's slot 2 should be assumed playable.
		hasInfs(game, None, Cathy, 2, Vector("g5"))

	test("will interpret a reverse rank trash finesse"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y5", "g2", "g1", "p2", "b3"),
			Vector("r1", "p3", "b4", "g4", "g1")
		),
			playStacks = Some(Vector(0, 1, 2, 1, 1))
		)
		.pipe(takeTurn("Alice clues 1 to Bob"))

		// Cathy's slot 1 should be finessed.
		hasStatus(game, Cathy, 1, CardStatus.Finessed)
		// Bob's slots 4 and 5 should be chop moved.
		hasStatus(game, Bob, 4, CardStatus.ChopMoved)
		hasStatus(game, Bob, 5, CardStatus.ChopMoved)

	test("will interpret a reverse colour trash finesse"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y5", "b1", "g2", "p2", "b5"),
			Vector("g4", "p3", "b4", "g4", "g1")
		),
			playStacks = Some(Vector(0, 1, 3, 1, 1)),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 5 to Bob"))
		.pipe(takeTurn("Alice clues green to Bob"))

		// Cathy's slot 1 should be finessed.
		hasStatus(game, Cathy, 1, CardStatus.Finessed)
		// Bob's slot 4 should be chop moved.
		hasStatus(game, Bob, 4, CardStatus.ChopMoved)

	test("will interpret receiving a rank trash finesse"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y5", "g2", "g4", "p2", "g1"),
			Vector("r1", "p3", "b4", "g4", "b3")
		),
			playStacks = Some(Vector(0, 1, 2, 0, 0)),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slot 3)"))

		// Cathy's slot 1 should be blind played.
		assert(game.isBlindPlaying(game.state.hands(Cathy.ordinal)(0)))
		// Alice's slots 4 and 5 should be chop moved.
		hasStatus(game, Alice, 4, CardStatus.ChopMoved)
		hasStatus(game, Alice, 5, CardStatus.ChopMoved)

	test("will interpret receiving a rank trash finesse touching multiple cards"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y5", "g2", "g4", "p2", "g1"),
			Vector("r1", "p3", "b4", "g4", "b3")
		),
			playStacks = Some(Vector(0, 1, 2, 0, 0)),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slots 3,4)"))

		// Cathy's slot 1 should be blind played.
		assert(game.isBlindPlaying(game.state.hands(Cathy.ordinal)(0)))
		// Alice's slots 3 and 4 are known trash.
		assert(game.meta(game.state.hands(Alice.ordinal)(2)).trash)
		assert(game.meta(game.state.hands(Alice.ordinal)(3)).trash)
		// Alice's slot 5 should be chop moved.
		hasStatus(game, Alice, 5, CardStatus.ChopMoved)

	test("will interpret receiving a colour trash finesse"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y5", "g2", "p4", "p2", "r1"),
			Vector("g4", "p3", "b4", "g4", "b3")
		),
			playStacks = Some(Vector(0, 1, 3, 0, 0)),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 5 to Alice (slot 5)"))
		.pipe(takeTurn("Alice clues 1 to Bob"))
		.pipe(takeTurn("Bob clues green to Alice (slot 3)"))

		// Cathy's slot 1 should be blind played.
		hasStatus(game, Cathy, 1, CardStatus.Finessed)
		// Alice's slot 4 should be chop moved.
		hasStatus(game, Alice, 4, CardStatus.ChopMoved)

	test("will interpret receiving a colour trash bluff"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y5", "g2", "p4", "p2", "p1"),
			Vector("r1", "p3", "b4", "g4", "b3")
		),
			playStacks = Some(Vector(0, 1, 3, 0, 0)),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 5 to Alice (slot 5)"))
		.pipe(takeTurn("Alice clues purple to Bob"))
		.pipe(takeTurn("Bob clues green to Alice (slot 3)"))
		.tap: g =>
			// Cathy's slot 1 should be blind played.
			assert(g.isBlindPlaying(g.state.hands(Cathy.ordinal)(0)))
			// Alice's slot 4 should be chop moved.
			hasStatus(g, Alice, 4, CardStatus.ChopMoved)
		.pipe(takeTurn("Cathy plays r1", "b5"))

		// Alice's slot 3 must be known trash.
		hasInfs(game, None, Alice, 3, Vector("g1", "g2", "g3"))
		hasStatus(game, Alice, 4, CardStatus.ChopMoved)

	test("will interpret receiving a colour play when possible trash bluff isn't played"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y5", "g2", "p4", "p2", "p1"),
			Vector("r1", "p3", "b4", "g4", "b3")
		),
			playStacks = Some(Vector(0, 1, 3, 0, 0)),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 5 to Alice (slot 5)"))
		.pipe(takeTurn("Alice clues purple to Bob"))
		.pipe(takeTurn("Bob clues green to Alice (slot 3)"))
		.tap: g =>
			// Cathy's slot 1 should be blind played.
			assert(g.isBlindPlaying(g.state.hands(Cathy.ordinal)(0)))
			// Alice's slot 4 should be chop moved.
			hasStatus(g, Alice, 4, CardStatus.ChopMoved)
		.pipe(takeTurn("Cathy discards b3", "p5"))

		// Alice has a direct play.
		hasInfs(game, None, Alice, 3, Vector("g4"))
		// Alice is no longer chop moved.
		hasStatus(game, Alice, 4, CardStatus.None)

	test("will interpret receiving an ambiguous rank direct play"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y5", "g2", "g4", "p2", "g1"),
			Vector("r1", "p3", "b4", "g4", "b3")
		),
			playStacks = Some(Vector(0, 1, 2, 0, 0)),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slot 3)"))
		.tap: g =>
			// Cathy's slot 1 is expected to blind play.
			assert(g.isBlindPlaying(g.state.hands(Cathy.ordinal)(0)))
			// Alice's slots 4 and 5 should be chop moved.
			hasStatus(g, Alice, 4, CardStatus.ChopMoved)
			hasStatus(g, Alice, 5, CardStatus.ChopMoved)
		.pipe(takeTurn("Cathy discards g4", "p3"))

		// Alice's slot 3 should be assumed playable.
		hasInfs(game, None, Alice, 3, Vector("r1", "b1", "p1"))
		// Alice is not chop moved.
		hasStatus(game, Alice, 4, CardStatus.None)

	test("will interpret receiving a reverse rank trash finesse"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "p3", "b4", "g4", "b3"),
			Vector("y5", "g2", "g4", "p3", "g1")
		),
			playStacks = Some(Vector(0, 1, 2, 1, 1)),
			starting = Cathy
		)
		// Alice can see that the identity of the promised play matches Bob's r1.
		// She should treat it as a reverse trash finesse and chop move her slots 4 and 5.
		.pipe(takeTurn("Cathy clues 1 to Alice (slot 3)"))
		.tap: g =>
			hasStatus(g, Alice, 4, CardStatus.ChopMoved)
			hasStatus(g, Alice, 5, CardStatus.ChopMoved)
			hasInfs(g, None, Alice, 3, Vector("r1", "y1", "g1", "b1", "p1"))

			// Alice should discard her slot 3 to show that the finesse is recognized.
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Discard(g.state.hands(Alice.ordinal)(2)))
		.pipe(takeTurn("Alice discards r1 (slot 3)"))

		// After the discard, slots 4 and 5 are still chop moved.
		hasStatus(game, Alice, 4, CardStatus.ChopMoved)
		hasStatus(game, Alice, 5, CardStatus.ChopMoved)
		assert(game.isBlindPlaying(game.state.hands(Bob.ordinal)(0)))

	test("will interpret receiving a reverse colour trash finesse"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "p3", "b4", "g4", "b3"),
			Vector("y5", "g2", "y4", "p3", "g1")
		),
			playStacks = Some(Vector(0, 1, 3, 1, 1)),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 5 to Alice (slot 5)"))
		// Alice can see that the identity of the promised play matches Bob's g4.
		// She should treat it as a reverse trash finesse and chop move her slot 4.
		.pipe(takeTurn("Cathy clues green to Alice (slot 3)"))
		.tap: g =>
			hasStatus(g, Alice, 4, CardStatus.ChopMoved)
			hasInfs(g, None, Alice, 3, Vector("g1", "g2", "g3", "g4"))

			// Alice should discard her slot 3 to show that the finesse is recognized.
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Discard(g.state.hands(Alice.ordinal)(2)))
		.pipe(takeTurn("Alice discards g1 (slot 3)"))

		// After the discard, slot 4 is still chop moved.
		hasStatus(game, Alice, 4, CardStatus.ChopMoved)
		assert(game.isBlindPlaying(game.state.hands(Bob.ordinal)(0)))

	test("will interpret receiving a play when an ambiguous reverse rank trash finesse is visible"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "p3", "b4", "g4", "b3"),
			Vector("y5", "g2", "g4", "p3", "g1")
		),
			playStacks = Some(Vector(0, 1, 2, 0, 0)),
			starting = Cathy
		)
		// This could be a trash finesse on the r1, however Alice can see
		// there are other playable 1s and so should treat it as a straight play.
		.pipe(takeTurn("Cathy clues 1 to Alice (slot 3)"))

		// Alice's slot 3 should be assumed a playable 1.
		hasInfs(game, None, Alice, 3, Vector("r1", "b1", "p1"))

	test("interprets a fill-in trash finesse"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "b4", "r1", "b3", "g4"),
			Vector("y2", "r1", "g4", "b3", "r5")
		),
			playStacks = Some(Vector(3, 0, 0, 0, 0)),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 5 to Cathy"))
		.pipe(takeTurn("Cathy clues 5 to Alice (slot 5)"))
		// This fills in the r5 and clues the r1 as a play.
		.pipe(takeTurn("Alice clues red to Cathy"))
		.tap: g =>
			// Bob's slot 1 should be blind played.
			assert(g.isBlindPlaying(g.state.hands(Bob.ordinal)(0)))
			// Cathy's slot 2 is known trash
			assert(g.meta(g.state.hands(Cathy.ordinal)(1)).trash)
			// Cathy's slots 3 and 4 should be chop moved.
			hasStatus(g, Cathy, 3, CardStatus.ChopMoved)
			hasStatus(g, Cathy, 4, CardStatus.ChopMoved)
			// Cathy's slot 5 is not trash
			assert(!g.meta(g.state.hands(Cathy.ordinal)(4)).trash)
		.pipe(takeTurn("Bob plays r4", "g2"))

		hasInfs(game, None, Cathy, 2, Vector("r1", "r2", "r3"))
		// Cathy's slots 3 and 4 remain chop moved.
		hasStatus(game, Cathy, 3, CardStatus.ChopMoved)
		hasStatus(game, Cathy, 4, CardStatus.ChopMoved)

	test("interprets receiving a reverse trash finesse"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "b4", "r1", "b3", "g4"),
			Vector("y3", "g3", "g4", "b1", "r5")
		),
			playStacks = Some(Vector(0, 1, 1, 1, 1)),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 1 to Cathy"))
		.pipe(takeTurn("Cathy discards b1", "y3"))
		.tap: g =>
			// Alice's slot 1 should be r1 blind playing.
			hasInfs(g, None, Alice, 1, Vector("r1"))
			assert(g.isBlindPlaying(g.state.hands(Alice.ordinal)(0)))

			// Cathy's slot 5 is chop moved.
			hasStatus(g, Cathy, 5, CardStatus.ChopMoved)
		.pipe(takeTurn("Alice plays r1 (slot 1)"))

		// Cathy is still chop moved.
		hasStatus(game, Cathy, 5, CardStatus.ChopMoved)

	test("interprets receiving a trash bluff"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y3", "g3", "g4", "b1", "r5"),
			Vector("r4", "b4", "r1", "b3", "g4")
		),
			playStacks = Some(Vector(0, 1, 1, 1, 1)),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 1 to Bob"))
		.tap: g =>
			// Alice's slot 1 should be blind playing.
			hasInfs(g, None, Alice, 1, Vector("r1", "y2", "g2", "b2", "p2"))
			hasInfs(g, None, Bob, 4, Vector("r1", "y1", "g1", "b1", "p1"))
			assert(g.isBlindPlaying(g.state.hands(Alice.ordinal)(0)))

			// Bob's slot 5 is chop moved.
			hasStatus(g, Bob, 5, CardStatus.ChopMoved)
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Play(g.state.hands(Alice.ordinal)(0)))
		.pipe(takeTurn("Alice plays g2 (slot 1)"))

		// Bob is still chop moved.
		hasInfs(game, None, Bob, 4, Vector("y1", "g1", "b1", "p1"))
		hasStatus(game, Bob, 5, CardStatus.ChopMoved)

	test("recognizes real prompt over false trash finesse"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b1", "g1", "r5", "r4"),
			Vector("g3", "r2", "y1", "g4"),
			Vector("g4", "y3", "b3", "p4")
		),
			playStacks = Some(Vector(1, 2, 2, 3, 5)),
			starting = Bob,
			init =
				preClue[HGroup](Cathy, 2, Seq("2")) andThen
				(_.copy(inEarlyGame = false))
		)
		.pipe(takeTurn("Bob clues 3 to Alice (slot 2)"))
		.tap: g =>
			hasStatus(g, Alice, 3, CardStatus.ChopMoved)
			hasStatus(g, Alice, 4, CardStatus.ChopMoved)
		.pipe(takeTurn("Cathy plays r2", "g5"))

		// After Cathy doesn't play possible trash bluff, we are no longer chop moved and can infer regular connections.
		hasInfs(game, None, Alice, 2, Vector("r3", "y3", "g3"))
		hasStatus(game, Alice, 3, CardStatus.None)
		hasStatus(game, Alice, 4, CardStatus.None)

	test("interprets trash touch elimination on a 5"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "g1", "r1", "y3", "y4"),
			Vector("g3", "r2", "y1", "r5", "g4")
		),
			playStacks = Some(Vector(4, 2, 2, 0, 1))
		)
		.pipe(takeTurn("Alice clues red to Bob"))

		// Should be interpreted as a trash chop move.
		hasStatus(game, Bob, 4, CardStatus.ChopMoved)
		hasStatus(game, Bob, 5, CardStatus.ChopMoved)

		// No blind play expected as there is only one useful red card.
		assert(!game.isBlindPlaying(game.state.hands(Cathy.ordinal)(0)))

	test("interprets trash touch elimination on possible card in our hand"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "r2", "y1", "y4", "g4"),
			Vector("b1", "g1", "r1", "y3", "y4")
		),
			playStacks = Some(Vector(4, 2, 2, 0, 1)),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Cathy"))

		// Should be interpreted as a trash chop move.
		hasStatus(game, Cathy, 4, CardStatus.ChopMoved)
		hasStatus(game, Cathy, 5, CardStatus.ChopMoved)

		// No blind play expected as there is only one useful red card.
		assert(!game.isBlindPlaying(game.state.hands(Alice.ordinal)(0)))

	test("shouldn't interpret playing a trash finesse if the card could already be blind playing"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g2", "g5", "g3", "y1", "p3"),
			Vector("r2", "y3", "r5", "y4", "r4")
		),
			playStacks = Some(Vector(0, 0, 0, 1, 0)),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 3 to Alice (slot 5)"))
		.tap: g =>
			assert(g.isBlindPlaying(g.state.hands(Alice.ordinal)(0)))
		.pipe(takeTurn("Cathy clues 1 to Bob"))

		// Since Alice was already going to play, this cannot be a trash finesse and should instead be interpreted as a play.
		hasInfs(game, None, Bob, 4, Vector("r1", "y1", "g1", "p1"))
		hasStatus(game, Bob, 5, CardStatus.None)

	test("should recognize own trash finesse where necessary to connect before target"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b3", "b4", "p5", "r5"),
			Vector("r2", "p2", "p1", "y3"),
			Vector("r1", "y4", "y5", "r3")
		),
			playStacks = Some(Vector(0, 3, 4, 4, 3)),
			starting = Bob
		)
		// r1 -> r2 -> r3
		.pipe(takeTurn("Bob clues 3 to Donald"))
		// Trash finesse on b4.
		.pipe(takeTurn("Cathy clues 4 to Bob"))

		// Since Donald already has a blind play, this cannot be a trash bluff and Alice must play [y4,p4] as a trash finesse.
		hasInfs(game, None, Alice, 1, Vector("y4", "p4"))
		hasStatus(game, Bob, 3, CardStatus.ChopMoved)
		hasStatus(game, Bob, 4, CardStatus.ChopMoved)

	test("will interpret receiving a direct play on a known good card"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b3", "p4", "p2", "b5"),
			Vector("y5", "b4", "r1", "g2"),
			Vector("g1", "b4", "p1", "g4")
		),
			playStacks = Some(Vector(5, 4, 5, 1, 4)),
			starting = Bob,
			discarded = Vector("r2", "y2")		// We can see all 2s except both copies of b2.
		)
		.pipe(takeTurn("Bob clues 2 to Alice (slot 3)"))
		// Cathy is never trash finessed/bluffed here, but that isn't common knowledge.
		.pipe(takeTurn("Cathy discards g2", "p3"))

		hasInfs(game, None, Alice, 3, Vector("b2"))
		hasStatus(game, Alice, 4, CardStatus.None)

	test("doesn't keep playing after a trash finesse"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y4", "p3", "b4", "g2", "b3"),
			Vector("y5", "g3", "g4", "r1", "b3")
		),
			playStacks = Some(Vector(3, 0, 0, 0, 0)),
			discarded = Vector("r4"),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Cathy"))
		.pipe(takeTurn("Cathy discards r1", "p1"))
		.pipe(takeTurn("Alice plays r4 (slot 1)"))

		// Alice shouldn't keep playing for r5.
		hasStatus(game, Alice, 2, CardStatus.None)

	test("understands a trash finesse where we may have the last playable identity"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y4", "p3", "b4", "g2"),
			Vector("y5", "g3", "g4", "r1"),
			Vector("r4", "b3", "b3", "y4")
		),
			playStacks = Some(Vector(3, 0, 0, 0, 0)),
			discarded = Vector("r4"),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Cathy"))
		.tap: g =>
			assert(g.meta(g.state.hands(Cathy.ordinal)(3)).trash)
		.pipe(takeTurn("Cathy discards r1", "p1"))
		.pipe(takeTurn("Donald plays r4", "g5"))

		// Alice shouldn't keep playing for r5.
		hasStatus(game, Alice, 1, CardStatus.None)

	test("plays into an ambiguous trash finesse"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y4", "g1", "r1", "b3"),
			Vector("p4", "p3", "p5", "p3"),
			Vector("b1", "g5", "r1", "y1")
		),
			playStacks = Some(Vector(3, 3, 3, 0, 0)),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 1 to Bob"))
		.pipe(takeTurn("Donald discards y1", "b4"))

		hasStatus(game, Alice, 1, CardStatus.Finessed)

	test("understands a trash finesse where giver knows they have the last id"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r4", "g1", "r1", "b3"),
			Vector("p4", "p3", "p5", "p3"),
			Vector("b1", "g5", "r1", "r5")
		),
			playStacks = Some(Vector(3, 3, 3, 0, 0)),
			init =
				fullyKnown[HGroup](Donald, 4, "r5") andThen
				(_.copy(inEarlyGame = false)),
			starting = Donald
		)
		// Donald knows they have r5, so Alice should still recognize this as a trash finesse.
		.pipe(takeTurn("Donald clues red to Alice (slot 3)"))

		assert(game.isBlindPlaying(game.state.hands(Bob.ordinal)(0)))
		assert(game.meta(game.state.hands(Alice.ordinal)(2)).trash)
		hasStatus(game, Alice, 4, CardStatus.ChopMoved)

	test("prefers a bluff over a trash bluff"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g2", "b3", "r4", "p3", "r1"),
			Vector("r4", "p2", "g4", "y5", "y1")
		),
			playStacks = Some(Vector(3, 4, 2, 2, 2)),
			clueTokens = 4,
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 4 to Alice (slot 4)"))
		.pipe(takeTurn("Cathy plays r4", "y2"))

		// Cathy would need to discharge to prove trash.
		hasInfs(game, None, Alice, 4, Vector("g4", "b4", "p4"))
		hasStatus(game, Alice, 5, CardStatus.None)

	test("won't give a bad trash finesse"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "p3", "b4", "g2", "b3"),
			Vector("y5", "g3", "g4", "r1", "b3")
		),
			playStacks = Some(Vector(3, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues red to Cathy"))

		// Red to Cathy is not a valid clue as Cathy will think it is r5.
		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("won't chop move trash"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p2", "p2", "g1", "r1", "r1"),
			Vector("p4", "y5", "b4", "r2", "y1")
		),
			playStacks = Some(Vector(3, 3, 3, 3, 1))
		)
		.pipe(takeTurn("Alice clues 2 to Cathy"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("won't give a trash finesse starting with target"):
		// Based on https://hanab.live/replay/1797812#29
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "b4", "y4", "b2", "y2"),
			Vector("r2", "g4", "g5", "p2", "p3")
		),
			playStacks = Some(Vector(0, 1, 2, 2, 2)),
			starting = Cathy
		)
		// Bob knows about his 2's
		.pipe(takeTurn("Cathy clues 2 to Bob"))
		// Alice should not clue 2 to Bob - Bob needs to play to allow Cathy's play but would
		// need to see if Cathy plays to know if it is in fact a trash finesse.
		.pipe(takeTurn("Alice clues 2 to Bob"))

		assertNotEquals(game.lastMove, Some(ClueInterp.Play))
