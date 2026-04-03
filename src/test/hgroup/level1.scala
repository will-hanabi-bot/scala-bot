package tests.hgroup.level1

import cats.effect.unsafe.implicits.global

import scala_bot.basics._
import scala_bot.test.{hasInfs, hasStatus, Player, setup, takeTurn}, Player._
import scala_bot.hgroup.{getResult, HGroup}

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger,LogLevel}

class General extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("does not finesse from a 2 Save"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r5", "r4", "r2", "y4", "y2"),
			Vector("g5", "b4", "g1", "y2", "b3")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 2 to Bob"))

		hasInfs(game, None, Bob, 5, Vector("r2", "y2", "g2", "b2", "p2"))

		assert(game.common.thoughts(game.state.hands(Alice.ordinal)(0)).inferred.nonEmpty)
		hasStatus(game, Alice, 1, CardStatus.None)

	test("counts playables connecting on unknown plays"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "g1", "p5", "r2", "y2"),
			Vector("g2", "g3", "p2", "p1", "b4")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 1 to Bob"))

		val hypo = takeTurn("Alice clues 2 to Cathy")(game)

		val (_, playables) = playablesResult(game, hypo)
		assertEquals(playables.length, 1)

	test("doesn't clue a duplicate of a commonly unknown play"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r4", "g1", "y4", "y4"),
			Vector("r4", "g1", "b1", "y1"),
			Vector("g4", "g4", "b4", "b4")
		),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues 1 to Cathy"))

		val hypo = takeTurn("Alice clues 1 to Bob")(game)

		val (_, playables) = playablesResult(game, hypo)
		assert(playables.isEmpty)

		val action = ClueAction(Alice.ordinal, Bob.ordinal, Seq(game.state.hands(Bob.ordinal)(1)), BaseClue(ClueKind.Rank, 1))

		assertEquals(getResult(game, hypo, action), -100.0)

	test("connects an unknown playable on self"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r4", "b4", "g4", "y3"),
			Vector("y1", "y2", "b4", "g4"),
			Vector("y4", "r4", "p4", "g2")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slots 2,3,4)"))
		.pipe(takeTurn("Cathy clues 2 to Donald"))
		.pipe(takeTurn("Donald clues 2 to Cathy"))
		.tap: g =>
			hasInfs(g, None, Cathy, 2, Vector("r2", "y2", "g2", "b2", "p2"))
			// Alice is promised to have y1, since Cathy will never self-finesse.
			assert(g.common.links.exists:
				case Link.Promised(orders, id, _) =>
					orders.toSet == Set(0, 1, 2) && id == g.state.expandShort("y1")
				case _ => false
			)
			assertEquals(g.lastMove, Some(ClueInterp.Play))
		.pipe(takeTurn("Alice clues yellow to Bob"))
		.tap: g =>
			// hasInfs(g, None, Bob, 4, Vector("y3"))
			assertEquals(g.lastMove, Some(ClueInterp.Play))
		.pipe(takeTurn("Bob clues 2 to Donald"))
		.pipe(takeTurn("Cathy clues 2 to Alice (slot 1)"))

		// Cathy demonstrated that we have y1.
		assert(game.common.thoughts(game.state.hands(Cathy.ordinal)(0)).inferred.length > 5, s"Expected nothing special, got ${game.common.strInfs(game.state, game.state.hands(Cathy.ordinal)(0))}")
		hasStatus(game, Cathy, 1, CardStatus.None)

	test("connects an unknown playable through other"):
		val game = setup(HGroup.atLevel(9), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y1", "y2", "b4", "g4"),
			Vector("y4", "r4", "p4", "g2"),
			Vector("r2", "g1", "b1", "y1"),
		))
		.pipe(takeTurn("Alice clues 1 to Donald"))
		.pipe(takeTurn("Bob clues 2 to Cathy"))
		.pipe(takeTurn("Cathy clues 2 to Bob"))
		.tap: g =>
			hasInfs(g, None, Bob, 2, Vector("y2", "g2", "b2"))
			assertEquals(g.lastMove, Some(ClueInterp.Play))
		.pipe(takeTurn("Donald clues yellow to Alice (slot 4)"))

		hasInfs(game, None, Alice, 4, Vector("y3"))
		assertEquals(game.lastMove, Some(ClueInterp.Play))

	test("plays the focus first"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "y1", "g1", "b1", "p1"),
			Vector("r1", "y1", "g1", "b1", "p1"),
		),
			starting = Cathy,
			playStacks = Some(Vector(3, 3, 3, 3, 3))
		)
		.pipe(takeTurn("Cathy clues 4 to Alice (slots 2,4,5)"))

		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Play(game.state.hands(Alice.ordinal)(4)))

	test("gives a 5 save even if a 2 will be lost"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "g4", "b4", "y2", "r5")
		))

		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Rank(Bob.ordinal, 5))

	test("entertains all possible connections"):
		val game = setup(HGroup.atLevel(5), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g1", "p4", "g4", "b3"),		// Bob is holding the crit b3
			Vector("g4", "p5", "b1", "p1"),
			Vector("r4", "r4", "y4", "y5")
		),
			starting = Bob,
			playStacks = Some(Vector(2, 1, 0, 0, 0)),
			discarded = Vector("b3")
		)
		.pipe(takeTurn("Bob clues 1 to Cathy"))
		.pipe(takeTurn("Cathy clues green to Bob"))
		.pipe(takeTurn("Donald clues 2 to Alice (slot 3)"))
		.tap: g =>
			hasInfs(g, None, Alice, 3, Vector("y2", "g2", "b2", "p2"))
		.pipe(takeTurn("Alice clues 5 to Donald"))
		.pipe(takeTurn("Bob clues 5 to Cathy"))

		.pipe(takeTurn("Cathy plays p1", "r1"))
		.pipe(takeTurn("Donald clues 3 to Alice (slot 1)"))		// x3 xx x2 xx

		// Alice cannot play slot 1, since it might be y3 or p3.
		// She also can't play slot 3, since it might be b2.
		assert(game.me.thinksPlayables(game, Alice.ordinal).isEmpty)
