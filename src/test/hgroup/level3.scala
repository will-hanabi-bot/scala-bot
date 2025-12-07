package tests.hgroup.level3

import scala_bot.basics._
import scala_bot.test.{fullyKnown, hasInfs, Player, setup, takeTurn}, Player._
import scala_bot.hgroup.HGroup
import scala_bot.logger.{Logger, LogLevel}

import scala.util.chaining.scalaUtilChainingOps

class Playing1s extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("orders starting hand 1s from right to left") {
		val game = setup(HGroup.atLevel(3), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y5", "r5", "b1", "p3", "y4")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slots 3,4)"))

		val ordered1s = game.order1s(game.state.hands(Alice.ordinal))
		assertEquals(ordered1s, List(1, 2))
	}

	test("orders fresh 1s") {
		val game = setup(HGroup.atLevel(3), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y5", "r5", "b1", "p3", "y4")
		))
		.pipe(takeTurn("Alice bombs b4 (slot 1)"))
		.pipe(takeTurn("Bob clues 1 to Alice (slots 1,4)"))

		val ordered1s = game.order1s(game.state.hands(Alice.ordinal))
		assertEquals(ordered1s, List(10, 1))
	}

	test("orders chop focus") {
		val game = setup(HGroup.atLevel(3), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y5", "r5", "b1", "p3", "y4")
		))
		.pipe(takeTurn("Alice bombs b4 (slot 1)"))
		.pipe(takeTurn("Bob clues 1 to Alice (slots 1,2,5)"))

		val ordered1s = game.order1s(game.state.hands(Alice.ordinal))
		assertEquals(ordered1s, Seq(0, 10, 3))
	}

	test("doesn't prompt playable 1s") {
		val game = setup(HGroup.atLevel(3), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b2", "r2", "g3", "r5", "b3"),
			Vector("r4", "b4", "g4", "y3", "p4")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slots 2,3)"))
		.pipe(takeTurn("Cathy clues red to Bob"))

		// Alice's 1 can still be any 1 (not prompted to be r1).
		hasInfs(game, None, Alice, 2, Vector("r1", "y1", "g1", "b1", "p1"))
	}

	test("recognizes the correct focus of a 1 clue") {
		val game = setup(HGroup.atLevel(3), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b2", "r2", "g3", "r5", "b3"),
			Vector("r4", "b1", "g4", "y1", "p4")
		))

		val clue = BaseClue(ClueKind.Rank, 1)
		val action = ClueAction(
			Alice.ordinal,
			Cathy.ordinal,
			game.state.clueTouched(game.state.hands(Cathy.ordinal), clue),
			clue
		)
		val focusResult = game.determineFocus(game, action)

		// The focus of the clue is Cathy's slot 4.
		assertEquals(focusResult.focus, game.state.hands(Cathy.ordinal)(3))
	}

	test("orders 1s correctly when cluing chop moved cards") {
		val game = setup(HGroup.atLevel(3), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "b1", "r4", "y4", "g1")
		),
			init = (game) =>
				// Bob's g1 is chop moved.
				game.withMeta(game.state.hands(Bob.ordinal)(4))(_.copy(status = CardStatus.ChopMoved))
		)
		.pipe(takeTurn("Alice clues 1 to Bob"))

		val ordered1s = game.order1s(game.state.hands(Bob.ordinal))
		assertEquals(ordered1s, Seq(8, 9, 5))
	}

	test("orders 1s correctly when only cluing chop moved cards") {
		val game = setup(HGroup.atLevel(3), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y4", "b4", "r4", "r1", "y1")
		),
			init = (game) =>
				// Bob's 1s are chop moved.
				game.withMeta(game.state.hands(Bob.ordinal)(3))(_.copy(status = CardStatus.ChopMoved))
					.withMeta(game.state.hands(Bob.ordinal)(4))(_.copy(status = CardStatus.ChopMoved))
		)
		.pipe(takeTurn("Alice clues 1 to Bob"))

		val ordered1s = game.order1s(game.state.hands(Bob.ordinal))
		assertEquals(ordered1s, Seq(6, 5))
	}

class Sarcastic extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("understands a sarcastic discard") {
		val game = setup(HGroup.atLevel(3), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "b4", "g4", "y3", "y1")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 1 to Alice (slot 4)"))
		.pipe(takeTurn("Alice clues yellow to Bob"))
		.pipe(takeTurn("Bob discards y1", "r1"))

		hasInfs(game, None, Alice, 4, Vector("y1"))
	}

	test("sarcastic discards without assuming position") {
		val game = setup(HGroup.atLevel(3), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p4", "g2", "b2", "r4", "p2")
		),
			playStacks = Some(Vector(1, 1, 0, 1, 0)),
			clueTokens = 6
		)
		.pipe(takeTurn("Alice clues 2 to Bob"))
		.pipe(takeTurn("Bob clues blue to Alice (slot 1)"))
		.pipe(takeTurn("Alice discards b2 (slot 1)"))

		assert(List(1, 2, 4).forall { i =>
			val order = game.state.hands(Bob.ordinal)(i)
			val inferred = game.players(Bob.ordinal).thoughts(order).inferred
			inferred.contains(game.state.expandShort("b2"))
		})
	}

	test("preserves info lock after a sarcastic discard") {
		val game = setup(HGroup.atLevel(3), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("p4", "g2", "b2", "r4", "p2"),
			Vector("y3", "y1", "b5", "g3", "r2")
		),
			starting = Bob,
			playStacks = Some(Vector(0, 1, 1, 2, 2)),
			clueTokens = 6,
			init = fullyKnown(Cathy, 5, "r2")
		)
		.pipe(takeTurn("Bob clues 2 to Alice (slots 1,2)"))
		.pipe(takeTurn("Cathy discards r2", "p5"))

		hasInfs(game, None, Alice, 1, Vector("y2", "g2"))
		hasInfs(game, None, Alice, 2, Vector("r2"))
	}

	test("generates a link from a sarcastic discard") {
		val game = setup(HGroup.atLevel(3), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y3", "g2", "b2", "r4", "b5"),
			Vector("y4", "y1", "p2", "g3", "r2")
		),
			starting = Bob,
			playStacks = Some(Vector(1, 1, 0, 0, 0)),
			clueTokens = 6,
			init = fullyKnown(Cathy, 5, "r2")
		)
		.pipe(takeTurn("Bob clues 2 to Alice (slots 3,4,5)"))
		.pipe(takeTurn("Cathy discards r2", "g1"))
		.tap { g =>
			assert(g.common.links.exists {
				case Link.Sarcastic(orders, id) =>
					orders.contains(1) &&
					orders.contains(2) &&
					id.matches(g.state.expandShort("r2"))
				case _ => false
			})
		}
		.pipe(takeTurn("Alice clues 5 to Bob"))
		.pipe(takeTurn("Bob clues green to Alice (slot 3)"))

		hasInfs(game, None, Alice, 4, Vector("r2"))
	}
