package scala_bot.basics

import scala.collection.immutable.BitSet
import scala.util.chaining.scalaUtilChainingOps
import scala_bot.utils._
import scala_bot.logger.Log

extension[G <: Game](game: G)
	def onClue(action: ClueAction)(using ops: GameOps[G]): G =
		val state = game.state
		val ClueAction(giver, target, list, clue) = action
		val BaseClue(kind, value) = clue
		val newPossible = IdentitySet.from(state.variant.touchPossibilities(clue))

		state.hands(target).foldLeft(game) { (newGame, order) =>
			if (list.contains(order))
				val touchedGame = newGame.withCard(order)(c => c.copy(
						clued = true,
						clues = c.clues :+ CardClue(kind, value, giver, state.turnCount)
					))
					.withThought(order)(t => t.copy(
						inferred = t.inferred.intersect(newPossible),
						possible = t.possible.intersect(newPossible),
						infoLock = t.infoLock.map(_.intersect(newPossible))
					))

				val newThought = touchedGame.common.thoughts(order)

				touchedGame
					.when(_ => newThought.possible.length == 1) { g =>
						val id = newThought.possible.head
						g.withCard(order)(c => c.copy(suitIndex = id.suitIndex, rank = id.rank))
							.pipe(ops.copyWith(_, GameUpdates(deckIds = Some(g.deckIds.updated(order, Some(id))))))
					}
					.when(_ => newThought.inferred.length < game.common.thoughts(order).inferred.length)
						(_.withMeta(order)(_.reason(state.turnCount)))
			else
				newGame.withThought(order)(t => t.copy(
					inferred = t.inferred.difference(newPossible),
					possible = t.possible.difference(newPossible),
					infoLock = t.infoLock.map(_.difference(newPossible))
				))
		}
		.withState { s => s.copy(
			endgameTurns = s.endgameTurns.map(_ - 1),
			clueTokens = s.clueTokens - 1
		)}

	def onDiscard(action: DiscardAction)(using ops: GameOps[G]): G =
		val DiscardAction(playerIndex, order, suitIndex, rank, failed) = action
		val id = Identity(suitIndex, rank)

		game.withState { s =>
			s.copy(
				hands = s.hands.updated(playerIndex, s.hands(playerIndex).filter(_ != order)),
				endgameTurns = s.endgameTurns.map(_ - 1)
			)
			.cond(_ => failed)
				(_.copy(strikes = s.strikes + 1))
				(_.regainClue)
		}
		.when(_ => suitIndex != -1 && rank != -1) { g =>
			g.withState(_.withDiscard(id, order))
			.withId(order, id)
			.withThought(order)
				(_.copy(suitIndex, rank, inferred = IdentitySet.single(id), possible = IdentitySet.single(id)))
		}

	def onDraw(action: DrawAction)(using ops: GameOps[G]): G =
		val DrawAction(playerIndex, order, suitIndex, rank) = action

		val id = Option.when(suitIndex != -1 && rank != -1) {
			game.deckIds.lift(order).flatten.foreach { id =>
				if (id != Identity(suitIndex, rank))
					throw new Exception(s"drew ${game.state.logId(Identity(suitIndex, rank))}, expected ${game.state.logId(id)} ${game.deckIds.map(game.state.logId)} ${order}")
			}
			Identity(suitIndex, rank)
		}

		game.pipe { g =>
			val deckIds = g.deckIds

			if (deckIds.length == order)
				ops.copyWith(g, GameUpdates(deckIds = Some(deckIds :+ id)))
			else if (deckIds.length > order)
				if (deckIds(order).isEmpty)
					ops.copyWith(g, GameUpdates(deckIds = Some(deckIds.updated(order, id))))
				else
					g
			else
				throw new IllegalArgumentException(s"Only have ${deckIds.length} deck ids, but drew card with order $order! ${g.state.hands}")
		}
		.tap { g =>
			assert(g.state.deck.length == order, "deck length doesn't match drawn order")
			assert(g.state.deck.length == g.state.nextCardOrder, "deck length doesn't match next order")
			assert(g.common.thoughts.length == g.players(0).thoughts.length, "common thoughts length differs from player 0's thoughts")
			assert(g.common.thoughts.length == g.meta.length, "common thoughts length differs from meta length")
		}
		.withState { s =>
			s.copy(
				hands = s.hands.updated(playerIndex, order +: s.hands(playerIndex)),
				deck = s.deck :+ Card(suitIndex, rank, order, s.turnCount),
				nextCardOrder = order + 1,
				cardsLeft = s.cardsLeft - 1
			)
			.when(s => s.cardsLeft == 0 && s.endgameTurns.isEmpty)
				(_.copy(endgameTurns = Some(s.numPlayers)))
		}
		.when(order == _.meta.length) { g =>
			ops.copyWith(g, GameUpdates(
				players = Some(g.players.zipWithIndex.map((player, i) => player.copy(
					thoughts = player.thoughts :+ Thought(
						if (i != playerIndex) suitIndex else -1,
						if (i != playerIndex) rank else -1,
						order,
						player.allPossible
					),
					dirty = player.dirty + order
				))),
				common = Some(g.common.copy(
					thoughts = g.common.thoughts :+ Thought(-1, -1, order, g.common.allPossible),
					dirty = g.common.dirty + order
				)),
				meta = Some(g.meta :+ ConvData(order))
			))
		}

	def onPlay(action: PlayAction)(using ops: GameOps[G]): G =
		val PlayAction(playerIndex, order, suitIndex, rank) = action
		val id = Identity(suitIndex, rank)

		game.withState { s =>
			s.copy(
				hands = s.hands.updated(playerIndex, s.hands(playerIndex).filter(_ != order)),
				endgameTurns = s.endgameTurns.map(_ - 1)
			)
		}
		.when(_ => suitIndex != -1 && rank != -1) { g =>
			g.withState(_.withPlay(id))
			.withId(order, id)
			.withThought(order)(_.copy(
				suitIndex,
				rank,
				inferred = IdentitySet.single(id),
				possible = IdentitySet.single(id)
			))
		}

	def elim(goodTouch: Boolean = true)(using ops: GameOps[G]): G =
		val state = game.state
		var newThoughts = game.common.thoughts
		var newMeta = game.meta

		for (order <- state.hands.flatten) {
			var thought = game.common.thoughts(order)
			if (thought.inferred.isEmpty && !thought.reset)
				newThoughts = newThoughts.updated(order, thought.resetInferences())
				newMeta = newMeta.updated(order, newMeta(order).copy(
					status = CardStatus.None,
					by = None
				))

			thought = newThoughts(order)

			if (thought.infoLock.exists(_.isEmpty))
				Log.warn(s"lost info lock on $order!")
				newThoughts = newThoughts.updated(order, thought.copy(infoLock = None))
		}

		var (resets, newCommon) = game.common.copy(thoughts = newThoughts).cardElim(state)
		if (goodTouch)
			val (resets2, newCommon2) = newCommon.goodTouchElim(game)
			resets ++= resets2
			newCommon = newCommon2

		val (sarcastics, _newCommon) = newCommon.refreshLinks(game, goodTouch)
		newCommon = _newCommon.refreshPlayLinks(game).updateHypoStacks(game)

		val newPlayers = game.players.map { p =>
			p.copy(
				thoughts = p.thoughts.zipWithIndex.map {
					case (t, order) if newCommon.dirty.contains(order) =>
						val thought = newCommon.thoughts(order)
						t.copy(
							possible = thought.possible,
							inferred = thought.inferred,
							infoLock = thought.infoLock,
							reset = thought.reset
					)
					case (t, _) => t
				},
				links = newCommon.links,
				playLinks = newCommon.playLinks,
				dirty = newCommon.dirty
			).cardElim(state)._2
			.when(_ => goodTouch)
				(_.goodTouchElim(game)._2)
			.refreshLinks(game, goodTouch)._2
			.refreshPlayLinks(game)
			.updateHypoStacks(game)
			.copy(dirty = BitSet.empty)
		}

		for (order <- resets) {
			val entry = newMeta(order)
			if (entry.status == CardStatus.CalledToPlay)
				newMeta = newMeta.updated(order, entry.copy(status = CardStatus.None, by = None))
		}

		for (order <- sarcastics) {
			newMeta = newMeta.updated(order, newMeta(order).copy(status = CardStatus.Sarcastic))
		}

		ops.copyWith(game, GameUpdates(
			common = Some(newCommon.copy(dirty = BitSet.empty)),
			meta = Some(newMeta),
			players = Some(newPlayers)
		))
