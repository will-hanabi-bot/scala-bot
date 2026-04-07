package scala_bot.basics

import scala.collection.immutable.BitSet
import scala_bot.utils._
import scala_bot.logger.Log

extension[G <: Game](game: G)
	/** Returns the updated game after the clue, without conventional interpretations or elim. */
	def onClue(action: ClueAction)(using ops: GameOps[G]): G =
		val state = game.state
		val ClueAction(giver, target, list, clue) = action
		val BaseClue(kind, value) = clue
		val newPossible = IdentitySet.from(state.variant.touchPossibilities(clue))

		state.hands(target).foldLeft(game): (newGame, order) =>
			if list.contains(order) then
				val touchedGame = newGame.withCard(order)(c => c.copy(
						clued = true,
						clues = c.clues :+ CardClue(kind, value, giver, state.turnCount)
					))
					.withThought(order)(t => t.copy(
						inferred = t.inferred.intersect(newPossible),
						possible = t.possible.intersect(newPossible),
						infoLock = t.infoLock.mapO(_.intersect(newPossible))
					))

				val newThought = touchedGame.common.thoughts(order)

				touchedGame
					.when(_ => newThought.possible.length == 1): g =>
						val id = newThought.possible.head
						g.withCard(order)(c => c.copy(suitIndex = id.suitIndex, rank = id.rank))
							.pipe(ops.copyWith(_, GameUpdates(deckIds = Some(g.deckIds.updated(order, Some(id))))))

					.when(_ => newThought.inferred.length < game.common.thoughts(order).inferred.length):
						_.withMeta(order)(_.reason(state.turnCount))
			else
				newGame.withThought(order)(t => t.copy(
					inferred = t.inferred.difference(newPossible),
					possible = t.possible.difference(newPossible),
					infoLock = t.infoLock.mapO(_.difference(newPossible))
				))
		.withState: s =>
			s.copy(endgameTurns = s.endgameTurns.map(_ - 1), clueTokens = s.clueTokens - 1)

	/** Returns the updated game after the discard, without conventional interpretations or elim. */
	def onDiscard(action: DiscardAction)(using ops: GameOps[G]): G =
		val DiscardAction(playerIndex, order, suitIndex, rank, failed) = action
		val id = Identity(suitIndex, rank)

		game.withState: s =>
			s.copy(
				hands = s.hands.updated(playerIndex, s.hands(playerIndex).filter(_ != order)),
				endgameTurns = s.endgameTurns.map(_ - 1)
			)
			.cond(_ => failed)
				(_.copy(strikes = s.strikes + 1))
				(_.regainClue)

		.when(_ => suitIndex != -1 && rank != -1): g =>
			g.withState(_.withDiscard(id, order))
			.withId(order, id)
			.withThought(order): t =>
				t.copy(
					suitIndex,
					rank,
					inferred = IdentitySet.single(id),
					oldInferred = t.inferred.toOpt,
					possible = IdentitySet.single(id),
					oldPossible = t.possible.toOpt,
				)

	/** Returns the updated game after the draw, without conventional interpretations or elim.
	  * @throws Exception If the draw reveals an inconsistent state, like the card order not matching the # of cards in the deck.
	  */
	def onDraw(action: DrawAction)(using ops: GameOps[G]): G =
		val state = game.state
		val DrawAction(playerIndex, order, suitIndex, rank) = action

		if state.hands(playerIndex).length == HAND_SIZE(state.numPlayers) && !(state.options.deckPlays && order == state.cardCount.sum - 1) then
			throw new Exception(s"${state.names(playerIndex)} already has a full hand!")

		val id = Option.when(suitIndex != -1 && rank != -1):
			game.deckIds.lift(order).flatten.foreach: id =>
				if id != Identity(suitIndex, rank) then
					throw new Exception(s"drew ${state.logId(Identity(suitIndex, rank))}, expected ${state.logId(id)} ${game.deckIds.map(state.logId)} ${order}")

			Identity(suitIndex, rank)

		game.pipe: g =>
			val deckIds = g.deckIds

			if deckIds.length == order then
				ops.copyWith(g, GameUpdates(deckIds = Some(deckIds :+ id)))
			else if deckIds.length > order then
				if deckIds(order).isDefined then g else
					ops.copyWith(g, GameUpdates(deckIds = Some(deckIds.updated(order, id))))
			else
				throw new IllegalArgumentException(s"Only have ${deckIds.length} deck ids, but drew card with order $order! ${g.state.hands}")

		.tap: g =>
			if !state.options.deckPlays then
				assert(g.state.deck.length == order, s"Deck length ${g.state.deck.length} doesn't match drawn order $order!s")
				assert(g.state.deck.length == g.state.nextCardOrder, "deck length doesn't match next order")
			assert(g.common.thoughts.length == g.players(0).thoughts.length, "common thoughts length differs from player 0's thoughts")
			assert(g.common.thoughts.length == g.meta.length, "common thoughts length differs from meta length")

		.withState: s =>
			s.copy(
				hands = s.hands.updated(playerIndex, order +: s.hands(playerIndex)),
				deck = s.deck :+ Card(suitIndex, rank, order, s.turnCount),
				holders = s.holders :+ playerIndex,
				nextCardOrder = order + 1,
				cardsLeft = s.cardsLeft - 1
			)
			.when(s => s.cardsLeft == 0 && s.endgameTurns.isEmpty):
				_.copy(endgameTurns = Some(s.numPlayers))

		.when(order == _.meta.length): g =>
			ops.copyWith(g, GameUpdates(
				players = Some(g.players.zipWithIndex.map((player, i) => player.copy(
					thoughts = player.thoughts :+ Thought(
						if i != playerIndex then suitIndex else -1,
						if i != playerIndex then rank else -1,
						order,
						player.allPossible
					),
					dirty = player.dirty.incl(order)
				))),
				common = Some(g.common.copy(
					thoughts = g.common.thoughts :+ Thought(-1, -1, order, g.common.allPossible),
					dirty = g.common.dirty + order
				)),
				meta = Some(g.meta :+ ConvData(order))
			))

	/** Returns the updated game after the play, without conventional interpretations or elim. */
	def onPlay(action: PlayAction)(using ops: GameOps[G]): G =
		val PlayAction(playerIndex, order, suitIndex, rank) = action
		val id = Identity(suitIndex, rank)

		game.withState: s =>
			s.copy(
				hands = s.hands.updated(playerIndex, s.hands(playerIndex).filter(_ != order)),
				endgameTurns = s.endgameTurns.map(_ - 1)
			)
		.when(_ => game.state.options.deckPlays && order == game.state.cardCount.sum - 1 && game.state.deck.length == order): g =>
			g.withState: s =>
				s.copy(
					deck = s.deck :+ Card(suitIndex, rank, order, s.turnCount),
					holders = s.holders :+ playerIndex,
					nextCardOrder = s.nextCardOrder + 1,
					cardsLeft = s.cardsLeft - 1,
					endgameTurns = Some(s.numPlayers)
				)
			.pipe(ops.copyWith(_, GameUpdates(
				players = Some(g.players.zipWithIndex.map((player, i) => player.copy(
					thoughts = player.thoughts :+ Thought(
						if i != playerIndex then suitIndex else -1,
						if i != playerIndex then rank else -1,
						order,
						player.allPossible
					),
					dirty = player.dirty.incl(order)
				))),
				common = Some(g.common.copy(
					thoughts = g.common.thoughts :+ Thought(-1, -1, order, g.common.allPossible),
					dirty = g.common.dirty + order
				)),
				meta = Some(g.meta :+ ConvData(order))
			)))
		.when(_ => suitIndex != -1 && rank != -1):
			_.withState(_.withPlay(id))
			.withId(order, id)
			.withThought(order): t =>
				t.copy(
					suitIndex,
					rank,
					inferred = IdentitySet.single(id),
					oldInferred = t.inferred.toOpt,
					possible = IdentitySet.single(id),
					oldPossible = t.possible.toOpt
				)

	/** Returns the updated game after performing empathy operations, including good touch if the convention uses it.
	  * Also clears the dirty bit on all players.
	  * @param except The index of the player to ignore during good touch elim (e.g. if they are giving a clue).
	  */
	def elim(except: Option[Int] = None)(using ops: GameOps[G]): G =
		val state = game.state

		game.pipe:
			state.hands.flatten.foldLeft(_): (g, order) =>
				val thought = g.common.thoughts(order)

				g.when(_ => thought.inferred.isEmpty && !thought.reset):
					_.withThought(order)(_.resetInferences())
					.withMeta(order):
						_.copy(status = CardStatus.None, by = None)
				.when(_.common.thoughts(order).infoLock.existsO(_.isEmpty)):
					Log.warn(s"lost info lock on $order!")
					_.withThought(order)(_.copy(infoLock = IdentitySetOpt.empty))

		.pipe: g =>
			val (resets, newCommon) = g.common.cardElim(state)
				.when(_ => g.goodTouch): (r, c) =>
					val (resets, newCommon) = c.goodTouchElim(g, except)
					(resets.union(r), newCommon)

			resets.foldLeft(ops.copyWith(g, GameUpdates(common = Some(newCommon)))): (acc, order) =>
				val entry = acc.meta(order)
				if entry.status != CardStatus.CalledToPlay then acc else
					acc.withMeta(order):
						_.copy(status = CardStatus.None, by = None)

		.pipe: g =>
			val (sarcastics, newCommon) = g.common.refreshLinks(g)
			val newGame = ops.copyWith(g, GameUpdates(
				common = Some(newCommon.refreshPlayLinks(g).updateHypoStacks(g))
			))

			sarcastics.foldLeft(newGame): (acc, order) =>
				acc.withMeta(order)(_.copy(status = CardStatus.Sarcastic))

		.pipe: g =>
			val newPlayers = g.players.map: p =>
				p.copy(
					thoughts = p.thoughts.map: t =>
						if !g.common.dirty.contains(t.order) then t else
							val thought = g.common.thoughts(t.order)
							val newInferred =
								thought.inferred.intersect(t.possible).whenEmpty(t.possible)

							val newInfoLock =
								if !thought.infoLock.isDefined then
									thought.infoLock
								else
									val ids = thought.infoLock.get.intersect(t.possible)
									if ids.isEmpty then
										IdentitySetOpt.empty
									else
										ids.toOpt
							t.copy(
								possible = thought.possible,
								inferred = newInferred,
								infoLock = newInfoLock,
								reset = thought.reset
							),
					links = g.common.links,
					playLinks = g.common.playLinks,
					dirty = g.common.dirty
				)
				.cardElim(state)._2
				.when(_ => g.goodTouch):
					_.goodTouchElim(g, except)._2
				.refreshLinks(g)._2
				.refreshPlayLinks(g)
				.updateHypoStacks(g)
				.copy(dirty = BitSet.empty)

			ops.copyWith(g, GameUpdates(
				common = Some(g.common.copy(dirty = BitSet.empty)),
				players = Some(newPlayers)
			))
