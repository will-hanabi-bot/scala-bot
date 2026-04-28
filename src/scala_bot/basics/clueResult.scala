package scala_bot.basics

import scala_bot.utils._

/** Computes empathy-related statistics of the clue.
  * @return A tuple of the orders that were newly touched, filled-in, and had inferences removed from negative info.
  */
def elimResult(prev: Game, game: Game, hand: IndexedSeq[Int], list: Seq[Int]) =
	val state = game.state
	val initial = (List[Int](), List[Int](), List[Int]())

	hand.foldRight(initial) { case (order, (newTouched, fill, elim)) =>
		val prevThought = prev.common.thoughts(order)
		val thought = game.common.thoughts(order)
		val card = state.deck(order)
		val status = game.meta(order).status

		if card.clued && status != CardStatus.CalledToDiscard && thought.possible.length < prevThought.possible.length then
			if game.common.orderKt(game, order) || card.id().exists(state.isBasicTrash) then
				(newTouched, fill, elim)

			else if !prev.state.deck(order).clued && !prev.isBlindPlaying(order) then
				(order +: newTouched, fill, elim)

			else if list.contains(order) && state.hasConsistentInfs(thought) && status != CardStatus.CalledToPlay then
				(newTouched, order +: fill, elim)

			else if state.hasConsistentInfs(thought) then
				(newTouched, fill, order +: elim)

			else
				(newTouched, fill, elim)
		else
			(newTouched, fill, elim)
	}

/** Returns the player indices who are respnsible for saving a particular id.
  * @param game   The current game state.
  * @param id     The identity to save.
  * @param except The player holding the id, who cannot save themselves (obviously).
  */
def dupeResponsibility(game: Game, id: Identity, except: Int) =
	val state = game.state

	def potentialDupes(playerIndex: Int) =
		state.hands(playerIndex).count: o =>
			state.deck(o).clued &&
			game.common.thoughts(o).inferred.contains(id)

	val dupes = (0 until state.numPlayers).filter(_ != except).map(potentialDupes)
	val minDupe = dupes.min

	dupes.zipWithIndex.collect:
		case (ds, i) if ds == minDupe => i

/** Computes bad-touch-related statistics of the clue.
  * @return A tuple of the orders that were bad touched, known trash, and dupes that could have been clued by someone else.
  */
def badTouchResult(prev: Game, game: Game, action: ClueAction) =
	val state = game.state
	val ClueAction(giver, target, _, _) = action

	val dupeScores = prev.players.zipWithIndex.map: (player, i) =>
		if i == target then 99 else
			state.hands(target).summing: order =>
				val card = state.deck(order)

				// Not newly clued, trash id or we don't know: don't care about duplicating
				if prev.state.deck(order).clued || !card.clued || card.id().forall(state.isBasicTrash) then
					0
				else
					state.hands(i).count: o =>
						val thought = player.thoughts(o)
						state.deck(o).clued && thought.inferred.length > 1 && thought.inferred.contains(card.id().get)

	val avoidableDupe = dupeScores(giver) - dupeScores.min

	def areDupes(o1: Int, o2: Int, badTouch: List[Int], trash: List[Int]) =
		o1 != o2 &&
		state.deck(o1).clued && state.deck(o2).clued &&
		game.me.thoughts(o1).matches(state.deck(o2)) &&
		!badTouch.contains(o2) &&
		!trash.contains(o2) &&
		(game.common.thoughts(o1).id().isEmpty || game.common.thoughts(o2).id().isEmpty)

	val inter = state.hands(target).foldRight((List[Int](), List[Int]())) { case (order, (badTouch, trash)) =>
		if prev.state.deck(order).clued || !state.deck(order).clued then
			(badTouch, trash)
		else if game.common.orderTrash(game, order) then
			(badTouch, order +: trash)
		else
			state.deck(order).id() match
				case Some(id) if state.isBasicTrash(id) || state.hands(target).exists(areDupes(order, _, badTouch, trash)) =>
					(order +: badTouch, trash)
				case _ => (badTouch, trash)
	}

	// Previously-finessed cards can be reset (and no longer touched) after the clue, so double-check for "duplicates".
	val (badTouch, trash) = state.hands(target).foldRight(inter) { case (order, (badTouch, trash)) =>
		val duplicated =
			(!prev.state.deck(order).clued && state.deck(order).clued && !badTouch.contains(order) && !trash.contains(order)) &&
			state.hands.exists: hand =>
				hand.exists: o =>
					(prev.isTouched(o) || game.isTouched(o)) &&
					areDupes(order, o, badTouch, trash)

		if duplicated then
			(order +: badTouch, trash)
		else
			(badTouch, trash)
	}

	(badTouch, trash, avoidableDupe)

/** Computes playable-related statistics of the clue.
  * @return A tuple of the orders that are newly blind playing and newly playable.
  */
def playablesResult(prev: Game, game: Game) =
	game.me.hypoPlays.foldRight((List[Int](), List[Int]())) { case (order, (blindPlays, playables)) =>
		val badPlayable = prev.me.hypoPlays.contains(order) ||
			game.me.thoughts(order).id(infer = true).exists: id =>
				prev.me.hypoStacks(id.suitIndex) >= id.rank ||
				prev.me.hypoPlays.exists: o =>
					game.me.thoughts(o).matches(id, infer = true)

		if badPlayable then
			(blindPlays, playables)
		else if game.isBlindPlaying(order) && !prev.isBlindPlaying(order) then
			(order +: blindPlays, order +: playables)
		else
			(blindPlays, order +: playables)
	}
