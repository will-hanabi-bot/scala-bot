package scala_bot.basics

import scala_bot.utils._

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

	val inter = state.hands(target).foldRight((List[Int](), List[Int]())) { case (order, (badTouch, trash)) =>
		if prev.state.deck(order).clued || !state.deck(order).clued then
			(badTouch, trash)
		else if game.common.orderKt(game, order) then
			(badTouch, order +: trash)
		else
			state.deck(order).id() match
				case Some(id) if state.isBasicTrash(id) =>
					(order +: badTouch, trash)
				case _ => (badTouch, trash)
	}

	// Previously-finessed cards can be reset (and no longer touched) after the clue, so double-check for "duplicates".
	val (badTouch, trash) = state.hands(target).foldRight(inter) { case (order, (badTouch, trash)) =>
		lazy val duplicated =
			(!prev.state.deck(order).clued && state.deck(order).clued && !badTouch.contains(order) && !trash.contains(order)) &&
			state.hands.zipWithIndex.exists: (hand, i) =>
				hand.exists: o =>
					(prev.isTouched(o) || game.isTouched(o)) &&
					game.me.thoughts(o).matches(state.deck(order), infer = true) &&
					(i != target || o < order)

		if duplicated then
			(order +: badTouch, trash)
		else
			(badTouch, trash)
	}

	(badTouch, trash, avoidableDupe)

def playablesResult(prev: Game, game: Game) =
	game.me.hypoPlays.foldRight((List[Int](), List[Int]())) { case (order, (blindPlays, playables)) =>
		if prev.me.hypoPlays.contains(order) then
			(blindPlays, playables)
		else if game.isBlindPlaying(order) && !prev.isBlindPlaying(order) then
			(order +: blindPlays, order +: playables)
		else
			(blindPlays, order +: playables)
	}
