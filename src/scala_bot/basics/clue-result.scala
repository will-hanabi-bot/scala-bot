package scala_bot.basics

def elimResult(prev: Game, game: Game, hand: IndexedSeq[Int], list: Seq[Int]) =
	val state = game.state
	val initial = (List[Int](), List[Int](), List[Int]())

	hand.foldRight(initial) { case (order, (newTouched, fill, elim)) =>
		val prevThought = prev.common.thoughts(order)
		val thought = game.common.thoughts(order)
		val card = state.deck(order)
		val status = game.meta(order).status

		if (card.clued && status != CardStatus.CalledToDiscard && thought.possible.length < prevThought.possible.length)
			if (!prev.state.deck(order).clued && !prev.isBlindPlaying(order) && !game.common.orderKt(game, order))
				(order +: newTouched, fill, elim)
			else if (list.contains(order) && state.hasConsistentInfs(thought) && status != CardStatus.CalledToPlay)
				(newTouched, order +: fill, elim)
			else if (state.hasConsistentInfs(thought))
				(newTouched, fill, order +: elim)
			else
				(newTouched, fill, elim)
		else
			(newTouched, fill, elim)
	}

def badTouchResult(prev: Game, game: Game, giver: Int, target: Int) =
	val state = game.state

	val dupeScores = prev.players.zipWithIndex.map { (player, i) =>
		if (i == target) 99 else
			state.hands(target).map { order =>
				val card = state.deck(order)

				// Not newly clued, trash id or we don't know: don't care about duplicating
				if (prev.state.deck(order).clued || !card.clued || card.id().forall(state.isBasicTrash))
					0
				else
					state.hands(i).count{ o =>
						val thought = player.thoughts(o)
						state.deck(o).clued && thought.inferred.length > 1 && thought.inferred.contains(card.id().get)
					}
			}.sum
	}

	val avoidableDupe = dupeScores(giver) - dupeScores.min

	val inter = state.hands(target).foldRight((List[Int](), List[Int]())) { case (order, (badTouch, trash)) =>
		if (prev.state.deck(order).clued || !state.deck(order).clued)
			(badTouch, trash)
		else if (game.common.orderKt(game, order))
			(badTouch, order +: trash)
		else
			state.deck(order).id() match {
				case Some(id) if state.isBasicTrash(id) =>
					(order +: badTouch, trash)
				case _ => (badTouch, trash)
			}
	}

	// Previously-finessed cards can be reset (and no longer touched) after the clue, so double-check for "duplicates".
	val (badTouch, trash) = state.hands(target).foldRight(inter) { case (order, (badTouch, trash)) =>
		lazy val duplicated =
			(!prev.state.deck(order).clued && state.deck(order).clued && !badTouch.contains(order) && !trash.contains(order)) &&
			state.hands.zipWithIndex.exists { (hand, i) =>
				hand.exists { o =>
					(prev.isTouched(o) || game.isTouched(o)) &&
					game.me.thoughts(o).matches(state.deck(order), infer = true) &&
					(i != target || o < order)
				}
			}

		if (duplicated)
			(order +: badTouch, trash)
		else
			(badTouch, trash)
	}

	(badTouch, trash, avoidableDupe)

def playablesResult(prev: Game, game: Game) =
	game.me.hypoPlays.foldRight((List[Int](), List[Int]())) { case (order, (blindPlays, playables)) =>
		if (prev.me.hypoPlays.contains(order))
			(blindPlays, playables)
		else if (game.isBlindPlaying(order) && !prev.isBlindPlaying(order))
			(order +: blindPlays, order +: playables)
		else
			(blindPlays, order +: playables)
	}
