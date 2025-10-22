package scala_bot.basics

def checkFix(prev: Game, game: Game, action: ClueAction) =
	val list = action.list

	list.foldLeft((List[Int](), List[Int]())) { case ((cluedResets, duplicateReveals), order) =>
		lazy val duplicated = prev.state.deck(order).clued && list.exists { o =>
			o != order &&
			prev.state.deck(o).clued &&
			game.state.deck(order).matches(game.state.deck(o)) &&
			!prev.common.thoughts(order).matches(prev.common.thoughts(o), infer = true)
		}

		if (!prev.common.thoughts(order).reset && game.common.thoughts(order).reset)
			(order +: cluedResets, duplicateReveals)
		else if (duplicated)
			(cluedResets, order +: duplicateReveals)
		else
			(cluedResets, duplicateReveals)
	}

def connectableSimple[G <: Game](game: G, player: Player, start: Int, target: Int, id: Option[Identity] = None)(using ops: GameOps[G]): List[Int] =
	val state = game.state

	if (id.exists(state.isPlayable))
		List(99)
	else if (start == target)
		player.obviousPlayables(game, target).toList
	else if (game.state.ended)
		List()
	else
		val nextPlayerIndex = state.nextPlayerIndex(start)
		val playables = player.obviousPlayables(game, start)

		val connectables = playables.view.map { order =>
			player.thoughts(order).id(infer = true).map { playId =>
				val newGame = game
					.simulateAction(TurnAction(state.turnCount, start))	// Go to starting player's turn
					.simulateAction(PlayAction(start, order, playId.suitIndex, playId.rank))
					.simulateAction(TurnAction(state.turnCount + 1, nextPlayerIndex))

				connectableSimple(newGame, player, nextPlayerIndex, target, id)
			}
		}.flatten.find(_.nonEmpty)

		connectables.getOrElse(connectableSimple(game, player, nextPlayerIndex, target, id))

/** Returns the possible ids of a distribution clue. */
def distributionClue(prev: Game, game: Game, action: ClueAction, focus: Int): Option[IdentitySet] =
	val state = game.state
	val ClueAction(_, target, list, clue) = action
	val thought = game.common.thoughts(focus)

	if (list.forall(prev.state.deck(_).clued) || (!state.inEndgame && state.remScore > state.variant.suits.length))
		return None

	if (thought.id(infer = true).exists(state.isBasicTrash))
		return None

	val poss = if (clue.kind == ClueKind.Colour)
		thought.possible.toList
	else
		thought.possible.filter(_.rank == clue.value).toList

	def loop(poss: List[Identity], useful: IdentitySet): Option[IdentitySet] =
		if (poss.isEmpty) Some(useful) else
			val p = poss.head
			lazy val duplicated = state.hands.zipWithIndex.exists { (hand, i) =>
				i != target && hand.exists(o => game.isTouched(o) && game.orderMatches(o, p, infer = true))
			}

			if (state.isBasicTrash(p))
				loop(poss.tail, useful)
			else if (duplicated)
				loop(poss.tail, useful.union(p))
			else
				None

	val useful = loop(poss, IdentitySet.empty)
	useful.filter(_.nempty)
