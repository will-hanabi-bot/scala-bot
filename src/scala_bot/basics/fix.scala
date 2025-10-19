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

def connectableSimple(game: Game, player: Player, start: Int, target: Int, id: Option[Identity] = None): List[Int] =
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
