package scala_bot.reactor

import scala_bot.basics._
import scala_bot.basics.given_Conversion_IdentitySet_Iterable
import scala_bot.logger.Log
import scala_bot.utils._

private def reactiveContext(prev: Game, game: Game, action: ClueAction, reacter: Int) =
	val ClueAction(giver = giver, target = receiver, clue = _, list = _) = action
	val state = game.state

	val possibleConns = delayedPlays(game, giver, receiver)

	val oldPlayables = prev.common.obviousPlayables(prev, receiver)
	val newPlayables = game.common.obviousPlayables(game, receiver)
	val knownPlays = oldPlayables.filter(newPlayables.contains)

	// Update play stacks to the reacter's turn
	val afterOthers = playersUntil(state.numPlayers, state.nextPlayerIndex(giver), reacter)
		.foldLeft(prev.state) { (s, i) =>
			val hypoPrev = prev.copy(state = s)
			val playables = {
				val ps = prev.common.obviousPlayables(hypoPrev, i)
				ps.find(prev.meta(_).urgent).map(Vector(_)).getOrElse(ps)
			}

			playables.headOption.flatMap(state.deck(_).id()) match {
				case Some(id) => s.tryPlay(id)
				case None => s
			}
		}

	// The receiver can also play their own known playables first
	val hypoState = prev.common.obviousPlayables(prev.copy(state = afterOthers), receiver)
		.foldLeft(afterOthers) { (s, selfPlay) =>
			state.deck(selfPlay).id().map(s.tryPlay).getOrElse(s)
		}

	(possibleConns, knownPlays, hypoState)

def interpretReactiveColour(prev: Game, game: Game, action: ClueAction, focusSlot: Int, reacter: Int, inverted: Boolean): (Option[ClueInterp], Game) =
	val ClueAction(giver = giver, target = receiver, clue = _, list = _) = action
	val state = game.state
	val (possibleConns, knownPlays, hypoState) = reactiveContext(prev, game, action, reacter)

	val playTargets = state.hands(receiver).zipWithIndex
		.filter { (o, _) =>
			game.meta(o).status != CardStatus.CalledToDiscard &&
			!knownPlays.contains(o) &&
			state.deck(o).id().exists(hypoState.isPlayable)
		}.sortBy { (o, i) =>
			// Unclued dupe, with a clued dupe
			val uncluedDupe = !prev.state.deck(o).clued && state.hands(receiver).exists { o2 =>
					o2 < o &&
					prev.state.deck(o2).clued &&
					state.deck(o).matches(state.deck(o2))
				}
			if (uncluedDupe) 99 else i
		}

	// Try targeting all play targets
	playTargets.view.flatMap { case (_, index) =>
		val targetSlot = index + 1
		val reactSlot = calcSlot(focusSlot, targetSlot)
		lazy val prevTrash = prev.common.thinksTrash(prev, reacter)

		state.hands(reacter).lift(reactSlot - 1) match {
			case None =>
				Log.warn(s"Reacter doesn't have slot $reactSlot!")
				None
			case Some(reactOrder) if prevTrash.contains(reactOrder) || (inverted && prevTrash.isEmpty && reactSlot == 1) =>
				Log.warn(s"attempted dc+play would result in reacter naturally discarding ${state.logId(reactOrder)} $reactOrder!")
				None
			case Some(reactOrder) if game.common.thoughts(reactOrder).possible.forall(state.isCritical) =>
				Log.warn(s"attempted dc+play would result in reacter discarding known critical ${state.logId(reactOrder)} $reactOrder!")
				None
			case Some(reactOrder) =>
				val newCommon = game.common.withThought(reactOrder) { t =>
					t.copy(oldInferred = Some(t.inferred))
				}
				val newGame = targetDiscard(game.copy(common = newCommon), action, reactOrder, urgent = true)
				Log.info(s"reactive dc+play, reacter ${state.names(reacter)} (slot $reactSlot) receiver ${state.names(receiver)} (slot $targetSlot), focus slot $focusSlot")
				Some((Some(ClueInterp.Reactive), newGame))
		}
	}.headOption
	.getOrElse {
		// Didn't work, so target trash
		val prevKT = prev.players(giver).thinksTrash(prev, receiver)
		val dcTargets = {
			val unknownTrash = state.hands(receiver).zipWithIndex.filter { (o, _) =>
				!prevKT.contains(o) &&
				(state.isBasicTrash(state.deck(o).id().get) ||
					state.hands(receiver).exists(o2 => o2 != o && state.deck(o).matches(state.deck(o2)))) // duped in the same hand
			}.sortBy { (o, _) =>
				if (prev.state.deck(o).clued)
					0
				else if (state.hands(receiver).exists(o2 => o2 < o && prev.state.deck(o2).clued && state.deck(o).matches(state.deck(o2))))
					-1		// Unclued dupe, with a clued dupe
				else
					1
			}

			lazy val knownTrash = state.hands(receiver).zipWithIndex.filter { (o, _) =>
				state.isBasicTrash(state.deck(o).id().get)
			}

			lazy val sacrifices = state.hands(receiver).zipWithIndex.filter { (o, _) =>
				!prevKT.contains(o) && !state.isCritical(state.deck(o).id().get)
			}.sortBy { (o, _) =>
				val id = state.deck(o).id().get
				-game.common.playableAway(id) * 10 + (5 - id.rank)
			}

			if (unknownTrash.isEmpty)
				if (knownTrash.isEmpty) sacrifices else knownTrash
			else
				unknownTrash
		}

		if (dcTargets.isEmpty)
			Log.warn(s"reactive clue but receiver had no playable, trash or sacrifice targets!")
			(None, game)
		else
			dcTargets.view.flatMap { (target, index) =>
				if (state.nextPlayerIndex(giver) != reacter && game.meta(target).status == CardStatus.CalledToPlay)
					Log.warn("can't target previously-playable trash with a reverse reactive clue!")
					None
				else
					val targetSlot = index + 1
					val reactSlot = calcSlot(focusSlot, targetSlot)
					lazy val prevPlays = prev.common.obviousPlayables(prev, reacter)

					state.hands(reacter).lift(reactSlot - 1) match {
						case None =>
							Log.warn(s"Reacter doesn't have slot $reactSlot!")
							None
						case Some(reactOrder) if prevPlays.contains(reactOrder) =>
							Log.warn(s"attempted play+dc would result in reacter naturally playing ${state.logId(reactOrder)} $reactOrder!")
							None
						case Some(reactOrder) if !game.common.thoughts(reactOrder).possible.exists(i => state.isPlayable(i) || possibleConns.exists(_._2 == i)) =>
							Log.warn(s"reaction would involve playing unplayable ${state.logId(reactOrder)} $reactOrder!")
							None
						case Some(reactOrder) =>
							val newCommon = game.common.withThought(reactOrder) { t =>
								t.copy(oldInferred = Some(t.inferred))
							}
							val (interp, newGame) = targetPlay(game.copy(common = newCommon), action, reactOrder, urgent = true, stable = false)
							interp match {
								case None => Some(None, newGame)
								case Some(_) =>
									Log.info(s"reactive play+dc, reacter ${state.names(reacter)} (slot ${reactSlot}) receiver ${state.names(receiver)} (slot ${targetSlot}), focus slot ${focusSlot}")
									Some(Some(ClueInterp.Reactive), newGame)
							}
				}
			}.headOption
			.getOrElse(None, game)
	}

def interpretReactiveRank(prev: Game, game: Game, action: ClueAction, focusSlot: Int, reacter: Int): (Option[ClueInterp], Game) =
	val ClueAction(giver = _, target = receiver, clue = _, list = _) = action
	val state = game.state
	val (possibleConns, knownPlays, hypoState) = reactiveContext(prev, game, action, reacter)

	val playTargets = state.hands(receiver).zipWithIndex.filter { (o, _) =>
		game.meta(o).status != CardStatus.CalledToDiscard &&
		!knownPlays.contains(o) &&
		state.deck(o).id().exists(hypoState.isPlayable)
	}.sortBy { (o, i) =>
		// Do not target an unclued copy when there is a clued copy
		val uncluedDupe = !prev.state.deck{9}.clued &&
			state.hands(receiver).exists{ o2 =>
				o2 != o && prev.state.deck(o2).clued && state.deck(o).matches(state.deck(o2))
			}
		if (uncluedDupe) 99 else i
	}

	playTargets.view.flatMap { (target, index) =>
		val targetSlot = index + 1
		val reactSlot = calcSlot(focusSlot, targetSlot)
		val receiveOrder = target
		lazy val prevPlays = prev.common.obviousPlayables(prev, reacter)

		state.hands(reacter).lift(reactSlot - 1) match {
			case None =>
				Log.warn(s"Reacter doesn't have slot $reactSlot!")
				None
			case Some(reactOrder) if prevPlays.contains(reactOrder) =>
				Log.warn(s"attempted play+play would result in reacter naturally playing ${state.logId(reactOrder)} $reactOrder!")
				None
			case Some(reactOrder) if !game.common.thoughts(reactOrder).possible.exists(i => state.isPlayable(i) || possibleConns.exists(_._2 == i)) =>
				Log.warn(s"reaction would involve playing unplayable ${state.logId(reactOrder)} $reactOrder!")
				None
			case Some(reactOrder) =>
				val (interp, newGame) = targetPlay(game, action, reactOrder, urgent = true, stable = false)
				val nextGame = newGame.withThought(reactOrder) { t =>
					t.copy(inferred = t.inferred.difference(state.deck(receiveOrder).id().get))
				}
				interp match {
					case None => Some(None, nextGame)
					case Some(_) =>
						Log.info(s"reactive play+play, reacter ${state.names(reacter)} (slot ${reactSlot}) receiver ${state.names(receiver)} (slot ${targetSlot}), focus slot ${focusSlot}")
						Some((Some(ClueInterp.Reactive), nextGame))
				}
		}
	}.headOption
	.getOrElse {
		val finesseTargets = state.hands(receiver).zipWithIndex.filter { (order, _) =>
			state.playableAway(state.deck(order).id().get) == 1
		}

		if (finesseTargets.isEmpty)
			Log.warn("reactive clue but receiver had no playable targets!")
			(None, game)
		else
			(for {
				reactSlot <- List(1, 5, 4, 3, 2).view
				targetSlot = calcSlot(focusSlot, reactSlot)
				reactOrder <- state.hands(reacter).lift(reactSlot - 1)
				(receiveOrder, _) <- finesseTargets.find((_, i) => i + 1 == targetSlot)
			} yield {
				val prevPlays = prev.common.obviousPlayables(prev, reacter)

				lazy val unplayable = !game.common.thoughts(reactOrder).possible.exists(i => state.isPlayable(i) || possibleConns.exists(_._2 == i)) ||
					!game.common.thoughts(reactOrder).possible.contains(state.deck(receiveOrder).id().get.prev)

				if (prevPlays.contains(reactOrder))
					Log.warn(s"attempted finesse would result in reacter naturally playing ${state.logId(reactOrder)} $reactOrder!")
					None
				else if (unplayable)
					Log.warn(s"reaction would involve playing unplayable ${state.logId(reactOrder)} $reactOrder!")
					None
				else
					val newCommon = game.common.withThought(reactOrder) { t =>
						t.copy(oldInferred = Some(t.inferred))
					}
					val (interp, newGame) = targetPlay(game.copy(common = newCommon), action, reactOrder, urgent = true, stable = false)
					interp match {
						case None => Some(None, newGame)
						case Some(_) =>
							val newGame2 = newGame.withThought(reactOrder) { t =>
								t.copy(inferred = IdentitySet.single(state.deck(receiveOrder).id().get.prev))
							}

							Log.info(s"reactive finesse, reacter ${state.names(reacter)} (slot ${reactSlot}) receiver ${state.names(receiver)} (slot ${targetSlot}), focus slot ${focusSlot}")
							Some((Some(ClueInterp.Reactive), newGame2))
					}
			}).flatten.headOption
			.getOrElse((None, game))
	}
