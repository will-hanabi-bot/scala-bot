package scala_bot.refSieve

import scala_bot.basics._
import scala_bot.logger.{Log, Logger, LogLevel}

def getResult(game: RefSieve, hypo: RefSieve, action: ClueAction): Double =
	val (common, state, meta) = (game.common, game.state, game.meta)
	val ClueAction(giver, target, list, clue) = action

	val (newTouched, fill, elim) = elimResult(game, hypo, hypo.state.hands(target), list)
	val (badTouch, trash, _) = badTouchResult(game, hypo, giver, target)
	val (_, playables) = playablesResult(game, hypo)

	val revealedTrash = hypo.common.thinksTrash(hypo, target).count(o =>
		hypo.state.deck(o).clued && !common.thinksTrash(game, target).contains(o))

	val newPlayables = state.hands.flatten.filter(o =>
		meta(o).status != CardStatus.CalledToPlay && hypo.meta(o).status == CardStatus.CalledToPlay)

	val badPlayable = newPlayables.find(o =>
		!(hypo.me.hypoPlays.contains(o) || (state.inEndgame && state.deck(o).id().exists(state.isPlayable))))

	badPlayable match {
		case Some(badPlay) =>
			Log.warn(s"clue ${clue.fmt(state, target)} results in ${state.logId(badPlay)} ${badPlay} looking playable! ${hypo.me.hypoPlays}")
			-100
		case None =>
			val badZcs = Option.when(hypo.state.clueTokens == 0) {
				state.hands.flatten.find(o =>
					hypo.meta(o).status == CardStatus.ZeroClueChop && state.deck(o).id().exists(state.isCritical))
			}.flatten

			badZcs match {
				case Some(badZcs) =>
					Log.warn(s"clue ${clue.fmt(state, target)} results in bad zcs ${state.logId(badZcs)} $badZcs!")
					-100
				case None =>
					hypo.lastMove match {
						case Some(ClueInterp.Play) if playables.isEmpty && !state.inEndgame =>
							Log.warn(s"clue ${clue.fmt(state, target)} looks like ref play but gets no playables!")
							-100
						case Some(ClueInterp.Reveal) if playables.isEmpty && trash.nonEmpty && trash.forall(state.deck(_).clued) =>
							Log.warn(s"clue ${clue.fmt(state, target)} only reveals new trash but isn't a trash push!")
							-100
						case Some(i) if i != ClueInterp.Reactive && badTouch.nonEmpty && newTouched.forall(badTouch.contains) && playables.isEmpty =>
							Log.warn(s"clue ${clue.fmt(state, target)} only bad touches and gets no playables! ${common.hypoPlays}")
							-100
						case _ =>
							// Previously-unclued playables whose copies are already touched
							val dupedPlayables = hypo.me.hypoPlays.count { p =>
								!state.deck(p).clued &&
								state.hands.flatten.exists(o =>
									o != p && game.isTouched(o) && state.deck(o).matches(state.deck(p)))
							}

							val goodTouch: Double =
								if (badTouch.length > newTouched.length)
									-badTouch.length
								else
									List(0.0, 0.125, 0.25, 0.35, 0.45, 0.55)(newTouched.length - badTouch.length)

							val untouchedPlays = playables.count(!hypo.state.deck(_).clued)

							val playablesS = playables.map(state.logId(_)).mkString(", ")
							Log.info(s"good touch: $goodTouch, playables: $playablesS, duped: $dupedPlayables, trash: ${trash.length}, fill: ${fill.length}, elim: ${elim.length}, bad touch: $badTouch, ${hypo.lastMove}")

							val value = goodTouch +
								(playables.length - 2.0 * dupedPlayables) +
								0.2 * untouchedPlays +
								(if (state.inEndgame) 0.01 else 0.1) * revealedTrash +
								(if (state.inEndgame) 0.2 else 0.1) * fill.length +
								(if (state.inEndgame) 0.1 else 0.05) * elim.length +
								0.1 * badTouch.length

							hypo.lastMove match {
								case Some(ClueInterp.Mistake) => value - 10
								case Some(ClueInterp.Fix) | Some(ClueInterp.Reactive) => value + 1
								case _ => value
							}
					}
			}
	}

def advanceGame(game: RefSieve, action: Action) =
	action match {
		case clue: ClueAction => game.simulateClue(clue, log = true)
		case _ => game.simulateAction(action)
	}

def forceClue(orig: RefSieve, game: RefSieve, offset: Int, bobOnly: Boolean): Double =
	val state = game.state
	val playerIndex = (state.ourPlayerIndex + offset) % state.numPlayers
	val bob = state.nextPlayerIndex(playerIndex)

	val allClues =
		for
			i <- 0 until state.numPlayers if i != playerIndex && (!bobOnly || i == bob)
			clue <- state.allValidClues(i)
		yield
			val list = state.clueTouched(state.hands(i), clue)
			ClueAction(playerIndex, i, list, clue.toBase)

	val level = Logger.level
	allClues.map { action =>
		Logger.setLevel(LogLevel.Off)
		val hypoGame = advanceGame(game, action)

		if (hypoGame.lastMove == Some(ClueInterp.Mistake))
			Logger.setLevel(level)
			-100.0
		else
			val value = advance(orig, hypoGame, offset + 1)
			Logger.setLevel(level)
			Log.highlight(Console.YELLOW, s"${action.fmt(state)}: $value")
			value
	}.maxOption.getOrElse(0.0)

def advance(orig: RefSieve, game: RefSieve, offset: Int): Double =
	val (state, common, meta) = (game.state, game.common, game.meta)
	val playerIndex = (state.ourPlayerIndex + offset) % state.numPlayers
	val player = game.players(playerIndex)

	val bob = state.nextPlayerIndex(playerIndex)
	val bobChop = Option.when(state.numPlayers > 2)(RefSieve.chop(game, bob)).flatten

	lazy val trash = player.thinksTrash(game, playerIndex)
	lazy val urgentDc = trash.find(meta(_).urgent)
	lazy val allPlayables = player.thinksPlayables(game, playerIndex)

	if (playerIndex == state.ourPlayerIndex || state.endgameTurns.contains(0))
		evalGame(orig, game)

	else if (urgentDc.isEmpty && allPlayables.nonEmpty)
		val playables = allPlayables.find(meta(_).urgent) match {
			case Some(order) => List(order)
			case None => allPlayables.filter { o =>
				// Only consider playing the leftmost of similarly-possible cards
				!allPlayables.exists(p => p > o && common.thoughts(p).possible == common.thoughts(o).possible)
			}
		}

		val (knownPlays, unknownPlays) = playables.foldLeft((List.empty[Double], List.empty[Double])) { case ((known, unknown), order) =>
			val (id, action) = state.deck(order).id() match {
				case None => (None, PlayAction(playerIndex, order, -1, -1))
				case Some(id) =>
					val action = if (state.isPlayable(id))
						PlayAction(playerIndex, order, id.suitIndex, id.rank)
					else
						DiscardAction(playerIndex, order, id.suitIndex, id.rank, failed = true)
					(Some(id), action)
			}

			Log.info(s"${state.names(playerIndex)} ${if (id.exists(state.isPlayable)) "playing" else "bombing"} ${state.logId(id)}")
			val value = advance(orig, advanceGame(game, action), offset + 1)

			if (player.thoughts(order).id(infer = true).isDefined)
				(value +: known, unknown)
			else
				(known, value +: unknown)
		}
		math.min(knownPlays.maxOption.getOrElse(99.9), unknownPlays.minOption.getOrElse(99.9))

	else if (player.thinksLocked(game, playerIndex))
		if (!state.canClue)
			val lockedDc = player.lockedDiscard(state, playerIndex)
			val Identity(suitIndex, rank) = state.deck(lockedDc).id().get
			val action = DiscardAction(playerIndex, lockedDc, suitIndex, rank)
			Log.info(s"locked discard! $lockedDc")
			advance(orig, advanceGame(game, action), offset + 1)
		else
			forceClue(orig, game, offset, bobOnly = false)

	else if (state.clueTokens == 8)
		Log.info("forced clue at 8 clues!")
		forceClue(orig, game, offset, bobOnly = false)

	else if (RefSieve.mustClue(game, playerIndex))
		Log.info(s"forcing ${state.names(playerIndex)} to clue their Bob!")
		forceClue(orig, game, offset, bobOnly = true)

	else
		urgentDc.orElse(trash.headOption) match {
			case None =>
				val chop = RefSieve.chop(game, playerIndex).getOrElse(throw new Exception(s"Player ${state.names(playerIndex)} not locked but no chop! ${state.hands(playerIndex).map(state.deck(_))}"))
				val id = state.deck(chop).id().get
				val action = DiscardAction(playerIndex, chop, id.suitIndex, id.rank)
				val dcGame = advanceGame(game, action)

				if (state.clueTokens > 2)
					val clueGame = game.withState(s => s.copy(clueTokens = s.clueTokens - 1))

					val clueProb = if (offset == 1)
						if (common.thinksLoaded(game, bob))
							0.2
						else if (bobChop.isDefined)
							if (state.isBasicTrash(state.deck(bobChop.get).id().get)) 0.2 else 0.7
						else
							0.5
					else
						0.8

					Log.info(s"${state.names(playerIndex)} discarding ${state.logId(id)} but might clue $clueProb")
					clueProb * advance(orig, clueGame, offset + 1) + (1.0 - clueProb) * advance(orig, dcGame, offset + 1)
				else
					Log.info(s"${state.names(playerIndex)} discarding ${state.logId(id)}")
					advance(orig, dcGame, offset + 1)

			case Some(order) =>
				val id = state.deck(order).id().get
				val Identity(suitIndex, rank) = id
				val action = DiscardAction(playerIndex, order, suitIndex, rank)

				Log.info(s"${state.names(playerIndex)} discarding trash ${state.logId(id)}")
				advance(orig, advanceGame(game, action), offset + 1)
		}

def evalAction(game: RefSieve, action: Action): Double =
	Log.highlight(Console.GREEN, s"===== Predicting value for ${action.fmt(game.state)} =====")
	val state = game.state
	val hypoGame = advanceGame(game, action)

	val mistake = hypoGame.lastMove match {
		case Some(ClueInterp.Mistake) if action.isInstanceOf[ClueAction] => true
		case Some(DiscardInterp.Mistake) if action.isInstanceOf[DiscardAction] => true
		case _ => false
	}

	val value = action match {
		case _ if mistake => -100

		case clue: ClueAction =>
			val mult = if (!game.me.thinksPlayables(game, state.ourPlayerIndex).isEmpty)
				if (state.inEndgame) 0.1 else 0.25
			else
				0.5
			getResult(game, hypoGame, clue) * mult - 0.5

		case PlayAction(_, _, suitIndex, rank) =>
			if (suitIndex == -1 || rank == -1) 1.5 else 0.02 * (5 - rank)

		case _ => 0
	}

	if (value == -100)
		Log.info("mistake! -100")
		-100
	else
		Log.info(f"starting value $value%.2f")

		val best = value + advance(game, hypoGame, 1)
		Log.info(f"${action.fmt(state)}%s: $best%.2f (${hypoGame.lastMove}%s)")
		best

def evalState(state: State): Double =
	// The first 2 * (# suits) pts are worth 2.
	val scoreVal: Double = state.score.min(2 * state.variant.suits.length) + state.score

	val clueVal: Double = state.clueTokens match {
		case 0 					 => -0.5
		case _ if !state.canClue => -0.25
		case c if c > 6 		 => 3 + (c - 6) * 0.25
		case c 					 => c / 2.0
	}

	val scoreLoss = state.variant.suits.length * 5 - state.maxScore
	val dcCritVal = -8 * scoreLoss

	val strikesVal = state.strikes match {
		case 1 => -1.5
		case 2 => -3.5
		case 3 => -100
		case _ => 0
	}

	Log.info(s"state eval: score: $scoreVal, clues: $clueVal, dc crit: $dcCritVal, strikes: $strikesVal")
	scoreVal + clueVal + dcCritVal + strikesVal

def evalGame(orig: RefSieve, game: RefSieve): Double =
	val state = game.state

	if (state.score == state.maxScore)
		return 100

	val stateVal = evalState(state)

	val futureVal = state.hands.flatten.map { order =>
		game.meta(order).status match {
			case CardStatus.CalledToPlay =>
				game.me.thoughts(order).id(infer = true) match {
					case None => 0.4
					case Some(id) =>
						if (state.isBasicTrash(id))
							-1.5
						else if (id.rank == 5)
							0.8
						else
							0.4
				}
			case CardStatus.CalledToDiscard =>
				val by = game.meta(order).by.getOrElse(throw new Exception(s"order $order doesn't have a by!"))

				state.deck(order).id() match {
					case None =>
						// Trust others to discard trash
						if (by != state.ourPlayerIndex)
							0
						else
							0.5
					case Some(id) =>
						if (state.isBasicTrash(id))
							1
						else if (game.me.isSieved(game, id, order))
							0.5
						else if (state.isCritical(id))
							-(5 - state.playableAway(id)) * 10.0
						else if (by != state.ourPlayerIndex)
							0
						else
							-(5 - state.playableAway(id)) * 0.5
				}
			case _ => 0
		}
	}.sum

	val bdrVal = state.variant.allIds.map { id =>
		val prevDiscarded = orig.state.discardStacks(id.suitIndex)(id.rank - 1)
		val discarded = state.discardStacks(id.suitIndex)(id.rank - 1).filterNot(prevDiscarded.contains)

		lazy val duplicated = state.hands.flatten.exists { o =>
			state.deck(o).matches(id) ||
			game.me.thoughts(o).matches(id, infer = true) && game.meta(o).focused
		}

		if (state.isBasicTrash(id) || id.rank == 5 || discarded.isEmpty)
			0
		else if (duplicated)
			-0.1
		else
			id.rank match {
				case 1 => -math.pow(discarded.length, 2)
				case 2 => -3
				case 3 => -1.5
				case _ => -1
			}
	}.sum * 2.5

	val lockedPenalty = (0 until state.numPlayers).map { playerIndex =>
		if (!game.players(playerIndex).thinksLocked(game, playerIndex)) 0 else (state.clueTokens match {
			case c if c > 4 => -1
			case _ => -2
		})
	}.sum

	val endgamePenalty = state.endgameTurns.fold(0) { turns =>
		val finalScore = (0 until turns).foldLeft(state.playStacks) { (stacks, i) =>
			val playerIndex = (state.currentPlayerIndex + i + 1) % state.numPlayers

			state.hands(playerIndex).map(state.deck(_).id()).flatten.find(state.isPlayable).fold(stacks) { id =>
				stacks.updated(id.suitIndex, id.rank)
			}
		}.sum

		(finalScore - state.maxScore) * 5
	}

	Log.info(s"state: $stateVal, future: $futureVal, bdr: $bdrVal locked: $lockedPenalty${if (endgamePenalty != 0) s" endgame penalty: ${endgamePenalty}" else ""}")
	stateVal + futureVal + bdrVal + endgamePenalty
