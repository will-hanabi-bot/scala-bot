package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.utils._
import scala_bot.logger.Log
import scala_bot.utils.visibleFind

def getResult(game: HGroup, hypo: HGroup, action: ClueAction): Double =
	val (common, state, meta) = (game.common, game.state, game.meta)
	val FocusResult(focus, chop, _) = game.determineFocus(game, action)
	val ClueAction(giver, target, list, clue) = action

	if hypo.lastMove == Some(ClueInterp.Mistake) then
		return -100

	val newPlayables = state.hands.flatten.filter: o =>
		meta(o).status != CardStatus.Finessed && hypo.meta(o).status == CardStatus.Finessed

	val badPlay = newPlayables.find: o =>
		!(hypo.me.hypoPlays.contains(o) || (state.inEndgame && state.deck(o).id().exists(state.isPlayable)))

	if badPlay.isDefined then
		Log.warn(s"clue ${clue.fmt(state, target)} results in ${state.logId(badPlay.get)} ${badPlay.get} looking playable! ${hypo.me.hypoPlays}")
		return -100

	val badTrash = state.hands.flatten.find: o =>
		!meta(o).trash && hypo.meta(o).trash &&
		state.deck(o).id().exists: id =>
			state.isUseful(id) && visibleFind(game.state, game.players(giver), id, excludeOrder = o).isEmpty

	if badTrash.isDefined then
		Log.warn(s"clue ${clue.fmt(state, target)} results in ${state.logId(badTrash.get)} ${badTrash.get} looking trash!")
		return -100

	def distance5(g: HGroup, playerIndex: Int) =
		g.chop(playerIndex).flatMap: chop =>
			val oldest5 = state.hands(playerIndex).findLast: o =>
				o > chop &&
				g.state.deck(o).rank == 5 &&
				!g.state.deck(o).clued

			oldest5.map(g.chopDistance(playerIndex, _))

	if giver == state.ourPlayerIndex && hypo.lastMove == Some(ClueInterp.Stall) && hypo.stallInterp == Some(StallInterp.Stall5) then
		val dist5 = game.chopDistance(target, list.filter(o => !state.deck(o).clued && game.meta(o).status != CardStatus.ChopMoved).min)
		val bad5Stall = (0 until state.numPlayers).exists: i =>
			i != giver &&
			distance5(game, i).exists(_ < dist5)

		if bad5Stall then
			Log.warn(s"gave 5 stall to 5 not closest to chop!")
			return -100

	val (newTouched, fill, elim) = elimResult(game, hypo, hypo.state.hands(target), list)
	val (badTouch, trash, avoidableDupe) = badTouchResult(game, hypo, action)
	val (_, playables) = playablesResult(game, hypo)

	if playables.isEmpty && hypo.lastMove == Some(ClueInterp.Play) then
		Log.warn("play clue with no new playables! (duped existing playable?)")
		return -100

	val revealedTrash = hypo.common.thinksTrash(hypo, target).count: o =>
		hypo.state.deck(o).clued && !common.thinksTrash(game, target).contains(o)

	// Previously-unclued playables whose copies are already touched
	val dupedPlayables = hypo.me.hypoPlays.count: p =>
		!state.deck(p).clued &&
		state.hands.flatten.exists: o =>
			o != p && game.isTouched(o) && state.deck(o).matches(state.deck(p))

	val goodTouch: Double =
		if badTouch.length > 0 then
			-badTouch.length * 4
		else
			2 * List(0.0, 0.125, 0.25, 0.35, 0.45, 0.55)(newTouched.length)

	val precision = (list.summing(o => -0.001 * hypo.common.thoughts(o).inferred.length)) / list.length

	val untouchedPlays = playables.count(!hypo.state.deck(_).clued)

	val pangOfGuilt = if state.clueTokens > 1 && target != state.nextPlayerIndex(state.ourPlayerIndex) && game.findFinesse(target).contains(focus) then -0.25 else 0

	val playablesS = playables.map(state.logId(_)).mkString(", ")
	Log.info(s"good touch: $goodTouch, playables: [$playablesS], duped: $dupedPlayables, trash: ${trash.length}, fill: ${fill.length}, elim: ${elim.length}, bad touch: $badTouch, avoidDupe: $avoidableDupe, ${hypo.lastMove}")

	val value = goodTouch +
		(if playables.nonEmpty then 0.5 else 0) +
		-dupedPlayables +
		0.2 * untouchedPlays +
		(if state.inEndgame then 0.01 else 0.1) * revealedTrash +
		(if state.inEndgame then 0.2 else 0.1) * fill.length +
		(if state.inEndgame then 0.1 else 0.05) * elim.length +
		-0.1 * badTouch.length +
		-2 * avoidableDupe +
		pangOfGuilt +
		precision

	hypo.lastMove match
		case Some(ClueInterp.Useless)  => value - 10
		case Some(ClueInterp.Fix)      => value + 3
		case _ => value

private def clueFilter(game: HGroup, giver: Int) =
	val state = game.state

	(clue: Clue) =>
		val Clue(kind, value, target) = clue
		val list = state.clueTouched(state.hands(target), clue)
		val focus = game.determineFocus(game, ClueAction(giver, target, list, clue.base)).focus

		// Don't clue known duplicates
		state.deck(focus).id().forall: id =>
			visibleFind(state, game.common, id, infer = true, excludeOrder = focus).isEmpty

def _forceClue(orig: HGroup, game: HGroup, offset: Int, only: Option[Int] = None): Double =
	val state = game.state
	val giver = (state.ourPlayerIndex + offset) % state.numPlayers

	val adv = (game: HGroup) =>
		advance(orig, game.copy(dcStatus = DcStatus.None), offset + 1)

	forceClue(game.copy(allowFindOwn = false), giver, adv, only, clueFilter = clueFilter(game, giver))

def advance(orig: HGroup, game: HGroup, offset: Int): Double =
	val (state, common, meta) = (game.state, game.common, game.meta)
	val playerIndex = (state.ourPlayerIndex + offset) % state.numPlayers
	val player = game.players(playerIndex)

	val bob = state.nextPlayerIndex(playerIndex)
	val bobChop = Option.when(state.numPlayers > 2)(game.chop(bob)).flatten

	lazy val trash = player.thinksTrash(game, playerIndex)
	val allPlayables = player.thinksPlayables(game, playerIndex)

	lazy val earlyGameClue = game.earlyGameClue(playerIndex)

	if playerIndex == state.ourPlayerIndex || state.endgameTurns.contains(0) then
		evalGame(orig, game)

	else if allPlayables.nonEmpty then
		val unknown1s = allPlayables.filter(game.unknown1)
		val next1 = game.order1s(unknown1s).headOption
		val playables = allPlayables.find(meta(_).urgent) match
			case Some(order) => List(order)
			case None => allPlayables.filter: o =>
				if unknown1s.contains(o) then
					next1.contains(o)
				else
					// Only consider playing the leftmost of similarly-possible cards
					!allPlayables.exists(p => p > o && common.thoughts(p).possible == common.thoughts(o).possible)

		val finessed = playables.find(meta(_).status == CardStatus.Finessed)

		if finessed.isDefined then
			val order = finessed.get
			val (id, action) = state.deck(order).id() match
				case None => (None, PlayAction(playerIndex, order, -1, -1))
				case Some(id) =>
					val action = if state.isPlayable(id) then
						PlayAction(playerIndex, order, id.suitIndex, id.rank)
					else
						DiscardAction(playerIndex, order, id.suitIndex, id.rank, failed = true)
					(Some(id), action)

			Log.info(s"${state.names(playerIndex)} ${if id.exists(state.isPlayable) then "playing" else "bombing"} ${state.logId(id)} (f)")
			advance(orig, game.simulate(action), offset + 1)
		else
			var strikes = 0

			val (knownPlays, unknownPlays) = playables.partitionMap: order =>
				val (id, action) = state.deck(order).id() match
					case None => (None, PlayAction(playerIndex, order, -1, -1))
					case Some(id) =>
						val action = if state.isPlayable(id) then
							PlayAction(playerIndex, order, id.suitIndex, id.rank)
						else
							DiscardAction(playerIndex, order, id.suitIndex, id.rank, failed = true)
						(Some(id), action)

				Log.info(s"${state.names(playerIndex)} ${if id.exists(state.isPlayable) then "playing" else "bombing"} ${state.logId(id)}")
				val advancedGame = game.simulate(action)

				if advancedGame.state.strikes > game.state.strikes then
					strikes += 1

				val value = advance(orig, advancedGame, offset + 1)
				if player.thoughts(order).id(infer = true).isDefined then Left(value) else Right(value)

			val maxPlay = math.min(knownPlays.maxOption.getOrElse(99.9), unknownPlays.minOption.getOrElse(99.9))

			if strikes > 0 then
				(knownPlays ++ unknownPlays).min
			else
				maxPlay.max(_forceClue(orig, game, offset))

	else if player.thinksLocked(game, playerIndex) then
		Log.info(s"${state.names(playerIndex)} locked!")
		if !state.canClue then
			val action =
				val anxietyPlay = player.anxietyPlay(state, playerIndex)

				if game.level >= Level.Stalling && anxietyPlay.isDefined then
					val id = state.deck(anxietyPlay.get).id().get

					Log.info(s"${state.names(playerIndex)} playing ${state.logId(id)} (${anxietyPlay.get}) from anxiety!")
					if state.isPlayable(id) then
						PlayAction(playerIndex, anxietyPlay.get, id.suitIndex, id.rank)
					else
						DiscardAction(playerIndex, anxietyPlay.get, id.suitIndex, id.rank, failed = true)
				else
					val lockedDc = player.lockedDiscard(state, playerIndex)
					val id = state.deck(lockedDc).id().get
					Log.info(s"${state.names(playerIndex)} discarding ${state.logId(id)} ($lockedDc) from a locked hand!")
					DiscardAction(playerIndex, lockedDc, id.suitIndex, id.rank)

			advance(orig, game.simulate(action), offset + 1)
		else
			_forceClue(orig, game, offset)

	else if state.clueTokens == 8 then
		Log.info(s"forced clue at 8 clues! (${state.names(playerIndex)})")
		_forceClue(orig, game, offset)

	else if game.dcStatus != DcStatus.None then
		Log.info(s"forced clue due to scream/shout! (${state.names(playerIndex)})")
		_forceClue(orig, game, offset)

	else if earlyGameClue.isDefined then
		Log.info(s"forced clue in early game! (${state.names(playerIndex)}) ${earlyGameClue.get.fmt(state)}")
		_forceClue(orig, game, offset)

	else if game.mustClue(playerIndex) then
		Log.info(s"forcing ${state.names(playerIndex)} to clue ${state.names(state.nextPlayerIndex(playerIndex))}!")
		_forceClue(orig, game, offset, only = Some(bob))

	else
		trash.headOption match
			case None =>
				val chop = game.chop(playerIndex).getOrElse:
					throw new Exception(s"Player ${state.names(playerIndex)} not locked but no chop! ${state.hands(playerIndex).map(o => s"${state.deck(o).clued} ${game.meta(o).status}").mkString(", ")}")
				val id = state.deck(chop).id().get
				val action = DiscardAction(playerIndex, chop, id.suitIndex, id.rank)
				val dcGame = game.simulate(action)

				if state.canClue then
					val clueProb = if offset == 1 then
						if common.thinksLoaded(game, bob) then
							0.2
						else if bobChop.isDefined then
							if state.isBasicTrash(state.deck(bobChop.get).id().get) then 0.2 else 0.7
						else
							0.5
					else
						0.8

					Log.info(s"${state.names(playerIndex)} discarding ${state.logId(id)} but might clue $clueProb")
					val clueVal = _forceClue(orig, game, offset)
					val dcVal = advance(orig, dcGame, offset + 1)

					if clueVal < evalGame(orig, game) then
						val drewNew = state.hands.zipWithIndex.indexWhere((hand, i) => i != state.ourPlayerIndex && hand.exists(state.deck(_).id().isEmpty))

						if drewNew != -1 then
							Log.info(s"no visible clue available for ${state.names(playerIndex)} but ${state.names(drewNew)} may have drawn something")
							0.2 * clueVal + 0.8 * dcVal
						else
							Log.info(s"no visible clue available for ${state.names(playerIndex)}, lowering clue prob to 0")
							dcVal
					else
						clueProb * clueVal + (1.0 - clueProb) * dcVal
				else
					Log.info(s"${state.names(playerIndex)} discarding ${state.logId(id)}")
					advance(orig, dcGame, offset + 1)

			case Some(order) =>
				val id = state.deck(order).id().get
				val Identity(suitIndex, rank) = id
				val action = DiscardAction(playerIndex, order, suitIndex, rank)

				Log.info(s"${state.names(playerIndex)} discarding trash ${state.logId(id)}")
				advance(orig, game.simulate(action), offset + 1)

def _evalAction(game: HGroup, action: Action): Double =
	Log.highlight(Console.GREEN, s"===== Predicting value for ${action.fmt(game.state)} =====")
	val state = game.state

	val (lastMove, value) = action match
		case clue: ClueAction =>
			val hypoGame = game.copy(allowFindOwn = false).simulate(action)
			val clueValue = getResult(game, hypoGame, clue)

			if hypoGame.lastMove == Some(ClueInterp.Mistake) || clueValue == -100 then
				(ClueInterp.Mistake, -100.0)
			else
				val bonus = if hypoGame.lastMove == Some(ClueInterp.Fix) then 0.5 else 0
				val value =
					if game.me.thinksPlayables(game, state.ourPlayerIndex).nonEmpty && state.inEndgame then
						-1 + 0.5 * clueValue
					else
						0.5 * clueValue
				Log.info(s"initial clue value: $value")
				val best = value + advance(game, hypoGame, 1)

				Log.info(f"${action.fmt(state)}%s: $best%.2f (${hypoGame.lastMove}%s) + $bonus")
				(hypoGame.lastMove.get, best + bonus)

		case PlayAction(playerIndex, order, suitIndex, rank) if suitIndex != -1 && rank != -1 =>
			val hypoGame = game.copy(allowFindOwn = false).simulate(action)

			if hypoGame.lastMove == Some(PlayInterp.Mistake) then
				(PlayInterp.Mistake, -100.0)
			else
				val bonus = if game.isBlindPlaying(order) then 1.0 else 0
				val best = advance(game, hypoGame, 1)
				Log.info(f"${action.fmt(state)}%s: $best%.2f (${hypoGame.lastMove}%s) + $bonus")
				(hypoGame.lastMove.get, best + bonus)

		case PlayAction(playerIndex, order, _, _) =>
			val uniqueInfs = game.me.thoughts(order).inferred.toList.filterNot: id =>
				visibleFind(state, game.me, id, cond = (playerIndex, _) => playerIndex != state.ourPlayerIndex).exists(state.deck(_).clued)

			uniqueInfs.foldLeftOpt[(Interp, Double)]((PlayInterp.None, 0.0)):
				case ((_, value), id) =>
					Log.highlight(Console.GREEN, s"playing ${state.logId(id)}")
					val hypoGame = game.copy(allowFindOwn = false).simulate(PlayAction(playerIndex, order, id.suitIndex, id.rank))

					if hypoGame.lastMove == Some(PlayInterp.Mistake) then
						Left((PlayInterp.Mistake, -100.0))
					else
						val bonus = if game.isBlindPlaying(order) then 1.0 else 0
						val best = advance(game, hypoGame, 1)
						Log.info(f"${action.fmt(state)}%s: $best%.2f (${hypoGame.lastMove}%s) + $bonus")
						Right((hypoGame.lastMove.get, value + best + bonus))
			.pipe: (lastMove, value) =>
				(lastMove, value / uniqueInfs.length)

		case DiscardAction(_, order, suitIndex, rank, _) if (suitIndex != -1 && rank != -1) || game.me.orderTrash(game, order) =>
			val hypoGame = game.copy(allowFindOwn = false).simulate(action)

			if hypoGame.lastMove == Some(DiscardInterp.Mistake) then
				(DiscardInterp.Mistake, -100.0)
			else
				val best = advance(game, hypoGame, 1)
				Log.info(f"${action.fmt(state)}%s: $best%.2f (${hypoGame.lastMove}%s)")
				(hypoGame.lastMove.get, best)

		case DiscardAction(_, order, suitIndex, rank, _) if !state.deck(order).clued =>
			val hypoGame = game.copy(allowFindOwn = false).simulate(action)

			if hypoGame.lastMove == Some(DiscardInterp.Mistake) then
				(DiscardInterp.Mistake, -100.0)
			else
				val best = -0.5 + advance(game, hypoGame, 1)
				Log.info(f"${action.fmt(state)}%s: $best%.2f (${hypoGame.lastMove}%s)")
				(hypoGame.lastMove.get, best)

		case DiscardAction(playerIndex, order, _, _, failed) =>
			game.me.thoughts(order).inferred.toList.foldLeftOpt[(Interp, Double)]((DiscardInterp.None, 0.0)):
				case ((_, value), i) =>
					Log.highlight(Console.GREEN, s"discarding ${state.logId(i)}")
					val hypoGame = game.copy(allowFindOwn = false).simulate(DiscardAction(playerIndex, order, i.suitIndex, i.rank, failed))

					if hypoGame.lastMove == Some(DiscardInterp.Mistake) then
						Left((DiscardInterp.Mistake, -100.0))
					else
						val best = advance(game, hypoGame, 1)
						Log.info(f"${action.fmt(state)}%s: $best%.2f (${hypoGame.lastMove}%s)")
						Right((hypoGame.lastMove.get, best + value))
			.pipe: (lastMove, value) =>
				(lastMove, value / game.me.thoughts(order).inferred.length)

		case _ => throw new Error("impossible")

	if value == -100 then
		Log.info("mistake! -100")
		-100
	else
		Log.info(f"${action.fmt(state)}%s: $value%.2f ($lastMove%s)")
		value

def evalState(state: State, inEndgame: Boolean): Double =
	// The first 2 * (# suits) pts are worth 1.5.
	val scoreVal: Double =
		if inEndgame then
			2 * state.score
		else
			state.score.min(2 * state.variant.suits.length) * 0.5 + state.score

	val clueVal: Double =
		if inEndgame then 0 else
			state.clueTokens match
			case 0 					 => 0
			case _ if !state.canClue => 0
			case c if c > 6 		 => 3 + (c - 6) * 0.25
			case c 					 => c / 2.0

	val scoreLoss = state.variant.suits.length * 5 - state.maxScore
	val dcCritVal = -20 * scoreLoss

	val strikesVal = state.strikes match
		case 1 => -2.5
		case 2 => -4.5
		case 3 => -100
		case _ => 0

	Log.info(s"state eval: score: $scoreVal, clues: $clueVal, dc crit: $dcCritVal, strikes: $strikesVal")
	scoreVal + clueVal + dcCritVal + strikesVal

def evalGame(orig: HGroup, game: HGroup): Double =
	val state = game.state

	if state.score == state.maxScore && state.score == orig.state.maxScore then
		return 100

	val stateVal = evalState(state, inEndgame = orig.state.inEndgame)

	val futureVal = (if orig.inEarlyGame then 2 else 1) * game.common.hypoPlays.summing: order =>
		game.me.thoughts(order).id(infer = true) match
			case None => 0.4
			case Some(id) =>
				if state.isBasicTrash(id) then
					if state.holderOf(order) == state.ourPlayerIndex then
						0.0
					else
						-1.5
				else if id.rank == 5 then
					0.45
				else
					0.4

	val bdrVal = state.variant.allIds.summing: id =>
		val prevDiscarded = orig.state.discardStacks(id.suitIndex)(id.rank - 1)
		val discarded = state.discardStacks(id.suitIndex)(id.rank - 1).filterNot(prevDiscarded.contains)

		lazy val duplicated = state.hands.flatten.exists: o =>
			state.deck(o).matches(id) ||
			game.me.thoughts(o).matches(id, infer = true) && game.meta(o).focused

		lazy val potentialOwn = !dupeResponsibility(orig, id, orig.state.holderOf(discarded.head)).contains(state.ourPlayerIndex)

		if state.isBasicTrash(id) || id.rank == 5 || discarded.isEmpty then
			0
		else if duplicated then
			-0.01
		else if potentialOwn then
			-0.01
		else
			id.rank match
				case 1 => -math.pow(state.discardStacks(id.suitIndex)(id.rank - 1).length, 2)
				case 2 => -6
				case 3 => -1.5
				case _ => -0.5

	val touchVal = state.hands.flatten.summing: o =>
		if !state.deck(o).clued && game.meta(o).status == CardStatus.None then
			0
		else if game.common.orderTrash(game, o) then
			0.05
		else
			state.deck(o).id() match
				case Some(id) if state.isBasicTrash(id) => -0.1
				case _ => Array(0.0, 0.1, 0.07, 0.05, 0.04, 0.03)(game.common.thoughts(o).inferred.length.min(5))

	val lockedPenalty = (0 until state.numPlayers).summing: playerIndex =>
		if !game.players(playerIndex).thinksLocked(game, playerIndex) then 0 else
			state.clueTokens match
				case c if c > 4 => -1
				case _ => -2

	val endgamePenalty = if orig.state.endgameTurns.isEmpty then 0 else state.endgameTurns.fold(0): turns =>
		val finalScore = (0 until turns).foldLeft(state.playStacks) { (stacks, i) =>
			val playerIndex = (orig.state.currentPlayerIndex + i + 1) % state.numPlayers

			state.hands(playerIndex).map(state.deck(_).id()).flatten.find(state.isPlayable).fold(stacks): id =>
				stacks.updated(id.suitIndex, id.rank)
		}.sum

		(finalScore - state.maxScore) * 5

	val badCM = -1 * game.state.hands.flatten.count: o =>
		game.isCM(o) &&
		game.state.deck(o).id().exists: id =>
			state.isBasicTrash(id) ||
			state.hands(state.holderOf(o)).exists(p => p != o && game.me.thoughts(p).matches(id, infer = true) && game.isSaved(p))
		&&
		!game.common.orderKt(game, o)

	val playablesDiscarded = -1 * game.state.playableSet.count: id =>
		game.state.discardStacks(id.suitIndex)(id.rank - 1).length > orig.state.discardStacks(id.suitIndex)(id.rank - 1).length &&
		visibleFind(game.state, game.me, id, infer = true).forall(!game.isTouched(_))

	Log.info(s"state: $stateVal, future: $futureVal, bdr: $bdrVal touch: $touchVal badCM: $badCM plays dc'd: $playablesDiscarded locked: $lockedPenalty${if endgamePenalty != 0 then s" endgame penalty: ${endgamePenalty}" else ""}")
	stateVal + futureVal + bdrVal + touchVal + badCM + playablesDiscarded + lockedPenalty + endgamePenalty
