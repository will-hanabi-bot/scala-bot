package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.utils._
import scala_bot.logger.Log

case class ClueContext(prev: HGroup, game: HGroup, action: ClueAction):
	inline def common = game.common
	inline def state = game.state

	lazy val focusResult = game.determineFocus(prev, action)

/** Returns the appropriate clue interpretation given the newly chop moved cards. */
def evaluateCM(ctx: ClueContext, chopMoved: Seq[Int]): ClueInterp =
	val ClueContext(prev, game, action) = ctx
	val state = game.state
	val ClueAction(giver, target, list, clue) = action

	if chopMoved.forall(state.deck(_).id().exists(state.isBasicTrash)) then
		if chopMoved.forall(game.common.orderKt(game, _)) then
			ClueInterp.Useless
		else
			ClueInterp.Mistake
	else
		ClueInterp.Discard

def unacceptableClue(prev: HGroup, game: HGroup, @annotation.unused action: ClueAction): Boolean =
	val fSymmetric = game.state.hands.flatten.find: o =>
		game.isBlindPlaying(o) &&
		!prev.isBlindPlaying(o) &&
		prev.waiting.exists: wc =>
			wc.symmetric && wc.connections.existsM:
				case f: FinesseConn => f.order == o

	if fSymmetric.isDefined then
		Log.warn(s"clue finesses ${fSymmetric.get}, preventing a symmetric finesse from being dispoven!")
		true
	else
		false

def checkHFix(ctx: ClueContext): Option[HGroup] =
	val ClueContext(prev, game, action) = ctx
	val state = game.state
	val FocusResult(focus, chop, _) = ctx.focusResult
	val ClueAction(giver, _, list, clue) = action

	checkFix(prev, game, action) match
		// Check for pink 1's fix, which won't be caught by checkFix().
		case FixResult.None if state.includesVariant(PINKISH) && prev.common.hypoStacks.exists(_ == 0) =>
			val fixed = list.filter: o =>
				!prev.meta(o).focused &&
				prev.common.thoughts(o).inferred.forall(_.rank == 1) &&
				!prev.common.thoughts(o).possible.forall(_.rank == 1) &&
				!prev.waiting.exists(wc => wc.connections.exists(_.order == o) && !prev.xmeta(o).idUncertain) && (
					(clue.kind == ClueKind.Rank && clue.value != 1) ||
					(clue.kind == ClueKind.Colour && game.knownAs(o, PINKISH) && !prev.knownAs(o, PINKISH))
				)

			if fixed.isEmpty then None else
				if clue.kind == ClueKind.Colour || (chop && (clue.value == 2 || clue.value == 5)) then
					Log.info(s"pink fix! $fixed")
					Some(fixed.foldLeft(game)((acc, o) =>
							acc.withThought(o)(t => t.copy(
								inferred = t.possible.difference(state.playableSet)
							))
						).copy(lastMove = Some(ClueInterp.Fix)))

				else
					val old1s = list.filter: o =>
						prev.common.thoughts(o).inferred.forall(_.rank == 1) &&
						!prev.common.thoughts(o).possible.forall(_.rank == 1)
					val oldOrdered1s = prev.order1s(old1s, noFilter = true)
					val fixedOrder = oldOrdered1s.head

					if state.deck(fixedOrder).id().forall(_.rank == clue.value) then
						Log.info(s"pink fix promise! $fixedOrder")
						val newGame =
							game.withThought(fixedOrder)(t => t.copy(
								inferred = t.possible.filter(i => i.rank == clue.value && !state.isPlayable(i))
							))
							.pipe: g =>
								list.filter(oldOrdered1s.contains).foldLeft(g): (acc, o) =>
									if o == fixedOrder then
										acc
									else
										acc.withThought(o)(t => t.copy(inferred = t.possible))
							.copy(lastMove = Some(ClueInterp.Fix))

						Some(newGame)

					else
						Log.error("looked like pink fix but didn't match possible interpretations?")
						None

		case FixResult.None => None

		case FixResult.Normal(cluedResets, duplicateReveals) =>
			Log.info(s"fix clue! not inferring anything else $cluedResets $duplicateReveals")
			Some(game.copy(lastMove = Some(ClueInterp.Fix)))

		case FixResult.NoNewInfo(fixes) =>
			if game.level < Level.Fix then
				Log.info("looked like out-of-level no-info fix!")
				Some(game.copy(lastMove = Some(ClueInterp.Mistake)))
			else
				Log.info(s"no info fix clue on $fixes! not inferring anything else")

				val fixTarget = Option.when(clue == BaseClue(ClueKind.Rank, 1)):
					prev.order1s(list.filter(prev.unknown1), noFilter = true).headOption
				.flatten.getOrElse(focus)

				val badFix = giver == state.ourPlayerIndex && !game.me.orderTrash(game, fixTarget)

				Some(game
					.withThought(fixTarget)(t => t.copy(inferred = t.possible.intersect(state.trashSet)))
					.withMeta(fixTarget)(_.copy(trash = true))
					.copy(lastMove = Some(if badFix then ClueInterp.Mistake else ClueInterp.Fix)))

def interpClue(ctx: ClueContext): HGroup =
	val ClueContext(prev, game, action) = ctx
	val (common, state) = (game.common, game.state)
	val ClueAction(giver, target, list, clue) = action
	val FocusResult(focus, chop, positional) = ctx.focusResult

	checkHFix(ctx) match
		case Some(newGame) => return newGame
		case _ => ()

	val stall = stallingSituation(ctx)
	val thinksStall = stall.map(_._2).getOrElse(Set.empty)

	if stall.isDefined then
		val (interp, thinksStall) = stall.get

		if thinksStall.size > 0 && thinksStall.size < state.numPlayers then
			Log.warn(s"asymmetric! only ${thinksStall.map(state.names)} think stall")

			if giver == state.ourPlayerIndex then
				return game.copy(lastMove = Some(ClueInterp.Mistake))

		else if thinksStall.size == state.numPlayers then
			Log.info(s"stalling situation $interp")

			return game
				.when(_.inEarlyGame && interp == StallInterp.Stall5):
					_.copy(stalled5 = true)
				// Pink promise on stalls
				.when(_.state.includesVariant(PINKISH) && clue.kind == ClueKind.Rank):
					_.withThought(focus)(t => t.copy(inferred = t.inferred.filter(_.rank == clue.value)))
				.copy(
					lastMove = Some(ClueInterp.Stall),
					stallInterp = Some(interp)
				)

	val distributionIds = distributionClue(prev, game, action, focus)

	if distributionIds.isDefined then
		Log.info(s"distribution clue!")

		return game
			.withThought(focus): t =>
				t.copy(
					inferred = t.possible.intersect(distributionIds.get),
					infoLock = t.possible.intersect(distributionIds.get).toOpt,
					reset = false
				)
			.withMeta(focus)(_.copy(focused = true))
			.copy(lastMove = Some(ClueInterp.Distribution))

	if game.level >= Level.BasicCM && !state.inEndgame then
		interpretTcm(ctx) match
			case None => ()
			case Some(tcm) => return handleTcm(ctx, tcm, stall.isEmpty || thinksStall.isEmpty)

		interpret5cm(ctx) match
			case None => ()
			case Some(cm5) => return performCM(game, cm5).copy(lastMove = Some(evaluateCM(ctx, cm5)))

	val pinkTrashFix = state.includesVariant(PINKISH) &&
		!positional && clue.kind == ClueKind.Rank &&
		list.forall(o => prev.state.deck(o).clued && game.knownAs(o, PINKISH)) &&
		state.variant.suits.zipWithIndex.forall: (suit, suitIndex) =>
			!suit.suitType.pinkish ||
			common.isTrash(game, Identity(suitIndex, clue.value), focus)

	if pinkTrashFix then
		Log.info(s"pink trash fix!")
		return game
			.withThought(focus): t =>
				val newInferred = t.possible.filter(common.isTrash(game, _, focus))
				t.copy(
					inferred = newInferred,
					infoLock = newInferred.toOpt
				)
			.withMeta(focus): m =>
				m.copy(trash = m.trash ||
					state.variant.suits.zipWithIndex.forall: (suit, suitIndex) =>
						!suit.suitType.pinkish ||
						game.state.isBasicTrash(Identity(suitIndex, clue.value))
				)
			.copy(lastMove = Some(ClueInterp.Fix))

	val uselessReclue = prev.state.deck(focus).clued &&
		!positional && {
			prev.common.hypoPlays.contains(focus) ||
			game.me.thoughts(focus).id().exists: id =>
				prev.common.hypoStacks(id.suitIndex) >= id.rank
		}

	if uselessReclue then
		Log.warn("nonsensical burn!")
		return game.copy(lastMove = Some(ClueInterp.Useless))

	val trashPush = chop && game.common.orderKt(game, focus)

	if trashPush && game.level <= Level.TrashMoves then
		Log.warn("out-of-level trash push! interpreting burn")
		return game.copy(lastMove = Some(ClueInterp.Useless))

	def validSave(inf: Identity) =
		!state.isBasicTrash(inf) &&
		visibleFind(state, common, inf, infer = true, excludeOrder = focus).isEmpty &&
		(if clue.kind == ClueKind.Colour then
			colourSave(prev, action, inf, focus)
		else
			rankSave(prev, action, inf, focus))

	val savePoss = if !chop then Nil else
		for
			inf <- common.thoughts(focus).inferred if validSave(inf)
		yield
			FocusPossibility(inf, Nil, ClueInterp.Save, save = true)

	if savePoss.nonEmpty then
		Log.info(s"found saves: [${savePoss.map(fp => state.logId(fp.id)).mkString(",")}]")

	val focusPoss =
		val looksDirect = common.thoughts(focus).id().isEmpty &&
			(action.clue.kind == ClueKind.Colour || savePoss.nonEmpty || positional)

		common.thoughts(focus).inferred.filter: inf =>
			!game.invalidFocus(giver, clue, inf, ctx.focusResult) &&
			visibleFind(state, game.players(target), inf, excludeOrder = focus).filter(o => state.deck(o).clued && !state.hands(giver).contains(o)).isEmpty &&
			!savePoss.exists(_.id == inf)
		.flatMap:
			connect(ctx, _, looksDirect, thinksStall)

	val simplest =
		val possible = (savePoss ++ focusPoss)
			.filter(fp => game.players(target).thoughts(focus).possible.contains(fp.id))

		occamsRazor(ctx, possible, target)

	val noSelf = !game.allowFindOwn ||
		giver == state.ourPlayerIndex ||
		simplest.exists(fp => state.deck(focus).matches(fp.id))

	{
		if noSelf then
			if simplest.isEmpty then
				Log.warn("no inferences!")
				game.copy(lastMove = Some(ClueInterp.Mistake))
			else
				Log.info(s"simplest focus possibilities [${simplest.map(fp => state.logId(fp.id)).mkString(",")}]")
				resolveClue(ctx, simplest)
		else
			Log.highlight(Console.YELLOW, s"finding own!")

			val ownFps =
				val looksDirect = game.players(target).thoughts(focus).id().isEmpty && {
					// clue.kind == ClueKind.Colour ||
					positional ||
					savePoss.nonEmpty ||
					// Looks like an existing possibility
					focusPoss.exists: fp =>
						game.players(target).thoughts(focus).possible.contains(fp.id) &&
						fp.connections.forall: c =>
							(c.isInstanceOf[KnownConn] || (c.isInstanceOf[PlayableConn] && c.reacting != state.ourPlayerIndex)) &&
							!game.isBlindPlaying(c.order)		// A blind-playing card is still unknown
				}

				common.thoughts(focus).inferred.filter: inf =>
					!game.invalidFocus(giver, clue, inf, ctx.focusResult) &&
					!(savePoss.exists(_.id == inf) || simplest.exists(_.id == inf))
				.flatMap:
					connect(ctx, _, looksDirect, thinksStall, findOwn = Some(state.ourPlayerIndex))

			val simplestOwn = occamsRazor(ctx, filterFps(ctx, simplest ++ ownFps, target), state.ourPlayerIndex, actualId = game.me.thoughts(focus).id())

			if simplestOwn.isEmpty then
				Log.warn("no inferences!")
				game.copy(lastMove = Some(ClueInterp.Mistake))
			else
				resolveClue(ctx, simplestOwn, if savePoss.nonEmpty then Nil else ownFps.filter(fp => !simplestOwn.contains(fp) && !fp.symmetric))
	}
	.when(g => g.lastMove != Some(ClueInterp.Mistake) && g.level >= Level.TempoClues && state.numPlayers > 2): g =>
		val newCtx = ctx.copy(game = g)
		interpretTccm(newCtx) match
			case Some(tccm) if stall.isEmpty || thinksStall.isEmpty =>
				performCM(g, tccm).copy(
					lastMove = Some(evaluateCM(newCtx, tccm))
				)
			case Some(_) =>
				Log.info("stalling situation, tempo clue stall!")
				g.copy(lastMove = Some(ClueInterp.Stall), stallInterp = Some(StallInterp.Tempo))
			case _ =>
				g

	.when(_.state.includesVariant(PINKISH) && clue.kind == ClueKind.Rank && clue.value == 1): g =>
		// Pink 1's Assumption
		list.filter(g.unknown1).foldLeft(g): (acc, o) =>
			acc.withThought(o)(t => t.copy(inferred = t.inferred.filter(_.rank == 1)))

	.when(g => unacceptableClue(prev, g, action)): g =>
		g.copy(lastMove = Some(ClueInterp.Mistake))
