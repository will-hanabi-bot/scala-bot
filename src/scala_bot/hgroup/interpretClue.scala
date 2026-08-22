package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.lib.FastBitSet
import scala_bot.utils._
import scala_bot.logger.Log

case class ClueContext(prev: HGroup, game: HGroup, action: ClueAction, focusOverride: Option[FocusResult] = None):
	inline def common = game.common
	inline def state = game.state

	lazy val focusResult = focusOverride.getOrElse(game.determineFocus(prev, action))

	def withFocus(newFocus: Int): ClueContext =
		copy(focusOverride = Some(focusResult.copy(focus = newFocus)))

/** Returns the appropriate clue interpretation given the newly chop moved cards. */
def evaluateCM(ctx: ClueContext, chopMoved: Seq[Int]): ClueInterp =
	val ClueContext(prev, game, action, _) = ctx
	val state = game.state
	val ClueAction(giver, target, list, clue) = action

	// Chop is a playable inverted card
	if state.deck(chopMoved.min).id().exists(id => state.isInverted(id) && state.isPlayable(id)) then
		ClueInterp.Mistake
	else if chopMoved.forall(state.deck(_).id().exists(id => state.isBasicTrash(id) || id.rank == 1)) then
		if chopMoved.forall(game.common.orderKt(game, _)) then
			ClueInterp.Useless
		else
			ClueInterp.Mistake
	else
		ClueInterp.Discard

def unacceptableClue(prev: HGroup, game: HGroup, @annotation.unused action: ClueAction): Boolean =
	val fSymmetric = game.state.heldOrders.find: o =>
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
	val ClueContext(prev, game, action, _) = ctx
	val state = game.state
	val FocusResult(focus, chop, _) = ctx.focusResult
	val ClueAction(giver, _, list, clue) = action

	checkFix(prev, game, action) match
		// Check for pink 1's fix, which won't be caught by checkFix().
		case FixResult.None if state.variant.pinkish && prev.common.hypoStacks.contains(0) =>
			val fixed = list.filter: o =>
				!prev.meta(o).focused &&
				// state.deck(o).rank != 1 &&
				prev.common.thoughts(o).inferred.forall(_.rank == 1) &&
				!prev.common.thoughts(o).possible.forall(_.rank == 1) &&
				!prev.waiting.exists(wc => wc.connections.exists(_.order == o) && !prev.xmeta(o).idUncertain) && (
					(clue.kind == ClueKind.Rank && clue.value != 1) ||
					(clue.kind == ClueKind.Colour && game.knownAs(o, PINKISH) && !prev.knownAs(o, PINKISH))
				)

			val fixed1s = fixed.filter(prev.unknown1)

			if fixed.isEmpty then None else
				if clue.kind == ClueKind.Colour || (chop && (clue.value == 2 || clue.value == 5)) then
					Log.info(s"pink fix! $fixed")
					Some(fixed.foldLeft(game)((acc, o) =>
							acc.withThought(o)(t => t.copy(
								inferred = t.possible.difference(state.playableSet)
							))
						).withMove(ClueInterp.Fix))

				else if fixed1s.nonEmpty then
					val fixedOrder = prev.next1(fixed1s).get

					Log.info(s"pink fix promise! $fixedOrder")

					val mistake = !state.deck(fixedOrder).id().forall(_.rank == clue.value)
					if mistake then
						Log.error("looked like pink fix but didn't match possible interpretations?")

					Some:
						game.withThought(fixedOrder): t =>
							val newInferred = t.possible.filter(i => i.rank == clue.value && !state.isPlayable(i))
							t.copy(
								inferred = newInferred,
								infoLock = newInferred.toOpt
							)
						.pipe: g =>
							list.foldLeft(g): (acc, o) =>
								if o == fixedOrder then
									acc
								else
									acc.withThought(o)(t => t.copy(inferred = t.possible))
						.withMove(if !mistake then ClueInterp.Fix else ClueInterp.Mistake)
				else
					None

		case FixResult.Normal(cluedResets, duplicateReveals) =>
			Log.info(s"fix clue! not inferring anything else $cluedResets $duplicateReveals")
			Some(game.withMove(ClueInterp.Fix))

		case _ => None

def interpClue(ctx: ClueContext): HGroup =
	val ClueContext(prev, game, action, _) = ctx
	val ClueAction(giver, target, list, clue) = action

	if game.state.options.emptyClues && list.length == 0 then
		Log.highlight(Console.YELLOW, "empty clue!")
		return game.withMove(ClueInterp.Useless)

	interpSpecialClue(ctx) match
		case SpecialClueResult.SpecialClue(newGame) => return newGame
		case SpecialClueResult.NoInterp(interp, thinksStall) =>
			interpNormalClue(ctx, interp, thinksStall)

def interpNormalClue(ctx: ClueContext, symmetricInterp: SymmetricInterp, thinksStall: FastBitSet) =
	val ClueContext(prev, game, action, _) = ctx
	val (common, state) = (game.common, game.state)
	val ClueAction(giver, target, list, clue) = action

	val FocusResult(focus, chop, positional) = ctx.focusResult

	def validSave(inf: Identity) =
		state.isUseful(inf) &&
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
			(action.clue.kind == ClueKind.Colour || savePoss.nonEmpty || positional.isDefined)

		common.thoughts(focus).inferred.filter: inf =>
			!prev.invalidFocus(giver, clue, inf, ctx.focusResult) &&
			!state.heldOrders.exists: o =>
				o != focus &&
				prev.state.deck(o).clued &&
				game.players(giver).thoughts(o).matches(inf, infer = true) && {			// giver knows about it, and
					game.players(target).thoughts(o).matches(inf, infer = true) ||		// either target knows about it too, or
					(state.ourHand.contains(o) && game.me.thoughts(o).matches(inf, infer = true))	// we know we have it in our hand
				}
			&&
			!savePoss.exists(_.id == inf)
		.flatMap:
			connect(ctx, _, looksDirect, thinksStall)

	val simplest =
		val possible = (savePoss ++ focusPoss)
			.filter: fp =>
				game.players(target).thoughts(focus).possible.contains(fp.id) ||
				game.players(giver).thoughts(focus).possible.contains(fp.id)

		occamsRazor(ctx, possible, target)

	val noSelf = !game.allowFindOwn ||
		giver == state.ourPlayerIndex ||
		simplest.exists(fp => state.deck(focus).matches(fp.id))

	{
		if noSelf then
			if simplest.isEmpty then
				Log.warn("no inferences!")

				if game.inEndgame && clue.kind == ClueKind.Colour && list.length == 1 && game.common.thoughts(focus).id().nonEmpty && state.numPlayers == 2 then
					game.withMove(ClueInterp.Useless)
				else
					game.withMove(ClueInterp.Mistake)
			else
				Log.info(s"simplest focus possibilities [${simplest.map(fp => state.logId(fp.id)).mkString(",")}]")
				resolveClue(ctx, simplest, symmetricInterp, thinksStall)
		else
			Log.highlight(Console.YELLOW, s"finding own!")

			val ownFps =
				val looksDirect = game.players(target).thoughts(focus).id().isEmpty && {
					// clue.kind == ClueKind.Colour ||
					positional.isDefined ||
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

				if giver != state.ourPlayerIndex && game.state.variant.inverted && chop && clue.kind == ClueKind.Colour && state.variant.colourableSuits(clue.value).suitType.inverted then
					Log.info(s"orange fix on trash!")
					game.withThought(focus)(t => t.copy(inferred = IdentitySet.empty))
						.withMeta(focus)(_.copy(trash = true))
						.withMove(ClueInterp.Fix)
				else if game.level >= Level.Context && chop && list.size > 1 then
					val newFocus = list.max
					val newCtx = ctx.withFocus(newFocus)

					Log.highlight(Console.YELLOW, s"attempting focus inversion to $newFocus!")

					val looksDirect = common.thoughts(newFocus).id().isEmpty &&
						(action.clue.kind == ClueKind.Colour || savePoss.nonEmpty || positional.isDefined)

					val focusPoss = common.thoughts(newFocus).inferred.filter: inf =>
						!prev.invalidFocus(giver, clue, inf, newCtx.focusResult) &&
						!state.heldOrders.exists: o =>
							o != newFocus &&
							prev.state.deck(o).clued &&
							game.players(giver).thoughts(o).matches(inf, infer = true) && {			// giver knows about it, and
								game.players(target).thoughts(o).matches(inf, infer = true) ||		// either target knows about it too, or
								(state.ourHand.contains(o) && game.me.thoughts(o).matches(inf, infer = true))	// we know we have it in our hand
							}
						&&
						!savePoss.exists(_.id == inf)
					.flatMap:
						connect(newCtx, _, looksDirect, thinksStall)

					val newSimplest =
						val possible = focusPoss.filter: fp =>
							game.players(target).thoughts(newFocus).possible.contains(fp.id) ||
							game.players(giver).thoughts(newFocus).possible.contains(fp.id)

						Log.info(s"focus poss ${focusPoss.map(_.id)} ${possible.map(_.id)} ${newCtx.focusResult.focus}")

						occamsRazor(newCtx, possible, target)

					if newSimplest.isEmpty then
						Log.warn(s"no inferences! (still)")
						game.withMove(ClueInterp.Mistake)
					else
						Log.info(s"simplest focus possibilities [${newSimplest.map(fp => state.logId(fp.id)).mkString(",")}]")
						resolveClue(newCtx, newSimplest, symmetricInterp, thinksStall)
				else
					game.withMove(ClueInterp.Mistake)
			else
				resolveClue(ctx, simplestOwn, symmetricInterp, thinksStall, ambiguousOwn = if savePoss.nonEmpty then Nil else ownFps.filter(fp => !simplestOwn.contains(fp) && !fp.symmetric))
	}
	.when(g => g.lastMove != Some(ClueInterp.Mistake) && g.level >= Level.TempoClues && state.numPlayers > 2): g =>
		val newCtx = ctx.copy(game = g)
		interpretTccm(newCtx) match
			case Some(tccm) if thinksStall.isEmpty =>
				performCM(g, tccm).withMove(evaluateCM(newCtx, tccm), overwrite = true)
			case Some(_) =>
				Log.info("stalling situation, tempo clue stall!")
				g.withMove(ClueInterp.Stall, overwrite = true).copy(stallInterp = Some(StallInterp.Tempo))
			case _ =>
				g

	.when(_.state.variant.pinkish && clue.isEq(ClueKind.Rank, 1)): g =>
		// Pink 1's Assumption
		list.filter(g.unknown1).foldLeft(g): (acc, o) =>
			acc.withThought(o)(t => t.copy(inferred = t.inferred.filter(_.rank == 1)))

	.when(g => unacceptableClue(prev, g, action)): g =>
		g.withMove(ClueInterp.Mistake, overwrite = true)
