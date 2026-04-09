package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.utils._
import scala_bot.logger.Log

val STALL_INDICES = Map(
	StallInterp.Stall5 -> 0,
	StallInterp.SaveLHS -> 0,
	StallInterp.Tempo -> 1,
	StallInterp.FillIn -> 2,
	StallInterp.Locked -> 3,
	StallInterp.Clues8 -> 4,
	StallInterp.Burn -> 5
)

val STALL_TO_SEVERITY = Map(
	StallInterp.Stall5 -> 0,
	StallInterp.Tempo -> 1,
	StallInterp.FillIn -> 2,
	StallInterp.Locked -> 2,
	StallInterp.SaveLHS -> 2,
	StallInterp.Clues8 -> 2,
	StallInterp.Burn -> 5
)

def stallSeverity(game: HGroup, player: Player, giver: Int, infoPlayer: Option[Player] = None) =
	val state = game.state

	lazy val severity2 =
		game.dcStatus == DcStatus.Scream ||
		game.dcStatus == DcStatus.Shout ||
		game.level >= Level.Stalling && game.state.numPlayers > 2 && game.dda.exists: id =>
			state.isCritical(id) &&
			game.chop(giver).exists: chop =>
				infoPlayer.getOrElse(player).thoughts(chop).possible.contains(id)

	if state.clueTokens == 8 && state.turnCount != 1 then
		4
	else if player.thinksLocked(game, giver) then
		3
	else if severity2 then
		2
	else if game.level >= 2 && game.inEarlyGame then
		1
	else
		0

def isStall(ctx: ClueContext, severity: Int): Option[StallInterp] =
	val ClueContext(prev, game, action) = ctx
	val state = ctx.state
	val ClueAction(giver, target, list, clue) = action
	val FocusResult(focus, chop, _) = ctx.focusResult

	val focusNew = !prev.state.deck(focus).clued
	val reclue = list.forall(o => prev.state.deck(o).clued || prev.meta(o).cm)

	lazy val trash = state.deck(focus).id().exists(state.isBasicTrash) ||
		game.me.orderKt(game, focus)

	def isSave(id: Identity) =
		if clue.kind == ClueKind.Colour then
			colourSave(prev, action, id, focus)
		else
			rankSave(prev, action, id, focus)

	val notStall = severity == 0 ||
		(chop && game.players(target).thoughts(focus).possible.forall(isSave)) ||
		(focusNew && (game.common.orderKp(game, focus) || trash))

	if notStall then
		return None

	val stall5 = clue.isEq(BaseClue(ClueKind.Rank, 5)) &&
		focusNew &&
		!prev.meta(focus).cm &&
		!chop

	if stall5 then
		Log.info(s"5 stall!")
		return Some(StallInterp.Stall5)

	val (_, fill, _) = elimResult(prev, game, state.hands(target), list)
	val (_, playables) = playablesResult(prev, game)

	if severity >= 2 && reclue then
		if playables.nonEmpty then
			Log.info(s"tempo clue stall! new playables: $playables")
			return Some(StallInterp.Tempo)

		if fill.nonEmpty || list.exists(o => prev.meta(o).cm && game.common.thoughts(o).possible.length < prev.common.thoughts(o).possible.length) then
			Log.info(s"fill-in stall!")
			return Some(StallInterp.FillIn)

	if severity >= 3 && chop && !game.common.thinksLocked(game, target) then
		if state.deck(focus).id().exists(isSave) then
			Log.info("save that could look like lhs!")
			return Some(StallInterp.SaveLHS)

		Log.info(s"locked hand stall!")
		return Some(StallInterp.Locked)

	if severity == 4 && prev.state.clueTokens == 8 && focusNew && !list.contains(state.hands(target).head) then
		Log.info(s"8 clue stall!")
		return Some(StallInterp.Clues8)

	if severity >= 2 && reclue && fill.isEmpty then
		Log.info(s"hard burn!")
		return Some(StallInterp.Burn)

	None

/** Returns the set of player indices that see an alternative clue. */
def alternativeClue(ctx: ClueContext, maxStall: Int) =
	val ClueContext(prev, game, action) = ctx
	val ClueAction(giver, target, list, clue) = action
	val state = game.state
	val origClue = Clue(clue.kind, clue.value, target)
	val origFocus = ctx.focusResult.focus

	var foundFPE: Option[Identity] = None

	def satisfied(hypo: HGroup, action: ClueAction) =
		val (badTouch, _, _) = badTouchResult(game, hypo, action)
		val (_, playables) = playablesResult(game, hypo)

		hypo.lastMove.get.matchesP:
			case ClueInterp.Play =>
				playables.exists(!state.deck(_).clued) &&
				badTouch.isEmpty &&
				// Can't expect them to clue a possible clued dupe in their hand or our hand
				!playables.forall(state.deck(_).id().exists: id =>
					(state.hands(giver) ++ state.ourHand).exists: o =>
						game.isTouched(o) && game.players(giver).thoughts(o).inferred.contains(id)
				) &&
				!game.findFinesse(action.target).exists: finesse =>
					val fpe =
						game.level >= Level.Stalling &&
						maxStall == 0 &&		// Must have been a 5 Stall
						playables.contains(finesse) &&
						state.clueTokens > 1

					fpe &&
					state.deck(finesse).id().exists: finesseId =>
						foundFPE match
							case Some(id) if id != finesseId =>
								Log.info(s"found FPE previously, not allowing FPE on ${action.target}")
								false
							case Some(_) => true	// Same id
							case _ =>
								Log.info(s"FPE on ${state.names(action.target)}'s ${state.logId(finesseId)}")
								foundFPE = Some(finesseId)
								true

			case ClueInterp.Save =>
				true

			case ClueInterp.Stall =>
				val interp = hypo.stallInterp.get
				interp match
					case StallInterp.Stall5 =>
						hypo.level >= 2 && !hypo.stalled5 && STALL_INDICES(interp) < maxStall

					case i => STALL_INDICES(i) < maxStall

	val seenBy =
		for
			target <- 0 until state.numPlayers if target != giver && target != state.ourPlayerIndex
			clue   <- state.allValidClues(target) if clue != origClue
			list = prev.state.clueTouched(prev.state.hands(target), clue)
			action = ClueAction(giver, target, list, clue.base)
			focus = prev.determineFocus(prev, action).focus if focus != origFocus && !prev.isTouched(focus)
			hypo = prev.copy(allowFindOwn = false, noRecurse = true, assumePlays = false)
				.simulateClue(action) if satisfied(hypo, action)
		yield
			Log.info(s"found alt clue ${clue.fmt(state)} ${hypo.lastMove.get}")
			val newWCs = hypo.waiting.filter: wc =>
				wc.turn == hypo.state.turnCount &&
				wc.connections.forall: conn =>
					// Only count valid wcs based on the new info we have
					conn.ids.exists(game.common.thoughts(conn.order).possible.contains)

			(0 until state.numPlayers).filterNot: p =>
				p == target ||
				newWCs.exists:
					_.connections.exists: conn =>
						conn.kind != "known" && p == state.holderOf(conn.order)

	seenBy.foldLeft((0 until state.numPlayers).toSet)(_ -- _)

def stallingSituation(ctx: ClueContext): Option[(StallInterp, Set[Int])] =
	val ClueContext(prev, game, action) = ctx
	val ClueAction(giver, target, list, clue) = ctx.action

	val severity = stallSeverity(ctx.prev, ctx.prev.common, giver, infoPlayer = Some(ctx.game.common))
	Log.info(s"severity $severity")

	lazy val giverLoaded = prev.common.thinksPlayables(prev, giver, assume = false).nonEmpty ||
		(prev.common.thinksTrash(prev, giver).nonEmpty && prev.state.clueTokens < 8)

	if severity == 0 then
		None

	else if giverLoaded then
		Log.info(s"giver loaded ${prev.common.thinksPlayables(prev, giver)}, not stall!")
		None

	else
		isStall(ctx, severity).map: stall =>
			if game.noRecurse then
				(stall, (0 until game.state.numPlayers).toSet)
			else
				val thinksStall = alternativeClue(ctx, STALL_TO_SEVERITY(stall))
				(stall, thinksStall)
