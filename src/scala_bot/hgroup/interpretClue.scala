package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.utils._
import scala_bot.logger.Log

import scala.util.chaining.scalaUtilChainingOps

case class ClueContext(prev: HGroup, game: HGroup, action: ClueAction):
	inline def common = game.common
	inline def state = game.state

	lazy val focusResult = game.determineFocus(prev, action)

def interpClue(ctx: ClueContext): HGroup =
	val ClueContext(prev, game, action) = ctx
	val (common, state) = (game.common, game.state)
	val ClueAction(giver, target, list, clue) = action

	val FocusResult(focus, chop, positional) = ctx.focusResult
	checkFix(prev, game, action) match
		case FixResult.Normal(cluedResets, duplicateReveals) =>
			Log.info(s"fix clue! not inferring anything else $cluedResets $duplicateReveals")

			lazy val oldOrdered1s = prev.order1s(list.filter(prev.unknown1), noFilter = true)
			val pinkFix1s = state.includesVariant(PINKISH) &&
				clue.kind == ClueKind.Rank && clue.value != 1 &&
				oldOrdered1s.nonEmpty

			return game.when (_ => pinkFix1s) { g =>
				val fixedOrder = oldOrdered1s.head

				if chop && (clue.value == 2 || clue.value == 5) then
					Log.info(s"pink fix!")
					g.withThought(fixedOrder)(t => t.copy(
						inferred = t.possible.difference(state.playableSet)
					))
				else
					Log.info(s"pink fix promise!")
					g.withThought(fixedOrder)(t => t.copy(
						inferred = t.inferred.filter(i => i.rank == clue.value && !state.isPlayable(i))
					))
			}
			.copy(lastMove = Some(ClueInterp.Fix))

		case FixResult.NoNewInfo(fixes) =>
			Log.info("no info fix clue! not inferring anything else")

			val fixTarget = Option.when (clue == BaseClue(ClueKind.Rank, 1)):
				prev.order1s(list.filter(prev.unknown1), noFilter = true).headOption
			.flatten.getOrElse(focus)

			return game
				.withThought(fixTarget)(t => t.copy(inferred = t.possible.intersect(state.trashSet)))
				.withMeta(fixTarget)(_.copy(trash = true))
				.copy(lastMove = Some(ClueInterp.Fix))

		case _ => ()

	val stall = stallingSituation(ctx)

	if stall.isDefined then
		val (interp, thinksStall) = stall.get

		if thinksStall.size > 0 && thinksStall.size < state.numPlayers then
			Log.warn(s"asymmetric! only ${thinksStall.map(state.names)} think stall")
			// return game.copy(lastMove = Some(ClueInterp.Mistake))

		else if thinksStall.size == state.numPlayers then
			Log.info(s"stalling situation $interp")

			return game
				.when(g => interp == StallInterp.Stall5 && g.inEarlyGame):
					_.copy(stalled5 = true)
				// Pink promise on stalls
				.when(g => g.state.includesVariant(PINKISH) && clue.kind == ClueKind.Rank):
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
		def badCM(chopMoved: Seq[Int]) =
			game.chop(target).exists: oldChop =>
				state.deck(oldChop).id().exists: chopId =>
					state.isBasicTrash(chopId) ||
					chopMoved.exists(o => o != oldChop && state.deck(o).id().exists(_.matches(chopId))) ||
					// Could be directly clued
					state.allValidClues(target).exists: clue =>
						val directList = state.clueTouched(state.hands(target), clue)

						// Clue must touch all non-trash CM'd cards
						chopMoved.forall: o =>
							state.deck(o).id().exists(state.isBasicTrash) ||
							directList.contains(o)
						&&
						// Must have no bad touch
						!directList.exists: o =>
							!prev.state.deck(o).clued &&
							state.deck(o).id().exists(state.isBasicTrash)
						&&
						// Clue must be valid
						prev.copy(noRecurse = true).simulateClue(clueToAction(prev.state, clue, giver)).lastMove != Some(ClueInterp.Mistake)

		val tcm = interpretTcm(ctx)

		if tcm.isDefined then
			// All newly cards are trash
			return list.foldLeft(game): (acc, order) =>
				if prev.state.deck(order).clued then acc else
					acc.withThought(order): t =>
						val newInferred = t.possible.intersect(state.trashSet)
						t.copy(
							inferred = newInferred,
							infoLock = newInferred.toOpt
						)
					.withMeta(order)(_.copy(trash = true))
			.pipe(performCM(_, tcm.get))
			.copy(lastMove = Some(
				if badCM(tcm.get) then ClueInterp.Mistake else ClueInterp.Discard
			))

		val cm5 = interpret5cm(ctx)

		if cm5.isDefined then
			return performCM(game, cm5.get)
				.copy(lastMove = Some(
					if badCM(cm5.get) then ClueInterp.Mistake else ClueInterp.Discard
				))

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

	def validSave(inf: Identity) =
		!state.isBasicTrash(inf) &&
		visibleFind(state, common, inf, infer = true, excludeOrder = focus).isEmpty &&
		(if clue.kind == ClueKind.Colour then
			colourSave(prev, action, inf, focus)
		else
			rankSave(prev, action, inf, focus))

	val savePoss = if !chop then List.empty else
		for
			inf <- common.thoughts(focus).inferred if validSave(inf)
		yield
			FocusPossibility(inf, List(), ClueInterp.Save)

	if savePoss.nonEmpty then
		Log.info(s"found saves: [${savePoss.map(fp => state.logId(fp.id)).mkString(",")}]")

	val thinksStall = stall.map(_._2).getOrElse(Set.empty)
	val focusPoss =
		val looksDirect = common.thoughts(focus).id(symmetric = true).isEmpty &&
			(action.clue.kind == ClueKind.Colour || savePoss.nonEmpty || positional)

		common.thoughts(focus).inferred.filter: inf =>
			!state.isBasicTrash(inf) &&
			visibleFind(state, common, inf, infer = true, excludeOrder = focus).isEmpty &&
			!savePoss.exists(_.id == inf)
		.flatMap:
			connect(ctx, _, looksDirect, thinksStall)

	val simplest =
		val possible = (savePoss ++ focusPoss)
			.filter(fp => game.players(target).thoughts(focus).possible.contains(fp.id))

		occamsRazor(state, possible, target)

	val noSelf = !game.allowFindOwn ||
		giver == state.ourPlayerIndex ||
		simplest.exists(fp => state.deck(focus).matches(fp.id))

	if noSelf then
		Log.info(s"simplest focus possibilities [${simplest.map(fp => state.logId(fp.id)).mkString(",")}]")
		resolveClue(ctx, simplest)
	else
		Log.highlight(Console.GREEN, s"finding own!")

		val ownFps =
			val looksDirect = common.thoughts(focus).id(symmetric = true).isEmpty && {
				clue.kind == ClueKind.Colour ||
				// Looks like an existing possibility
				focusPoss.exists:
					_.connections.forall: c =>
						c.isInstanceOf[KnownConn] ||
						(c.isInstanceOf[PlayableConn] && c.reacting != state.ourPlayerIndex)
			}

			common.thoughts(focus).inferred.filter: inf =>
				!state.isBasicTrash(inf) &&
				visibleFind(state, common, inf, infer = true, excludeOrder = focus).isEmpty &&
				!(savePoss.exists(_.id == inf) || simplest.exists(_.id == inf))
			.flatMap:
				connect(ctx, _, looksDirect, thinksStall, findOwn = Some(state.ourPlayerIndex))

		val simplestOwn = occamsRazor(state, simplest ++ ownFps, state.ourPlayerIndex, game.me.thoughts(focus).id())
		resolveClue(ctx, simplestOwn, ownFps.filterNot(simplestOwn.contains))
