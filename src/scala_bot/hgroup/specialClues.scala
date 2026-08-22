package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.lib.FastBitSet

import scala_bot.utils._
import scala_bot.logger.Log

import SpecialClueResult._

def validBluff(game: HGroup, action: ClueAction, blind: Identity, truth: Identity, reacting: Int, connected: FastBitSet, symmetric: Boolean = false) =
	val state = game.state
	val ClueAction(giver, target, _, clue) = action
	val focus = connected.head

	lazy val disconnect = symmetric ||
		(clue.kind == ClueKind.Rank && clue.value != blind.rank + 1) ||
		blind.next.forall(!game.common.thoughts(focus).possible.contains(_))

	lazy val interferes = state.hands(reacting).find: o =>
		game.players(reacting).thoughts(o).possible.contains(truth) &&
		(game.isBlindPlaying(o) || game.meta(o).status == CardStatus.GentlemansDiscard)

	game.level >= Level.Bluffs &&
	state.nextPlayerIndex(giver) == reacting &&
	connected.size == 1 &&
	disconnect &&
	!(clue.kind == ClueKind.Colour && reacting == target) &&	// not self-colour bluff
	interferes.isEmpty

/** Returns whether the colour clue could be a save on the given identity. */
def colourSave(prev: HGroup, action: ClueAction, id: Identity, focus: Int): Boolean =
	val state = prev.state
	val ClueAction(giver, target, list, clue) = action
	val Identity(suitIndex, rank) = id

	val thought = prev.common.thoughts(focus)
	val suit = state.variant.suits(suitIndex)

	if !state.variant.cardTouched(id, clue) || !thought.possible.contains(id) || state.isBasicTrash(id) then
		return false

	if rank == 5 && suit.name != "Black" && !suit.suitType.brownish then
		return false

	if state.variant.criticalRank.contains(rank) then
		return false

	if suit.name == "Black" && (rank == 2 || rank == 5) then
		// Newly touched or fill-in cards
		val fillIns = list.count: o =>
			!state.deck(o).clued ||
			prev.common.thoughts(o).possible.exists(!state.variant.idTouched(_, clue))

		// Trash that would be picked up by a rank clue
		val trash = state.hands(target).count: o =>
			val card = state.deck(o)
			!card.clued && card.id().exists(i => i.rank == rank && state.isBasicTrash(i))

		if fillIns < 2 && trash == 0 then
			return false

	if suit.suitType.brownish && prev.common.thinksLoaded(prev, target) then
		return false

	if "Dark Rainbow|Dark Prism".r.unanchored.matches(suit.name) then
		val completed = prev.common.hypoStacks(suitIndex) == state.maxRanks(suitIndex)
		val savedCrit = list.exists: o =>
			val card = state.deck(o)

			o != focus &&
			!card.clued && card.id().exists: i =>
				state.isCritical(i) &&
				i.rank != 5 &&
				!DARK.matches(state.variant.suits(i.suitIndex).name)

		if !completed && !savedCrit then
			return false

	// If there is a dark colour, save with that, otherwise red.
	val muddySaveColour = if !state.includesVariant("Black|Dark Brown|Dark Pink".r) then 0 else
		state.variant.suits.length - 2

	// Note that critical 2,3,4 can be saved with anything.
	if suit.name.contains("Muddy") && clue.value != muddySaveColour && !(state.isCritical(id) && Set(2,3,4).contains(rank)) then
		return false

	if suit.name.contains("Cocoa") && clue.value != muddySaveColour then
		return false

	if suit.suitType.inverted && suit.suitType.dark && (rank == 1 || rank == 2 || rank == 5) then
		return false

	state.isCritical(id) ||
	(suit.suitType.brownish && rank == 2) ||
	(suit.suitType.inverted && (rank == 3 || rank == 4))

def rankSave(prev: HGroup, action: ClueAction, id: Identity, focus: Int): Boolean =
	val state = prev.state
	val ClueAction(giver, target, list, clue) = action
	val Identity(suitIndex, rank) = id
	val thought = prev.common.thoughts(focus)

	if !thought.possible.contains(id) || state.isBasicTrash(id) || state.deck(focus).id().exists(_.rank != rank) then
		return false

	// Don't consider save on k3,k4 (or dark i3,i4) with rank
	// TODO: Florrat Save
	if "Black|Dark Pink".r.unanchored.matches(state.variant.suits(suitIndex).name) && (rank == 3 || rank == 4) then
		return false

	val loaded34 = prev.common.thinksLoaded(prev, target) &&
		(state.variant.suits.exists(s => s.suitType.whitish && !s.suitType.brownish) || state.includesVariant("Dark Rainbow|Dark Prism".r)) &&
		(rank == 3 || rank == 4)

	if loaded34 then
		return false

	// Breaks pink promise
	if state.variant.pinkish && clue.kind == ClueKind.Rank && rank != clue.value then
		return false

	state.isCritical(id) || rank == 2

enum SymmetricInterp:
	case NoInterp
	case Stall(interp: StallInterp)

enum SpecialClueResult:
	case SpecialClue(newGame: HGroup)
	case NoInterp(interp: SymmetricInterp, thinksStall: FastBitSet)

def interpSpecialClue(ctx: ClueContext): SpecialClueResult =
	val ClueContext(prev, game, action, _) = ctx
	val (common, state) = (game.common, game.state)
	val ClueAction(giver, target, list, clue) = action
	val FocusResult(focus, chop, positional) = ctx.focusResult

	val USELESS = SpecialClue(game.withMove(ClueInterp.Useless))
	val MISTAKE = SpecialClue(game.withMove(ClueInterp.Mistake))

	checkHFix(ctx) match
		case Some(newGame) => return SpecialClue(newGame)
		case _ => ()

	val stall = stallingSituation(ctx)
	val thinksStall = stall.map(_._2).getOrElse(FastBitSet.empty)
	val asymmetricStall = thinksStall.nonEmpty && thinksStall.size < state.numPlayers

	if stall.isDefined then
		val (interp, thinksStall) = stall.get

		if asymmetricStall then
			Log.warn(s"asymmetric! only ${thinksStall.map(state.names)} think stall")

		else if thinksStall.size == state.numPlayers then
			Log.info(s"stalling situation $interp")

			return SpecialClue:
				game.when(_.inEarlyGame && interp == StallInterp.Stall5):
					_.copy(stalled5 = true)
				// Pink promise on stalls
				.when(_.state.variant.pinkish && clue.kind == ClueKind.Rank):
					_.withThought(focus)(t => t.copy(inferred = t.inferred.filter(_.rank == clue.value)))
				.copy(stallInterp = Some(interp))
				.withMove(ClueInterp.Stall)

	distributionClue(prev, game, action, focus) match
		case None => ()
		case Some(ids) =>
			Log.info(s"distribution clue!")

			return SpecialClue:
				game.withThought(focus): t =>
					t.copy(
						inferred = t.possible.intersect(ids),
						infoLock = t.possible.intersect(ids).toOpt,
						reset = false
					)
				.withMeta(focus)(_.copy(focused = true))
				.withMove(ClueInterp.Distribution)

	if game.level >= Level.BasicCM then
		def saveCtx(g: HGroup) =
			g.copy(savedCtx = g.savedCtx.updated(giver, Some(ctx.copy(
				prev = prev.copy(savedCtx = Vector.fill(state.numPlayers)(None)),
				game = game.copy(savedCtx = Vector.fill(state.numPlayers)(None))
			))))

		interpretTcm(ctx) match
			case None => ()
			case Some(tcm) =>
				if game.inEndgame then
					return USELESS
				else
					return SpecialClue:
						handleTcm(ctx, tcm, notStall = thinksStall.isEmpty).pipe(saveCtx)

		interpret5cm(ctx) match
			case Result5cm.Mistake => return MISTAKE
			case Result5cm.Cm(cm5) if !thinksStall.contains(target) =>
				Log.info(s"5cm, saving ${state.logId(cm5)} $cm5")
				return SpecialClue:
					performCM(game, List(cm5)).withMove(evaluateCM(ctx, List(cm5))).pipe(saveCtx)
			case _ => ()

	val pinkTrashFix = state.variant.pinkish &&
		!positional.contains(Positional.Pink) && clue.kind == ClueKind.Rank &&
		list.forall(o => prev.state.deck(o).clued && game.knownAs(o, PINKISH)) &&
		state.variant.suits.zipWithIndex.forall: (suit, suitIndex) =>
			!suit.suitType.pinkish ||
			common.isTrash(game, Identity(suitIndex, clue.value), focus)
		&&
		game.common.thoughts(focus).possible.difference(state.criticalSet).exists:
			common.isTrash(game, _, focus)

	if pinkTrashFix then
		Log.info(s"pink trash fix!")

		if prev.meta(focus).trash then
			Log.warn("nonsensical burn!")
			return USELESS

		return SpecialClue:
			game.withThought(focus): t =>
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
			.withMove:
				if giver == state.ourPlayerIndex && !game.me.isTrash(game, state.deck(focus).id().get, focus) then
					ClueInterp.Mistake
				else
					ClueInterp.Fix

	val specialOrangeSituation = state.variant.inverted &&
		clue.kind == ClueKind.Colour &&
		state.variant.colourableSuits(clue.value).suitType.inverted

	if specialOrangeSituation then
		val chopFix = chop && state.deck(focus).id().exists(id => state.isBasicTrash(id) || (id.rank == 3 || id.rank == 4))

		if chopFix then
			val poss = game.common.thoughts(focus).possible.filter: id =>
				!state.isPlayable(id) &&
				(state.isBasicTrash(id) || (id.rank != 2 && id.rank != 5))

			if poss.forall(state.isBasicTrash) then
				Log.info(s"orange fix on $focus! (trash)")

				if giver == state.ourPlayerIndex && state.deck(focus).id().exists(state.isUseful) then
					Log.error("mistake!")
					return MISTAKE

				return SpecialClue:
					game.withThought(focus)(_.copy(inferred = IdentitySet.empty))
						.withMeta(focus)(_.copy(trash = true))
						.withMove(ClueInterp.Reveal)
			else
				Log.info(s"orange fix on $focus! ${poss.fmt(state)}")

				if giver == state.ourPlayerIndex && state.deck(focus).id().exists(!poss.contains(_)) then
					Log.error("mistake!")
					return MISTAKE

				return SpecialClue:
					game.withThought(focus)(_.copy(inferred = poss))
						.withMove(ClueInterp.Reveal)

		val playFix = list.find: o =>
			prev.common.orderPlayable(prev, o) &&
			!prev.common.thinksInverted(prev.state, o) &&
			state.isInverted(o)

		if playFix.isDefined then
			Log.info(s"orange play fix! ${playFix.get}")
			return SpecialClue(game.withMove(ClueInterp.Fix))

	if prev.state.deck(focus).clued && positional.isEmpty then
		if game.level >= Level.Fix then
			val fixTarget = Option.when(clue == BaseClue(ClueKind.Rank, 1)):
				prev.next1(list.filter(prev.unknown1))
			.flatten.getOrElse(focus)

			if prev.common.hypoPlays.contains(fixTarget) && common.thoughts(fixTarget).possible.intersect(state.trashSet).nonEmpty then
				Log.info(s"no info fix clue on $fixTarget! not inferring anything else")

				val badFix = giver == state.ourPlayerIndex && !game.me.orderTrash(game, fixTarget)

				return SpecialClue:
					game.withThought(fixTarget)(t => t.copy(inferred = t.possible.intersect(state.trashSet)))
						.withMeta(fixTarget)(_.copy(trash = true))
						.withMove(if badFix then ClueInterp.Mistake else ClueInterp.Fix)

		val uselessReclue =
			prev.common.hypoPlays.contains(focus) ||
			game.me.thoughts(focus).id().exists: id =>
				prev.common.hypoStacks(id.suitIndex) >= id.rank

		if uselessReclue then
			Log.warn("nonsensical burn!")
			return USELESS

	if prev.meta(focus).status == CardStatus.GentlemansDiscard then
		Log.warn("nonsensical burn on gd!")
		return USELESS

	if game.level <= Level.TrashMoves && chop && game.common.orderKt(game, focus) then
		Log.warn("out-of-level trash push! interpreting burn")
		return USELESS

	if asymmetricStall then
		NoInterp(SymmetricInterp.Stall(stall.get._1), thinksStall)
	else
		NoInterp(SymmetricInterp.NoInterp, thinksStall)
