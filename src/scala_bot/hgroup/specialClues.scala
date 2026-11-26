package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.lib.FastBitSet

import scala_bot.utils._
import scala_bot.logger.Log

import SpecialClueResult._

def isIntermediateBluffTarget(prev: HGroup, game: HGroup, action: ClueAction, id: Identity, focus: Int): Boolean =
	val state = game.state
	val clue = action.clue

	game.level >= Level.IntermediateBluffs && (
		id.rank == 3 ||
		(clue.kind == ClueKind.Colour && !prev.state.deck(focus).clued && id.rank != 5 && state.isCritical(id))
	)

def validBluff(game: HGroup, action: ClueAction, blind: Identity, reacting: Int, connected: FastBitSet, symmetric: Boolean = false) =
	val state = game.state
	val ClueAction(giver, receiver, _, clue) = action
	val focus = connected.head

	lazy val disconnect = symmetric ||
		(if clue.kind == ClueKind.Rank then
			clue.value != blind.rank + 1 ||
			blind.next.forall(!game.common.thoughts(focus).possible.contains(_))
		else
			!game.common.thoughts(focus).possible.exists(i => i.suitIndex == blind.suitIndex && i.rank > blind.rank))

	val interferes = state.hands(reacting).find: o =>
		// game.players(reacting).thoughts(o).possible.contains(truth) &&	// any f/gd should block a bluff
		(game.isBlindPlaying(o) || game.meta(o).status == CardStatus.GentlemansDiscard)

	game.level >= Level.Bluffs &&
	state.nextPlayerIndex(giver) == reacting &&
	connected.size == 1 &&
	disconnect &&
	!(clue.kind == ClueKind.Colour && reacting == receiver) &&	// not self-colour bluff
	interferes.isEmpty

enum SaveResult:
	case Invalid
	case Unconditional
	case Conditional(link: SaveLink)

	def isValid: Boolean = this != SaveResult.Invalid

def checkSave(ctx: ClueContext, id: Identity, player: Player): SaveResult =
	val state = ctx.prev.state
	val focus = ctx.focusResult.focus

	if !state.isUseful(id) || visibleFind(state, ctx.common, id, infer = true, excludeOrder = focus).nonEmpty then
		SaveResult.Invalid
	else if ctx.action.clue.kind == ClueKind.Colour then
		colourSave(ctx, id, player)
	else
		rankSave(ctx, id, player)

/** Returns whether the colour clue could be a save on the given identity. */
def colourSave(ctx: ClueContext, id: Identity, player: Player): SaveResult =
	val ClueContext(prev, game, action, _) = ctx
	val state = prev.state
	val ClueAction(giver, receiver, orders, clue) = action
	val focus = ctx.focusResult.focus
	val Identity(suitIndex, rank) = id

	val thought = prev.common.thoughts(focus)
	val suit = state.variant.suits(suitIndex)

	if !state.variant.cardTouched(id, clue) || !thought.possible.contains(id) || state.isBasicTrash(id) || state.isPlayable(id) then
		return SaveResult.Invalid

	if rank == 5 && suit.name != "Black" && !suit.suitType.brownish then
		return SaveResult.Invalid

	if state.variant.criticalRank.contains(rank) then
		return SaveResult.Invalid

	var saveLink: Option[SaveLink] = None

	if suit.name == "Black" && (rank == 2 || rank == 5) then
		val rankClueUnavailable = state.variant.specialRank.contains(rank) &&
			(state.variant.pinkS || state.variant.brownS || state.variant.deceptiveS)

		// Exception 3: filled in an ancillary card
		val filledIn = orders.exists: o =>
			o != focus &&
			state.deck(o).clued &&
			!prev.common.orderKt(prev, o) &&
			prev.common.thoughts(o).possible.exists(!state.variant.idTouched(_, clue))

		// Exception 4: gave important negative information to one or more clued cards in hand
		val importantNegInfo = state.hands(receiver).exists: o =>
			!orders.contains(o) &&
			state.deck(o).clued &&
			!prev.common.orderKt(prev, o) &&
			prev.common.thoughts(o).inferred.exists(state.variant.idTouched(_, clue)) && {
				val newlyIdentified = prev.common.thoughts(o).inferred.length > 1 && game.common.thoughts(o).inferred.length == 1
				val newlyPlayable = !prev.common.orderPlayable(prev, o) && game.common.orderPlayable(game, o)
				val newlyTrash = !prev.common.orderTrash(prev, o) && game.common.orderTrash(game, o)
				newlyIdentified || newlyPlayable || newlyTrash
			}

		// Exception 1: touched two or more previously unclued black cards or critical Rainbow cards
		val extraNewlyTouched = FastBitSet.from(orders.filter(o => o != focus && !state.deck(o).clued))
		val critRainbowIds = state.allIds.filter: i =>
			i != id &&
			state.variant.idTouched(i, clue) &&
			state.isUseful(i) && {
				val s = state.variant.suits(i.suitIndex)
				s.name == "Black" ||
				((s.suitType.rainbowish || (state.variant.rainbowS && state.variant.specialRank.contains(i.rank))) && state.isCritical(i))
			}

		val unconditionalMulti = extraNewlyTouched.nonEmpty && (
			!state.variant.rainbowish ||
			extraNewlyTouched.exists(o => prev.common.thoughts(o).possible.forall(critRainbowIds.contains))
		)

		if !rankClueUnavailable && !filledIn && !importantNegInfo && !unconditionalMulti then
			// Exception 2: required to avoid violating Good Touch Principle (and/or conditional critical rainbow)
			val rankClue = BaseClue(ClueKind.Rank, rank)
			val uncluedOthers = FastBitSet.from(state.hands(receiver).filter(o => o != focus && !state.deck(o).clued))
			val rankTouchedIds = state.allIds.filter(state.variant.idTouched(_, rankClue))
			val trashIds = rankTouchedIds.intersect(state.trashSet)
			val usefulIds = rankTouchedIds.difference(state.trashSet).difference(id).filter(i => state.cardCount(i.toOrd) >= 2)
			val touchedAtClue = state.heldOrders.filter: o =>
				o != focus &&
				prev.isTouched(o) &&
				!prev.meta(o).trash &&
				prev.common.thoughts(o).possibilities.intersect(usefulIds).nonEmpty

			val link = SaveLink(
				focus = focus,
				id = id,
				singles = List((uncluedOthers, trashIds), (extraNewlyTouched, critRainbowIds)).filter((os, is) => os.nonEmpty && is.nonEmpty),
				dupe = Option.when(uncluedOthers.nonEmpty && usefulIds.nonEmpty)((uncluedOthers, touchedAtClue, usefulIds))
			)

			if !link.isPossible(player, state) then
				return SaveResult.Invalid

			saveLink = Some(link)

	if suit.suitType.brownish && prev.common.thinksLoaded(prev, receiver) then
		return SaveResult.Invalid

	if "Dark Rainbow|Dark Prism".r.unanchored.matches(suit.name) then
		val completed = prev.common.hypoStacks(suitIndex) == state.maxRanks(suitIndex)
		val savedCrit = orders.exists: o =>
			val card = state.deck(o)

			o != focus &&
			!card.clued && card.id().exists: i =>
				state.isCritical(i) &&
				i.rank != 5 &&
				!DARK.matches(state.variant.suits(i.suitIndex).name)

		if !completed && !savedCrit then
			return SaveResult.Invalid

	// If there is a dark colour, save with that, otherwise red.
	val muddySaveColour = if !state.includesVariant("Black|Dark Brown|Dark Pink".r) then 0 else
		state.variant.suits.length - 2

	// Note that critical 2,3,4 can be saved with anything.
	if suit.name.contains("Muddy") && clue.value != muddySaveColour && !(state.isCritical(id) && Set(2,3,4).contains(rank)) then
		return SaveResult.Invalid

	if suit.name.contains("Cocoa") && clue.value != muddySaveColour then
		return SaveResult.Invalid

	if suit.suitType.inverted && suit.suitType.dark && (rank == 1 || rank == 2 || rank == 5) then
		return SaveResult.Invalid

	val valid = state.isCritical(id) ||
		(suit.suitType.brownish && rank == 2) ||
		(suit.suitType.inverted && (rank == 3 || rank == 4))

	if !valid then
		SaveResult.Invalid
	else
		saveLink.fold(SaveResult.Unconditional)(SaveResult.Conditional(_))

def rankSave(ctx: ClueContext, id: Identity, player: Player): SaveResult =
	val ClueContext(prev, _, action, _) = ctx
	val state = prev.state
	val ClueAction(giver, receiver, orders, clue) = action
	val focus = ctx.focusResult.focus
	val Identity(suitIndex, rank) = id
	val thought = prev.common.thoughts(focus)

	if !thought.possible.contains(id) || state.isBasicTrash(id) || state.deck(focus).id().exists(_.rank != rank) || state.isPlayable(id) then
		return SaveResult.Invalid

	var saveLink: Option[SaveLink] = None

	// Don't consider save on k3,k4 (or dark i3,i4) with rank unless it's a Florrat Save
	if "Black|Dark Pink".r.unanchored.matches(state.variant.suits(suitIndex).name) && (rank == 3 || rank == 4) then
		val extraNewlyTouched = FastBitSet.from(orders.filter(o => o != focus && !state.deck(o).clued))
		val critIds = state.criticalSet.filter(i => i != id && state.variant.idTouched(i, clue))

		if extraNewlyTouched.isEmpty || critIds.isEmpty then
			return SaveResult.Invalid

		val link = SaveLink(focus, id, singles = List((extraNewlyTouched, critIds)))
		if !link.isPossible(player, state) then
			return SaveResult.Invalid

		saveLink = Some(link)

	val loaded34 = prev.common.thinksLoaded(prev, receiver) &&
		(state.variant.suits.exists(s => s.suitType.whitish && !s.suitType.brownish) || state.includesVariant("Dark Rainbow|Dark Prism".r)) &&
		(rank == 3 || rank == 4)

	if loaded34 then
		return SaveResult.Invalid

	// Breaks pink promise
	if state.variant.pinkish && clue.kind == ClueKind.Rank && rank != clue.value then
		return SaveResult.Invalid

	val valid = (state.isCritical(id) && rank != 1) || rank == 2

	if !valid then
		SaveResult.Invalid
	else
		saveLink.fold(SaveResult.Unconditional)(SaveResult.Conditional(_))

enum SymmetricInterp:
	case NoInterp
	case Stall(interp: StallInterp)

enum SpecialClueResult:
	case SpecialClue(newGame: HGroup)
	case NoInterp(interp: SymmetricInterp, thinksStall: FastBitSet)

def interpSpecialClue(ctx: ClueContext): SpecialClueResult =
	val ClueContext(prev, game, action, _) = ctx
	val (common, state) = (game.common, game.state)
	val ClueAction(giver, receiver, orders, clue) = action
	val FocusResult(focus, chop, positional) = ctx.focusResult

	val USELESS = SpecialClue(game.withMove(ClueInterp.Useless))
	val MISTAKE = SpecialClue(game.withMove(ClueInterp.Mistake))

	checkHFix(ctx) match
		case Some(newGame) => return SpecialClue(newGame)
		case _ => ()

	val stall = stallingSituation(ctx)
	val thinksStall = stall.map(_._2).getOrElse(FastBitSet.empty)
	val asymmetricStall =
		thinksStall.nonEmpty &&
		thinksStall.size < state.numPlayers &&
		!thinksStall.contains(giver)		// it's always a valid stall if the giver thinks it's a stall

	if stall.isDefined then
		val (interp, thinksStall) = stall.get

		if asymmetricStall then
			Log.warn(s"asymmetric! only ${thinksStall.map(state.names)} think stall")

		else if thinksStall.nonEmpty then
			Log.info(s"stalling situation $interp")

			return SpecialClue:
				game.when(_.inEarlyGame && interp == StallInterp.Stall5):
					_.copy(stalled5 = true)
				// Pink promise on stalls
				.when(_.state.variant.pinkish && clue.kind == ClueKind.Rank):
					_.withThought(focus)(t => t.copy(inferred = t.inferred.filter(_.rank == clue.value)))
				.copy(stallInterp = Some(interp))
				.withMove(if interp == StallInterp.SaveLHS then ClueInterp.Save else ClueInterp.Stall)

	def validDistribution(ids: IdentitySet) =
		game.me.thoughts(focus).id(infer = true) match
			case Some(id) => ids.contains(id)
			case None     => game.me.thoughts(focus).inferred.forall(ids.contains)

	distributionClue(prev, game, action, focus) match
		case Some(ids) if validDistribution(ids) =>
			Log.info(s"distribution clue for [${ids.fmt(state)}]! ${game.me.strInfs(state, focus)}")

			return SpecialClue:
				game.withThought(focus): t =>
					t.copy(
						inferred = t.possible.intersect(ids),
						infoLock = t.possible.intersect(ids).toOpt,
						reset = false
					)
				.withMeta(focus)(_.copy(focused = true))
				.withMove(ClueInterp.Distribution)
		case _ => ()

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
			case Result5cm.Cm(cm5) if !thinksStall.contains(receiver) =>
				Log.info(s"5cm, saving ${state.logId(cm5)} $cm5")
				return SpecialClue:
					performCM(game, List(cm5)).withMove(evaluateCM(ctx, List(cm5))).pipe(saveCtx)
			case _ => ()

	val pinkTrashFix = state.variant.pinkish &&
		!positional.contains(Positional.Pink) && clue.kind == ClueKind.Rank &&
		orders.forall(o => prev.state.deck(o).clued && game.knownAs(o, PINKISH)) &&
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
		val chopFix = chop && state.deck(focus).id().exists: id =>
			state.isBasicTrash(id) ||
			(id.rank == 3 || id.rank == 4) ||
			visibleFind(state, common, id, infer = true, excludeOrder = focus).exists: o =>
				game.me.thoughts(o).matches(id) ||
				(game.me.thoughts(o).matches(id, assume = true) && game.me.thoughts(o).possible.contains(id))

		if chopFix then
			val poss = game.common.thoughts(focus).possible.filter: id =>
				!state.isPlayable(id) &&
				state.isInverted(id) &&
				(state.isBasicTrash(id) ||
					(id.rank != 2 && id.rank != 5) ||
					visibleFind(state, common, id, infer = true, excludeOrder = focus).exists: o =>
						game.me.thoughts(o).matches(id) ||
						(game.me.thoughts(o).matches(id, assume = true) && game.me.thoughts(o).possible.contains(id))
				)

			if poss.forall(state.isBasicTrash) then
				Log.info(s"orange fix on $focus! (trash, ${poss.fmt(state)})")

				if giver == state.ourPlayerIndex && state.deck(focus).id().exists(state.isUseful) then
					Log.error("mistake!")
					return MISTAKE

				return SpecialClue:
					game.withThought(focus)(_.copy(inferred = poss, reset = true))
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

		val playFix = orders.find: o =>
			prev.common.orderPlayable(prev, o) &&
			!prev.common.thinksInverted(prev.state, o) &&
			state.isInverted(o)

		if playFix.isDefined then
			Log.info(s"orange play fix! ${playFix.get}")
			return SpecialClue(game.withMove(ClueInterp.Fix))

	if prev.state.deck(focus).clued && positional.isEmpty then
		if game.level >= Level.Fix then
			val fixTarget = Option.when(clue == BaseClue(ClueKind.Rank, 1)):
				prev.next1(orders.filter(prev.unknown1))
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
