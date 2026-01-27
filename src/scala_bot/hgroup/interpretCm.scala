package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.logger.Log

/**
  * Checks whether a Trash Chop Move was performed.
  * Returns the orders of any chop moved cards.
 */
def interpretTcm(ctx: ClueContext): Option[Seq[Int]] =
	val ClueContext(prev, game, action) = ctx
	val state = ctx.state
	val ClueAction(_, target, list, clue) = action
	val focus = ctx.focusResult.focus
	val thought = ctx.common.thoughts(focus)

	lazy val promisedIds = if clue.kind == ClueKind.Rank then
		thought.possible.filter(_.rank == clue.value)
	else
		thought.possible

	val notTcm = prev.state.deck(focus).clued ||
		!promisedIds.forall(game.common.isTrash(game, _, focus)) ||
		thought.inferred.forall(i => state.isPlayable(i) && !game.common.isTrash(game, i, focus))

	if notTcm then
		return None

	val oldestTrash = list.filter(!prev.state.deck(_).clued).min

	val cmOrders = state.hands(target).filter{ o =>
		o < oldestTrash && !state.deck(o).clued && !game.meta(o).cm
	}

	if cmOrders.isEmpty then
		Log.highlight(Console.CYAN, s"no cards to tcm")
		None
	else
		Log.highlight(Console.CYAN, s"trash chop move on ${cmOrders.map(state.logId)}")
		Some(cmOrders)

/**
  * Checks whether a 5's Chop Move was performed.
  * Returns the orders of any chop moved cards.
 */
def interpret5cm(ctx: ClueContext): Option[Vector[Int]] =
	val ClueContext(prev, game, action) = ctx
	val state = game.state
	val ClueAction(_, target, list, clue) = action
	val focus = ctx.focusResult.focus
	val chop = prev.chop(target)

	val not5cm = clue != BaseClue(ClueKind.Rank, 5) ||
		prev.state.deck(focus).clued ||
		game.inEarlyGame ||
		chop.isEmpty

	if not5cm then
		return None

	list.filter(o => o > chop.get && !prev.state.deck(o).clued).minOption.flatMap { oldest5 =>
		val distance = prev.chopDistance(target, oldest5)

		if distance != 1 then
			Log.info(s"rightmost 5 was clued $distance-away from chop, not 5cm!")
			None

		else if game.common.orderKt(game, chop.get) then
			Log.info(s"saved card $chop has only trash possibilities!")
			None

		else
			Log.info(s"5cm, saving ${state.logId(chop.get)} ${chop.get}")
			Some(Vector(chop.get))
	}

def interpretOcm(prev: HGroup, action: PlayAction | DiscardAction) =
	val state = prev.state
	val (playerIndex, order) = action match
		case PlayAction(p, o, _, _) => (p, o)
		case DiscardAction(p, o, _, _, _) => (p, o)

	val ordered1s = prev.order1s(state.hands(playerIndex))
	val offset = ordered1s.indexOf(order)
	val target = (playerIndex + offset) % state.numPlayers

	if offset == -1 then
		None

	else if offset == 0 then
		Log.info("played unknown 1 in correct order, no ocm")
		None

	else if target == playerIndex then
		Log.error("double order chop move???")
		None

	else
		prev.chop(target) match
			case None =>
				Log.warn(s"attempted to interpret ocm on ${state.names(target)}, but they had no chop!")
				None
			case Some(chop) =>
				Log.highlight(Console.CYAN, s"ocm on ${state.names(target)}, distance $offset")
				Some(List(chop))

def interpretTccm(ctx: ClueContext): Option[List[Int]] =
	val ClueContext(prev, game, action) = ctx
	val (common, state) = (game.common, game.state)
	val ClueAction(_, target, list, clue) = action
	val focus = ctx.focusResult.focus

	if state.inEndgame then
		Log.info("in endgame, not tccm")
		return None

	if prev.common.thinksLocked(prev, target) then
		Log.info("target was locked, not tccm")
		return None

	if list.exists(!prev.state.deck(_).clued) then
		Log.info("touched at least 1 new card, not tccm")
		return None

	Log.info(s"checking tccm: old score ${prev.common.hypoStacks} ${prev.common.hypoPlays}, new score ${common.hypoStacks} ${common.hypoPlays}")

	if prev.common.hypoStacks.zip(common.hypoStacks).exists(s => s._1 > s._2) || prev.common.hypoScore + 1 != common.hypoScore then
		Log.info(s"new score is not 1 exactly more than old score, not tccm")
		return None

	state.deck(focus).id() match
		case Some(id) if !common.unknownPlays.contains(focus) && common.hypoStacks(id.suitIndex) < id.rank =>
			Log.info(s"${state.logId(id)} didn't become playable, not tccm")
			return None
		case _ => ()

	if state.hands.exists(_.exists(o => !prev.isBlindPlaying(o) && game.isBlindPlaying(o))) then
		Log.info(s"caused finesse, not tccm")
		return None

	val focusThoughts = common.thoughts(focus)
	val notPromptable = focusThoughts.inferred.forall: id =>
		val prompt = prev.common.findPrompt(prev, target, id)
		prompt.exists(_ != focus)
	val id = focusThoughts.id(infer = true)

	if notPromptable && id.forall(_.rank != 5) then
		Log.info(s"tempo on non-promptable non-5, not tccm")
		return None

	game.chop(target) match
		case Some(chop) =>
			Log.info(s"tccm, chop moving ${if target == state.ourPlayerIndex then s"slot ${state.ourHand.indexOf(chop) + 1}" else state.logId(state.deck(chop))}")
			Some(List(chop))
		case None =>
			Log.warn("looked like tccm but no chop!")
			None

def performCM(game: HGroup, cmOrders: Seq[Int]) =
	val (newCommon, newMeta) = cmOrders.foldLeft((game.common, game.meta)) { (acc, order) =>
		val (common, meta) = acc

		(common.withThought(order) { t =>
			t.copy(inferred = t.inferred.filter(!common.isTrash(game, _, order)))
		},
			meta.updated(order, meta(order).copy(status = CardStatus.ChopMoved))
		)
	}
	game.copy(
		common = newCommon,
		meta = newMeta
	)
