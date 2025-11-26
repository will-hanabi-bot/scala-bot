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

	lazy val promisedIds = if (clue.kind == ClueKind.Rank)
		thought.possible.filter(_.rank == clue.value)
	else
		thought.possible

	val notTcm = prev.state.deck(focus).clued ||
		!promisedIds.forall(game.common.isTrash(game, _, focus)) ||
		thought.inferred.forall(i => state.isPlayable(i) && !game.common.isTrash(game, i, focus))

	if (notTcm)
		return None

	val oldestTrash = list.filter(!prev.state.deck(_).clued).min

	val cmOrders = state.hands(target).filter{ o =>
		o < oldestTrash && !state.deck(o).clued && !game.meta(o).cm
	}

	if (cmOrders.isEmpty)
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

	if (not5cm)
		return None

	list.filter(o => o > chop.get && !prev.state.deck(o).clued).minOption.flatMap { oldest5 =>
		val distance = prev.chopDistance(target, oldest5)

		if (distance != 1)
			Log.info(s"rightmost 5 was clued $distance-away from chop, not 5cm!")
			None

		else if (game.common.orderKt(game, chop.get))
			Log.info(s"saved card $chop has only trash possibilities!")
			None

		else
			Log.info(s"5cm, saving ${state.logId(chop.get)} ${chop.get}")
			Some(Vector(chop.get))
	}

def interpretOcm(prev: HGroup, action: PlayAction | DiscardAction) =
	val state = prev.state
	val (playerIndex, order) = action match {
		case PlayAction(p, o, _, _) => (p, o)
		case DiscardAction(p, o, _, _, _) => (p, o)
	}

	val ordered1s = prev.order1s(state.hands(playerIndex))
	val offset = ordered1s.indexOf(order)
	val target = (playerIndex + offset) % state.numPlayers

	if (offset == -1)
		None

	else if (offset == 0)
		Log.info("played unknown 1 in correct order, no ocm")
		None

	else if (target == playerIndex)
		Log.error("double order chop move???")
		None

	else
		prev.chop(target) match {
			case None =>
				Log.warn(s"attempted to interpret ocm on ${state.names(target)}, but they had no chop!")
				None
			case Some(chop) =>
				Log.highlight(Console.CYAN, s"ocm on ${state.names(target)}, distance $offset")
				Some(List(chop))
		}


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
