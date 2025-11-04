package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.basics.given_Conversion_IdentitySet_Iterable
import scala_bot.logger.Log

enum UpdateResult:
	case Remove
	case Complete
	case Keep
	case Advance(nextIndex: Int, skipped: Boolean = false)
	case Demonstrated(order: Int, id: Identity, nextIndex: Option[Int], skipped: Boolean = false)
	case AmbiguousPassback

def stompedWc(game: HGroup, wc: WaitingConnection, lastPlayerIndex: Int) =
	game.lastActions(lastPlayerIndex).exists {
		case action @ ClueAction(giver, target, list, clue) =>
			// Reconstruct previous game
			val prev = game.withState { s => s.copy(
				deck = list.foldLeft(s.deck) { (acc, o) =>
					val clues = acc(o).clues
					if (clues.length == 1 && clues.head.turn == s.turnCount - 1)
						acc.updated(o, acc(o).copy(clued = false, clues = Nil))
					else
						acc
				})
			}

			val focus = game.determineFocus(prev, action).focus

			wc.connections.exists { conn =>
				!(conn.hidden && conn.reacting == giver) &&		// Allow a hidden player to stomp, since they don't know
				conn.ids.forall(game.state.deck(focus).id().contains)
			}
		case _ => false
	}

def updateWc(game: HGroup, wc: WaitingConnection, lastPlayerIndex: Int): UpdateResult =
	val state = game.state
	val reacting = wc.currConn.reacting

	Log.info(s"waiting for connecting ${wc.currConn.ids.map(state.logId).mkString(",")} ${wc.currConn.kind} for ${state.logId(wc.inference)}")

	val impossibleConn = wc.connections.find { conn =>
		game.common.thoughts(conn.order).possible.intersect(conn.ids).isEmpty
	}

	lazy val impossibleFocus = wc.connections.headOption.forall(!_.isInstanceOf[PositionalConn]) &&
		!game.common.thoughts(wc.focus).possible.contains(wc.inference)

	if (impossibleConn.isDefined)
		val c = impossibleConn.get
		Log.warn(s"future connection depends on ${c.order} having ids ${c.ids.map(state.logId).mkString(",")}, removing")
		UpdateResult.Remove

	else if (impossibleFocus)
		Log.warn(s"connection depends on focus ${wc.focus} having id ${state.logId(wc.inference)}, removing")
		UpdateResult.Remove

	else if (stompedWc(game, wc, lastPlayerIndex))
		Log.warn(s"connection was clued directly, cancelling")
		UpdateResult.Remove

	else if (otherPlay(game, wc, lastPlayerIndex))
		Log.highlight(Console.CYAN, s"${state.names(lastPlayerIndex)} played connecting card, continuing")
		resolveOtherPlay(game, wc)

	// After the turn we were waiting for
	else if (lastPlayerIndex == reacting)
		if (state.hands(reacting).contains(wc.currConn.order))
			resolveRetained(game, wc)
		else
			game.lastActions(reacting) match {
				case Some(PlayAction(_, order, _, _)) =>
					resolvePlayed(game, wc, order)
				case _ =>
					Log.highlight(Console.YELLOW, s"waiting card ${state.logId(wc.currConn.order)} discarded??")
					UpdateResult.Remove
			}

	else
		UpdateResult.Keep

def otherPlay(game: HGroup, wc: WaitingConnection, lastPlayerIndex: Int) =
	val thought = game.me.thoughts(wc.currConn.order)

	// The card needs to match our thoughts as well as the hypothesized identity in the connection
	game.lastActions(lastPlayerIndex).exists {
		case PlayAction(_, order, suitIndex, rank) =>
			order != wc.currConn.order &&
			thought.matches(Identity(suitIndex, rank), infer = true)
		case _ => false
	} &&
	// currConn.ids.length == 1 &&
	// thought.matches(currConn.ids.head, infer = true) &&
	!wc.currConn.hidden	// Don't advance if the real connection is layered, because that player won't skip

def resolveOtherPlay(game: HGroup, wc: WaitingConnection) =
	val nextIndex = wc.getNextIndex(game.state)

	if (wc.currConn.isInstanceOf[FinesseConn] || game.level < Level.IntermediateFinesses)
		UpdateResult.Demonstrated(wc.focus, wc.inference, nextIndex, skipped = true)
	else
		nextIndex.fold(UpdateResult.Complete) {
			UpdateResult.Advance(_, skipped = true)
		}


def resolveRetained(game: HGroup, wc: WaitingConnection): UpdateResult =
	val state = game.state
	val (reacting, connOrder) = (wc.currConn.reacting, wc.currConn.order)
	val name = state.names(reacting)

	val newFinesseQueued = wc.connections.find { conn =>
		conn.isInstanceOf[FinesseConn] &&
		state.hands(conn.reacting).exists { o =>
			// A newer finesse exists on this player that is not part of the same suit.
			game.isBlindPlaying(o) && o > conn.order &&
			!game.common.thoughts(o).inferred.exists(_.suitIndex == conn.ids.head.suitIndex)
		}
	}

	val unplayableIds = game.common.thoughts(connOrder).inferred.filter { i =>
		!state.isBasicTrash(i) && !state.isPlayable(i)
	}

	if (unplayableIds.nonEmpty)
		Log.info(s"$name didn't play into unplayable ${wc.currConn.kind}")
		return UpdateResult.Keep

	// val discarded = game.lastActions(reacting).exists(_.isInstanceOf[DiscardAction]) &&
	// 	game.dcStatus == DcStatus.None && newFinesseQueued.isEmpty

	// if (discarded)
	// 	Log.info(s"discarded with a waiting connection, removing ${state.logId(wc.inference)}")
	// 	// Does this need to be a rewind instead?
	// 	return UpdateResult.Remove

	val missedReaction = newFinesseQueued.isDefined || (wc.currConn match {
		case _: FinesseConn => true
		case _: PromptConn => true
		case _ => false
	})

	if (!missedReaction)
		return UpdateResult.Keep

	val olderUnplayableFinesse =
		game.level >= Level.IntermediateFinesses &&
		state.hands(reacting).zipWithIndex.exists { (o, i) =>
			o < connOrder &&
			!{
				state.deck(o).clued ||
				state.hands(reacting).drop(i).forall(state.deck(_).clued)
			} &&
			game.isBlindPlaying(o) &&
			game.xmeta(o).turnFinessed.get < game.xmeta(connOrder).turnFinessed.get &&
			game.common.thoughts(o).inferred.exists { i =>
				!state.isBasicTrash(i) && !state.isPlayable(i)
			}
		}

	if (olderUnplayableFinesse)
		Log.info(s"allowing $name to clue, need to wait for unplayable older finesse")
		return UpdateResult.Keep

	game.lastActions(reacting).get match {
		case _: ClueAction =>
			if (wc.currConn.isInstanceOf[PromptConn] && game.lastMove.exists(m => m != ClueInterp.Mistake && m != ClueInterp.Stall))
				Log.info(s"allowing $name to defer a prompt by giving a useful clue")
				return UpdateResult.Keep

			if (game.level >= Level.IntermediateFinesses && game.importantAction(reacting))
				if (wc.currConn.isPossiblyBluff)
					Log.info(s"$name not allowed to defer a potential bluff")
				else
					Log.info(s"allowing $name to defer a finesse for an important clue")
					return UpdateResult.Keep

		case PlayAction(_, order, _, _) =>
			if (wc.currConn.isInstanceOf[FinesseConn] && game.isBlindPlaying(order))
				if (order < connOrder)
					Log.info(s"$name played into older finesse, continuing to wait")
					return UpdateResult.Keep

				if (game.meta(connOrder).hidden && state.deck(connOrder).clued)
					Log.info(s"$name jumped ahead in layered finesse, continuing to wait")
					return UpdateResult.Keep
			// else
			// 	Log.info(s"$name played into something else, continuing")
			// 	return UpdateResult.Keep
		case _ => ()
	}

	val passback = {
		wc.currConn match {
			case f: FinesseConn =>
				lazy val nonHiddenConns = wc.connections.count { conn =>
					!conn.hidden &&
					conn.reacting == wc.currConn.reacting &&
					wc.currConn.isInstanceOf[FinesseConn]
				}

				reacting != state.ourPlayerIndex &&	// can't pass back to ourselves
				nonHiddenConns > 1 &&					// they need to play more than 1 card
				nonHiddenConns == wc.inference.rank - state.playStacks(wc.inference.suitIndex) &&	// they have all required cards
				!f.ambiguousPassback
			case _ => false
		}
	}

	if (passback)
		Log.info(s"$name didn't play into ${wc.currConn.kind} but they need to play multiple non-hidden cards, passing back")
		return UpdateResult.AmbiguousPassback

	Log.highlight(Console.CYAN, s"$name discarded with a waiting connection, removing ${state.logId(wc.inference)}")
	UpdateResult.Remove

def resolvePlayed(game: HGroup, wc: WaitingConnection, play: Int): UpdateResult =
	val state = game.state

	Log.info(s"waiting card ${state.logId(play)} played")

	val nextIndex = wc.getNextIndex(state)

	val missedReaction = wc.currConn match {
		case _: FinesseConn => true
		case _: PromptConn => true
		case _ => false
	}

	lazy val stompedFinesse = {
		val card = state.deck(wc.currConn.order)

		card.clued && card.clues.last.turn > wc.turn && (
			game.meta(wc.currConn.order).focused || (
				game.common.thinksPlayables(game, wc.currConn.reacting).isEmpty &&
				game.common.thoughts(wc.currConn.order).possible.forall { i =>
					state.isPlayable(i) || card.matches(i, assume = true) || state.isBasicTrash(i)
				}
			)
		)
	}

	if (missedReaction)
		if (wc.currConn.isInstanceOf[FinesseConn] && stompedFinesse)
			Log.info(s"connecting card was stomped on, not confirming finesse")

			if (nextIndex.exists(wc.connections(_).reacting == wc.currConn.reacting))
				Log.info(s"connection requires a self blind play, removing due to occam's razor")
				return UpdateResult.Remove

		else if (wc.currConn.isInstanceOf[FinesseConn] || game.level < Level.IntermediateFinesses)
			return UpdateResult.Demonstrated(wc.focus, wc.inference, nextIndex)

	nextIndex.fold(UpdateResult.Complete)(UpdateResult.Advance(_))
