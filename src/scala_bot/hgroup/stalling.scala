package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.logger.Log

val STALL_INDICES = Map(
	StallInterp.Stall5 -> 0,
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
	StallInterp.Clues8 -> 2,
	StallInterp.Burn -> 5
)

def stallSeverity(game: HGroup, player: Player, giver: Int, infoPlayer: Option[Player] = None) =
	val state = game.state

	lazy val severity2 =
		game.dcStatus == DcStatus.Scream ||
		game.dcStatus == DcStatus.Shout ||
		game.dda.exists { id =>
			game.chop(giver).exists { chop =>
				infoPlayer.getOrElse(player).thoughts(chop).possible.contains(id)
			}
		}

	if (state.clueTokens == 8 && state.turnCount != 1)
		4
	else if (player.thinksLocked(game, giver))
		3
	else if (severity2)
		2
	else if (game.inEarlyGame)
		1
	else
		0

def isStall(ctx: ClueContext, severity: Int): Option[StallInterp] =
	val ClueContext(prev, game, action) = ctx
	val state = ctx.state
	val ClueAction(_, target, list, clue) = action
	val FocusResult(focus, chop, _) = ctx.focusResult

	val focusNew = !prev.state.deck(focus).clued
	val reclue = list.forall(prev.state.deck(_).clued)

	lazy val trash = state.deck(focus).id().exists(state.isBasicTrash) ||
		game.me.orderKt(game, focus)

	val notStall = severity == 0 ||
		(chop && game.common.thoughts(focus).possible.exists(state.isCritical)) ||	// Save clue
		game.common.orderKp(game, focus) || 	// Play clue
		(focusNew && trash)

	if (notStall)
		return None

	val stall5 = clue.isEq(BaseClue(ClueKind.Rank, 5)) &&
		focusNew &&
		!prev.meta(focus).cm &&
		!chop

	if (stall5)
		Log.info(s"5 stall!")
		return Some(StallInterp.Stall5)

	val (_, fill, _) = elimResult(prev, game, state.hands(target), list)
	val (_, playables) = playablesResult(prev, game)

	if (severity >= 2 && reclue)
		if (playables.nonEmpty)
			Log.info(s"tempo clue stall!")
			return Some(StallInterp.Tempo)

		if (fill.nonEmpty)
			Log.info(s"fill-in stall!")
			return Some(StallInterp.FillIn)

	if (severity >= 3 && chop && !game.common.thinksLocked(game, target))
		Log.info(s"locked hand stall!")
		return Some(StallInterp.Locked)

	if (severity == 4 && prev.state.clueTokens == 8 && focusNew && !list.contains(state.hands(target).head))
		Log.info(s"8 clue stall!")
		return Some(StallInterp.Clues8)

	if (severity >= 2 && reclue && fill.isEmpty)
		Log.info(s"hard burn!")
		return Some(StallInterp.Burn)

	None


/** Returns the set of player indices that see an alternative clue. */
def alternativeClue(prev: HGroup, game: HGroup, giver: Int, maxStall: Int, origClue: Clue) =
	def satisfied(hypo: HGroup, action: ClueAction) =
		val (badTouch, _, _) = badTouchResult(game, hypo, action)
		val (_, playables) = playablesResult(game, hypo)

		hypo.lastMove.get match {
			case ClueInterp.Play =>
				playables.exists(!game.state.deck(_).clued) && badTouch.isEmpty
			case ClueInterp.Save =>
				true
			case ClueInterp.Stall =>
				val interp = hypo.stallInterp.get
				interp match {
					case StallInterp.Stall5 =>
						hypo.level >= 2 && !hypo.stalled5 && STALL_INDICES(interp) < maxStall

					case i => STALL_INDICES(i) < maxStall
				}
			case _ => false
		}

	val state = game.state

	(for
		target <- (0 until state.numPlayers).view if target != giver && target != state.ourPlayerIndex
		clue <- state.allValidClues(target) if clue != origClue
		list = state.clueTouched(state.hands(target), clue)
		action = ClueAction(giver, target, list, clue.toBase)
		hypo = prev.copy(allowFindOwn = false, noRecurse = true)
			.simulateClue(action) if satisfied(hypo, action)
	yield
		Log.info(s"found alt clue ${clue.fmt(state)} ${hypo.lastMove.get}")
		val newWCs = hypo.waiting.filter { wc =>
			wc.turn == hypo.state.turnCount && wc.connections.forall { conn =>
				// Only count valid wcs based on the new info we have
				conn.ids.exists(game.common.thoughts(conn.order).possible.contains)
			}
		}
		val seenBy = (0 until state.numPlayers).filterNot { p =>
			p == target ||
			newWCs.exists(_.connections.exists { conn =>
				conn.kind != "known" && p == state.holderOf(conn.order)
			})
		}
		seenBy)
	.foldLeft((0 until state.numPlayers).toSet)(_ -- _)

def stallingSituation(ctx: ClueContext): Option[(StallInterp, Set[Int])] =
	val ClueContext(prev, game, action) = ctx
	val ClueAction(giver, target, list, clue) = ctx.action
	val severity = stallSeverity(ctx.prev, ctx.prev.common, giver, infoPlayer = Some(ctx.game.common))

	Log.info(s"severity $severity")

	isStall(ctx, severity).flatMap { stall =>
		val giverLoaded = prev.common.thinksPlayables(prev, giver).nonEmpty ||
			(prev.common.thinksTrash(prev, giver).nonEmpty && prev.state.clueTokens < 8)

		Option.when(!giverLoaded) {
			if (game.noRecurse)
				(stall, (0 to game.state.numPlayers).toSet)
			else
				val fullClue = Clue(clue.kind, clue.value, target)
				val thinksStall = alternativeClue(prev, game, giver, STALL_TO_SEVERITY(stall), fullClue)
				(stall, thinksStall)
		}
	}
