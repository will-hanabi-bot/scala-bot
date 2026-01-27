package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.logger.Log
import scala_bot.utils.visibleFind

def interpretTransfer(game: HGroup, action: DiscardAction, holder: Int, dupe: Option[Int]): DiscardResult =
	val state = game.state
	val DiscardAction(playerIndex, order, suitIndex, rank, _) = action
	val id = Identity(suitIndex, rank)

	if playerIndex == holder then
		if dupe.exists(game.players(holder).thoughts(_).matches(id, infer = true)) then
			Log.info("discarded dupe of own hand")
		else
			Log.warn(s"discarded useful ${state.logId(id)} but dupe was in their own hand!")
		return DiscardResult.Mistake

	val cluedTargets = state.hands(holder).filter(o => game.isTouched(o) && validTransfer(game, id)(o))

	if cluedTargets.isEmpty then
		state.hands(holder).find(validTransfer(game, id)) match
			case Some(uncluedTarget) if dupe.contains(uncluedTarget) =>
				Log.info(s"${if state.isPlayable(id) then "gd" else "baton"} to ${state.names(holder)}'s ${uncluedTarget}")
				DiscardResult.GentlemansDiscard(uncluedTarget)

			case _ if (holder == state.ourPlayerIndex) =>
				DiscardResult.Mistake

			case _ =>
				// Try looking for a third copy
				val other = ((holder + 1) until state.numPlayers).view.flatMap { i =>
					state.hands(i).find(state.deck(_).matches(id)).map(i -> _)
				}.headOption

				other match
					case Some((otherHolder, otherDupe)) =>
						interpretTransfer(game, action, otherHolder, Some(otherDupe))

					case None if state.baseCount(id.toOrd) + 1 == state.cardCount(id.toOrd) =>
						DiscardResult.Mistake

					case None =>
						interpretTransfer(game, action, state.ourPlayerIndex, None)

	else if dupe.exists(!cluedTargets.contains(_)) then
		Log.warn(s"looks like sarcastic discard to $cluedTargets, but should be $dupe!")
		DiscardResult.Mistake

	else
		Log.info(s"sarcastic to ${state.names(holder)}'s $cluedTargets")
		DiscardResult.Sarcastic(cluedTargets)

def interpretUsefulDcH(game: HGroup, action: DiscardAction): DiscardResult =
	val state = game.state
	val DiscardAction(playerIndex, order, suitIndex, rank, _) = action
	val id = Identity(suitIndex, rank)

	Log.info("interpreting useful dc!")

	val dupe = (0 until state.numPlayers).view.flatMap { i =>
		state.hands(i).find(state.deck(_).matches(id)).map(i -> _)
	}.headOption

	dupe match
		case Some((dupeHolder, dupeOrder)) =>
			interpretTransfer(game, action, dupeHolder, Some(dupeOrder))

		case None if playerIndex == state.ourPlayerIndex =>
			// We discarded a card that we don't see nor have the other copy of
			DiscardResult.Mistake

		case None =>
			// Since we can't find it, we must be the target
			interpretTransfer(game, action, state.ourPlayerIndex, None)

def valid1ClueScream(game: HGroup, bob: Int) =
	val bobChop = game.chop(bob)
	bobChop.exists: o =>
		val hypo = game.withMeta(o)(_.copy(status = CardStatus.ChopMoved))
		hypo.common.thinksLocked(hypo, bob)

def checkSdcm(prev: HGroup, action: DiscardAction): Option[DcStatus] =
	val (common, state) = (prev.common, prev.state)
	val DiscardAction(playerIndex, order, _, _, _) = action
	val bob = state.nextPlayerIndex(playerIndex)
	val cathy = state.nextPlayerIndex(bob)

	if common.thinksLocked(prev, bob) && state.clueTokens == 0 then
		return None

	val chop = prev.chop(playerIndex)

	val scream =
		chop.contains(order) &&
		common.thinksLoaded(prev, playerIndex) && {
			state.clueTokens == 0 ||
			(state.clueTokens == 1 && valid1ClueScream(prev, bob))
		}

	val shout = common.thinksPlayables(prev, playerIndex).nonEmpty &&
		common.thinksTrash(prev, playerIndex).contains(order)

	val result = if scream then DcStatus.Scream else DcStatus.Shout

	Option.when(scream || shout):
		if state.numPlayers == 2 then
			result
		else if state.clueTokens == 0 && common.thinksLoaded(prev, bob) then
			Log.warn(s"${state.names(playerIndex)} discarded with a playable/kt but next player was safe! (echo?)")
			DcStatus.Generation
		else if cathy == state.ourPlayerIndex || common.thinksLoaded(prev, cathy) then
			result
		else
			val generation = state.clueTokens == 0 && prev.chop(cathy).exists: o =>
				state.deck(o).id().exists: id =>
					(state.isCritical(id) || state.isPlayable(id)) &&
					visibleFind(state, prev.players(playerIndex), id, infer = true, excludeOrder = o).isEmpty

			if generation then DcStatus.Generation else result
