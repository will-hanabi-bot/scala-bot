package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.logger.Log

def interpretTransfer(game: HGroup, action: DiscardAction, holder: Int, dupe: Option[Int]): DiscardResult =
	val state = game.state
	val DiscardAction(playerIndex, order, suitIndex, rank, _) = action
	val id = Identity(suitIndex, rank)

	if (playerIndex == holder)
		if (dupe.exists(game.players(holder).thoughts(_).matches(id, infer = true)))
			Log.info("discarded dupe of own hand")
		else
			Log.warn(s"discarded useful ${state.logId(id)} but dupe was in their own hand!")
		return DiscardResult.Mistake

	val cluedTargets = state.hands(holder).filter(o => game.isTouched(o) && validTransfer(game, id)(o))

	if (cluedTargets.isEmpty)
		state.hands(holder).find(validTransfer(game, id)) match {
			case Some(uncluedTarget) if dupe.contains(uncluedTarget) =>
				Log.info(s"${if (state.isPlayable(id)) "gd" else "baton"} to ${state.names(holder)}'s ${uncluedTarget}")
				DiscardResult.GentlemansDiscard(uncluedTarget)

			case _ if (holder == state.ourPlayerIndex) =>
				DiscardResult.Mistake

			case _ =>
				// Try looking for a third copy
				val other = ((holder + 1) until state.numPlayers).view.flatMap { i =>
					state.hands(i).find(state.deck(_).matches(id)).map(i -> _)
				}.headOption

				other match {
					case Some((otherHolder, otherDupe)) =>
						interpretTransfer(game, action, otherHolder, Some(otherDupe))

					case None if state.baseCount(id.toOrd) + 1 == state.cardCount(id.toOrd) =>
						DiscardResult.Mistake

					case None =>
						interpretTransfer(game, action, state.ourPlayerIndex, None)
				}
		}

	else if (dupe.exists(!cluedTargets.contains(_)))
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

	dupe match {
		case Some((dupeHolder, dupeOrder)) =>
			interpretTransfer(game, action, dupeHolder, Some(dupeOrder))

		case None if playerIndex == state.ourPlayerIndex =>
			// We discarded a card that we don't see nor have the other copy of
			DiscardResult.Mistake

		case None =>
			// Since we can't find it, we must be the target
			interpretTransfer(game, action, state.ourPlayerIndex, None)
	}
