package scala_bot.basics

import scala_bot.logger.Log

enum DiscardResult:
	case None
	case Mistake
	case Sarcastic(orders: Vector[Int])
	case GentlemansDiscard(order: Int)

def validTransfer(game: Game, id: Identity)(order: Int) =
	val thought = game.common.thoughts(order)

	thought.possible.contains(id) &&
	!thought.id(infer = true, symmetric = true).exists(_.rank < id.rank) &&		// Do not sarcastic on connecting cards
	thought.infoLock.forall(_.contains(id))

def interpretUsefulDc(game: Game, action: DiscardAction): DiscardResult =
	val (common, state) = (game.common, game.state)
	val DiscardAction(playerIndex, order, suitIndex, rank, _) = action
	val id = Identity(suitIndex, rank)
	val gd = state.isPlayable(id)

	Log.info("interpreting useful dc!")

	state.hands.flatten.find(state.deck(_).matches(id)) match {
		case Some(dupe) =>
			val holder = state.holderOf(dupe)

			if (holder == playerIndex)
				if (game.players(playerIndex).thoughts(dupe).matches(id, infer = true))
					Log.info("discarded dupe of own hand")
				else
					Log.warn(s"discarded useful ${state.logId(id)} but dupe was in their own hand!")
				DiscardResult.None
			else if (gd)
				val target = state.hands(holder).findLast(common.thoughts(_).possible.contains(id)).get

				if (target != dupe)
					Log.warn(s"transfer to $dupe was not to rightmost $target!")
					DiscardResult.Mistake
				else
					Log.info(s"gd to ${state.names(holder)}'s $target")
					DiscardResult.GentlemansDiscard(target)
			else
				val orders = state.hands(holder).filter(validTransfer(game, id))
				Log.info(s"sarcastic to ${state.names(holder)}'s $orders")
				DiscardResult.Sarcastic(orders)

		case None if playerIndex == state.ourPlayerIndex =>
			// We discarded a card that we don't see nor have the other copy of
			DiscardResult.Mistake

		case None if gd =>
			// Since we can't find it, we must be the target
			state.ourHand.findLast(game.me.thoughts(_).possible.contains(id)) match {
				case Some(target) =>
					Log.info(s"gd to our $target")
					DiscardResult.GentlemansDiscard(target)

				case None =>
					Log.warn("looked like gd but we don't see it and impossible for us to have!")
					DiscardResult.Mistake
			}

		case None =>
			val orders = state.ourHand.filter(validTransfer(game, id))
			Log.info(s"sarcastic to our $orders")
			DiscardResult.Sarcastic(orders)
	}
