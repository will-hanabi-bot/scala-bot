package scala_bot.basics

import scala_bot.logger.Log

def interpretUsefulDc[G <: Game](game: G, action: DiscardAction)(using ops: GameOps[G]) =
	val (common, state) = (game.common, game.state)
	val DiscardAction(playerIndex, order, suitIndex, rank, _) = action
	val id = Identity(suitIndex, rank)
	val gd = game.common.hypoPlays.contains(order)

	Log.info("interpreting useful dc!")

	state.hands.flatten.find(state.deck(_).matches(id)) match {
		case Some(dupe) =>
			val holder = state.holderOf(dupe)

			if (holder == playerIndex)
				if (game.players(playerIndex).thoughts(dupe).matches(id, infer = true))
					Log.info("discarded dupe of own hand")
				else
					Log.warn(s"discarded useful ${state.logId(id)} but dupe was in their own hand!")
				(DiscardInterp.None, game)
			else if (gd)
				val target = state.hands(holder).reverse.find(common.thoughts(_).possible.contains(id)).get

				if (target != dupe)
					Log.warn(s"transfer to $dupe was not to rightmost $target!")
					(DiscardInterp.Mistake, game)
				else
					Log.info(s"gd to ${state.names(holder)}'s $target")
					val newGame = ops.copyWith(game, GameUpdates(
						common = Some(common.withThought(target)(_.copy(
							inferred = IdentitySet.single(id)
						))),
						meta = Some(game.meta.updated(target, game.meta(target).copy(
							status = CardStatus.GentlemansDiscard
						))))
					)
					(DiscardInterp.GentlemansDiscard, newGame)
			else
				val orders = state.hands(holder).filter(common.thoughts(_).possible.contains(id))
				Log.info(s"sarcastic to ${state.names(holder)}'s $orders")
				val newGame = ops.copyWith(game, GameUpdates(
					common = Some(common.copy(
						links = Link.Sarcastic(orders, id) +: common.links
					)))
				)
				(DiscardInterp.Sarcastic, newGame)

		case None if playerIndex == state.ourPlayerIndex =>
			// We discarded a card that we don't see nor have the other copy of
			(DiscardInterp.Mistake, game)

		case None if gd =>
			// Since we can't find it, we must be the target
			state.ourHand.reverse.find(game.me.thoughts(_).possible.contains(id)) match {
				case Some(target) =>
					Log.info(s"gd to our $target")
					val newGame = ops.copyWith(game, GameUpdates(
						common = Some(common.withThought(target)(_.copy(
							inferred = IdentitySet.single(id)
						))),
						meta = Some(game.meta.updated(target, game.meta(target).copy(
							status = CardStatus.GentlemansDiscard
						))))
					)
					(DiscardInterp.GentlemansDiscard, newGame)
				case None =>
					Log.warn("looked like gd but we don't see it and impossible for us to have!")
					(DiscardInterp.Mistake, game)
			}
		case None =>
			val orders = state.ourHand.filter(common.thoughts(_).possible.contains(id))
			Log.info(s"sarcastic to our $orders")
			val newGame = ops.copyWith(game, GameUpdates(
				common = Some(common.copy(
					links = Link.Sarcastic(orders, id) +: common.links
				))
			))
			(DiscardInterp.Sarcastic, newGame)
	}
