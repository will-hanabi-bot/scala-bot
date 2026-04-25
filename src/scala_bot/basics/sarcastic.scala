package scala_bot.basics

import scala_bot.logger.Log

enum DiscardResult:
	case None
	case Mistake
	case Sarcastic(orders: Seq[Int])
	case GentlemansDiscard(orders: Seq[Int])
	case Baton(order: Int)

def validTransfer(game: Game, id: Identity)(order: Int) =
	val thought = game.common.thoughts(order)

	thought.possible.contains(id) &&
	!thought.id(infer = true, symmetric = true).exists(_.rank < id.rank) &&		// Do not sarcastic on connecting cards
	thought.infoLock.forallO(_.contains(id))

def interpretUsefulDc(game: Game, action: DiscardAction): DiscardResult =
	val (common, state) = (game.common, game.state)
	val DiscardAction(playerIndex, order, suitIndex, rank, _) = action
	val id = Identity(suitIndex, rank)
	val gd = state.isPlayable(id)

	Log.info("interpreting useful dc!")

	def findGD(holder: Int, hypoState: State, connected: Set[Int]): Option[List[Int]] =
		state.hands(holder).findLast(o => !connected.contains(o) && common.thoughts(o).possible.contains(id)) match
			case None => None
			case Some(f) =>
				val finesseId =
					if game.future(f).length == 1 then
						Some(game.future(f).head)
					else
						game.me.thoughts(f).id()

				finesseId match
					case None => Some(List(f))
					case Some(i) if i.matches(id) => Some(List(f))
					case Some(i) if hypoState.isPlayable(i) =>
						findGD(holder, hypoState.withPlay(i), connected + f).map: rest =>
							f +: rest
					case _ => None

	def tryFinding(excluding: Set[Int] = Set.empty): DiscardResult =
		state.hands.flatten.find(o => !excluding.contains(o) && state.deck(o).matches(id)) match
			case Some(dupe) =>
				val holder = state.holderOf(dupe)

				if holder == playerIndex then
					if state.cardCount(id.toOrd) - state.baseCount(id.toOrd) > 1 then
						tryFinding(excluding = excluding + dupe)
					else if game.players(playerIndex).thoughts(dupe).matches(id, infer = true) then
						Log.info("discarded dupe of own hand")
						DiscardResult.None
					else
						Log.warn(s"discarded useful ${state.logId(id)} but dupe was in their own hand!")
						DiscardResult.None

				else if gd then
					findGD(holder, state, Set.empty) match
						case None =>
							if state.cardCount(id.toOrd) - state.baseCount(id.toOrd) > 1 then
								tryFinding(excluding = excluding + dupe)
							else
								Log.warn(s"transfer to $dupe was not to rightmost $dupe!")
								DiscardResult.Mistake

						case Some(orders) =>
							Log.info(s"gd to ${state.names(holder)}'s $orders")
							DiscardResult.GentlemansDiscard(orders)
				else
					val orders = state.hands(holder).filter(validTransfer(game, id))
					Log.info(s"sarcastic to ${state.names(holder)}'s $orders")
					DiscardResult.Sarcastic(orders)

			case None if playerIndex == state.ourPlayerIndex =>
				// We discarded a card that we don't see nor have the other copy of,
				// but we trust that the team made a good decision.
				if game.meta(order).status == CardStatus.CalledToDiscard then
					DiscardResult.None
				else
					DiscardResult.Mistake

			case None if gd =>
				// Since we can't find it, we must be the target
				findGD(state.ourPlayerIndex, state, Set.empty) match
					case None =>
						Log.warn(s"looked like gd but we don't see it and impossible for us to have!")
						DiscardResult.Mistake

					case Some(orders) =>
						Log.info(s"gd to our $orders")
						DiscardResult.GentlemansDiscard(orders)

			case None =>
				val orders = state.ourHand.filter(validTransfer(game, id))
				Log.info(s"sarcastic to our $orders")
				DiscardResult.Sarcastic(orders)

	tryFinding()
