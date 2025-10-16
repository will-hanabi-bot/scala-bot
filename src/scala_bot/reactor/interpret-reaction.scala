package scala_bot.reactor

import scala_bot.basics._
import scala_bot.basics.given_Conversion_IdentitySet_Iterable
import scala_bot.logger.Log

def calcSlot(focusSlot: Int, slot: Int) =
	val other = (focusSlot + 5 - slot) % 5
	if (other == 0) 5 else other

private def calcTargetSlot(prev: Game, game: Game, order: Int, wc: WaitingConnection) =
	val WaitingConnection(_, reacter, receiver, receiverHand, _, focusSlot, _, _) = wc
	val reactSlot = prev.state.hands(reacter).indexWhere(_ == order) + 1
	val targetSlot = calcSlot(focusSlot, reactSlot)

	receiverHand.lift(targetSlot - 1) match {
		case None =>
			Log.warn(s"Receiver no longer has slot $targetSlot!")
			None
		case Some(receiveOrder) =>
			if (!game.state.hands(receiver).contains(receiveOrder))
				Log.warn(s"Receiver no longer has order $receiveOrder!")
				None
			else
				Some((reactSlot, targetSlot))
	}

def elimDcDc(state: State, common: Player, meta: Vector[ConvData], reacter: Int, receiverHand: Vector[Int], focusSlot: Int, targetSlot: Int) =
	// Entire hand is unplayable
	val (newCommon, newMeta) = elimPlayPlay(state, common, meta, reacter, receiverHand, focusSlot, receiverHand.length + 1)

	receiverHand.zipWithIndex.take(targetSlot - 1).foldLeft((newCommon, newMeta)) { case ((c, m), (receiveOrder, i)) =>
		val status = m(receiveOrder).status
		lazy val reactSlot = calcSlot(focusSlot, i + 1)

		val skip = status == CardStatus.CalledToPlay ||
			status == CardStatus.CalledToDiscard ||
			(state.deck(receiverHand(targetSlot - 1)).clued && !state.deck(receiveOrder).clued)

		if (skip)
			(c, m)
		else
			state.hands(reacter).lift(reactSlot - 1) match {
				case None => (c, m)
				case Some(reactOrder) if newCommon.thoughts(reactOrder).possible.forall(state.isCritical) =>
					(c, m)
				case _ =>
					val newCommon = c.withThought(receiveOrder)(t => t.copy(
						inferred = t.inferred.retain(!state.isBasicTrash(_))
					))
					Log.info(s"eliminated trash from slot ${i + 1} $receiveOrder - ${newCommon.strInfs(state, receiveOrder)}")
					(newCommon, newMeta)
			}
	}

def elimPlayDc(state: State, common: Player, meta: Vector[ConvData], reacter: Int, receiverHand: Vector[Int], focusSlot: Int, targetSlot: Int) =
	// Entire hand is unplayable
	val (newCommon, newMeta) = elimPlayPlay(state, common, meta, reacter, receiverHand, focusSlot, receiverHand.length + 1)

	receiverHand.zipWithIndex.take(targetSlot - 1).foldLeft((newCommon, newMeta)) { case ((c, m), (receiveOrder, i)) =>
		val status = m(receiveOrder).status
		lazy val reactSlot = calcSlot(focusSlot, i + 1)

		val skip = status == CardStatus.CalledToPlay ||
			status == CardStatus.CalledToDiscard ||
			(state.deck(receiverHand(targetSlot - 1)).clued && !state.deck(receiveOrder).clued)

		if (skip)
			(c, m)
		else
			state.hands(reacter).lift(reactSlot - 1) match {
				case Some(reactOrder) if newCommon.thoughts(reactOrder).possible.exists(state.isPlayable) =>
					val newCommon = c.withThought(receiveOrder)(t => t.copy(
						inferred = t.inferred.retain(!state.isBasicTrash(_))
					))
					Log.info(s"eliminated trash from slot ${i + 1} $receiveOrder - ${newCommon.strInfs(state, receiveOrder)}")
					(newCommon, newMeta)
				case _ => (c, m)
			}
	}

val updateMeta = (meta: Vector[ConvData], common: Player, receiveOrder: Int) =>
	if (common.thoughts(receiveOrder).inferred.isEmpty)
		meta.updated(receiveOrder, meta(receiveOrder).copy(trash = true))
	else
		meta

def elimDcPlay(state: State, common: Player, meta: Vector[ConvData], reacter: Int, receiverHand: Vector[Int], focusSlot: Int, targetSlot: Int) =
	receiverHand.zipWithIndex.take(targetSlot - 1).foldLeft((common, meta)) { case ((c, m), (receiveOrder, i)) =>
		val status = m(receiveOrder).status
		lazy val reactSlot = calcSlot(focusSlot, i + 1)

		if (status == CardStatus.CalledToPlay || status == CardStatus.CalledToDiscard)
			(c, m)
		else
			state.hands(reacter).lift(reactSlot - 1) match {
				case Some(reactOrder) if !common.thoughts(reactOrder).possible.forall(state.isCritical)=>
					val newCommon = c.withThought(receiveOrder)(t => t.copy(
						inferred = t.inferred.retain(!state.isPlayable(_))
					))
					Log.info(s"eliminated playables from slot ${i + 1} $receiveOrder - ${newCommon.strInfs(state, receiveOrder)}")
					(newCommon, updateMeta(meta, newCommon, receiveOrder))
				case _ => (c, m)
			}
		}

def elimPlayPlay(state: State, common: Player, meta: Vector[ConvData], reacter: Int, receiverHand: Vector[Int], focusSlot: Int, targetSlot: Int) =
	receiverHand.zipWithIndex.take(targetSlot - 1).foldLeft((common, meta)) { case ((c, m), (receiveOrder, i)) =>
		val status = m(receiveOrder).status
		lazy val reactSlot = calcSlot(focusSlot, i + 1)

		if (status == CardStatus.CalledToPlay || status == CardStatus.CalledToDiscard)
			(c, m)
		else
			state.hands(reacter).lift(reactSlot - 1) match {
				case None => (c, m)
				case Some(reactOrder) =>
					common.thoughts(reactOrder).possible.retain(state.isPlayable) match {
						case IdentitySet() => (c, m)

						case IdentitySet(id) =>
							val newCommon = c.withThought(receiveOrder)(t => t.copy(
								inferred = t.inferred.retain(i => !state.isPlayable(i) || i == id)
							))
							Log.info(s"eliminated playables except ${state.logId(id)} from slot ${i + 1} $receiveOrder - ${newCommon.strInfs(state, receiveOrder)}")
							(newCommon, updateMeta(meta, newCommon, receiveOrder))

						case IdentitySet(_ @ _*) =>
							val newCommon = c.withThought(receiveOrder)(t => t.copy(
								inferred = t.inferred.retain(!state.isPlayable(_))
							))
							Log.info(s"eliminated playables from slot ${i + 1} $receiveOrder - ${newCommon.strInfs(state, receiveOrder)}")
							(newCommon, updateMeta(meta, newCommon, receiveOrder))
					}
			}
	}

def targetIDiscard(prev: Game, game: Game, wc: WaitingConnection, targetSlot: Int) =
	val common = game.common
	val order = wc.receiverHand(targetSlot - 1)
	val meta = game.meta(order)

	val newInferred = common.thoughts(order).inferred.retain(!prev.state.isCritical(_))

	val newCommon = common.withThought(order)(t => t.copy(
		oldInferred = Some(t.inferred),
		inferred = newInferred
	))
	val newMeta = game.meta.updated(order, meta.copy(
		status = CardStatus.CalledToDiscard,
		by = Some(wc.giver),
		trash = newInferred.isEmpty
	).reason(game.state.turnCount))

	(newCommon, newMeta)

def targetIPlay(_prev: Game, game: Game, wc: WaitingConnection, targetSlot: Int) =
	val state = game.state
	val order = wc.receiverHand(targetSlot - 1)

	val newCommon = game.common.withThought(order)(t => t.copy(
		oldInferred = Some(t.inferred),
		inferred = t.inferred.retain(state.isPlayable)
	))

	val shifted =
		if (game.meta(order).status == CardStatus.ZeroClueChop)
			state.hands(wc.receiver).find(o => o < order && !state.deck(o).clued && game.meta(o).status == CardStatus.None) match {
				case None =>
					Log.warn("unable to shift zcs forward!")
					game.meta
				case Some(newZCS) =>
					game.meta.updated(newZCS, game.meta(newZCS).copy(status = CardStatus.ZeroClueChop))
			}
		else
			game.meta

	val newMeta = shifted.updated(order, game.meta(order).copy(
		status = CardStatus.CalledToPlay,
		by = Some(wc.giver),
		focused = true
	).reason(state.turnCount))

	(newCommon, newMeta)

def reactDiscard(prev: Game, game: Game, playerIndex: Int, order: Int, wc: WaitingConnection) =
	val state = game.state
	val WaitingConnection(_, reacter, receiver, receiverHand, clue, focusSlot, inverted, turn) = wc
	lazy val knownTrash = prev.common.thinksTrash(prev, reacter)

	if (playerIndex != reacter)
		game
	else if (inverted)
		// We were waiting for a response inversion and they reacted unnaturally
		val unnatural = if (knownTrash.isEmpty)
			prev.state.hands(reacter)(0) != order
		else
			!knownTrash.contains(order)

		if (unnatural)
			game.rewind(turn, InterpAction(ClueInterp.Reactive)) match {
				case Right(newGame) => newGame
				case Left(err) =>
					Log.warn(s"Failed to rewind a response inversion! $err")
					game
			}
		else
			game
	else
		calcTargetSlot(prev, game, order, wc) match {
			case Some((reactSlot, targetSlot)) =>
				val (newCommon, newMeta) = clue.kind match {
					case ClueKind.Colour =>
						val (newCommon, newMeta) = targetIPlay(prev, game, wc, targetSlot)
						elimDcPlay(prev.state, newCommon, newMeta, reacter, receiverHand, focusSlot, targetSlot)
					case ClueKind.Rank =>
						val (newCommon, newMeta) = targetIDiscard(prev, game, wc, targetSlot)
						elimDcDc(prev.state, newCommon, newMeta, reacter, receiverHand, focusSlot, targetSlot)
				}

				val action = if (clue.kind == ClueKind.Colour) "play" else "dc"
				Log.info(s"reactive dc+$action, reacter ${state.names(reacter)} (slot $reactSlot) receiver ${state.names(receiver)} (slot $targetSlot), focus slot $focusSlot, (order ${state.hands(receiver)(targetSlot - 1)})")
				game.copy(common = newCommon, meta = newMeta)
			case None =>
				game
		}

def reactPlay(prev: Game, game: Game, playerIndex: Int, order: Int, wc: WaitingConnection) =
	val state = game.state
	val WaitingConnection(_, reacter, receiver, receiverHand, clue, focusSlot, inverted, turn) = wc
	lazy val knownPlayables = prev.common.obviousPlayables(prev, reacter)

	if (playerIndex != reacter)
		game
	else if (inverted)
		if (!knownPlayables.contains(order))
			game.rewind(turn, InterpAction(ClueInterp.Reactive)) match {
				case Right(newGame) => newGame
				case Left(err) =>
					Log.warn(s"Failed to rewind a response inversion! $err")
					game
			}
		else
			game
	else
		calcTargetSlot(prev, game, order, wc) match {
			case None =>
				game
			case Some((reactSlot, targetSlot)) =>
				val (newCommon, newMeta) = clue.kind match {
					case ClueKind.Rank =>
						val (newCommon, newMeta) = targetIPlay(prev, game, wc, targetSlot)
						elimPlayPlay(prev.state, newCommon, newMeta, reacter, receiverHand, focusSlot, targetSlot)
					case ClueKind.Colour =>
						val (newCommon, newMeta) = targetIDiscard(prev, game, wc, targetSlot)
						elimPlayDc(prev.state, newCommon, newMeta, reacter, receiverHand, focusSlot, targetSlot)
				}
				val action = if (clue.kind == ClueKind.Colour) "dc" else "play"
				Log.info(s"reactive play+$action, reacter ${state.names(reacter)} (slot $reactSlot) receiver ${state.names(receiver)} (slot $targetSlot), focus slot $focusSlot, (order ${state.hands(receiver)(targetSlot - 1)})")
				game.copy(common = newCommon, meta = newMeta)
		}
