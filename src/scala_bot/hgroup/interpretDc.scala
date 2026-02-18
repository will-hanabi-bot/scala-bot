package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.utils._
import scala_bot.logger.Log

import scala.util.chaining.scalaUtilChainingOps

case class DiscardContext(prev: HGroup, game: HGroup, action: DiscardAction)

def interpretTransfer(ctx: DiscardContext, holder: Int, dupe: Option[Int]): (DiscardResult, Boolean) =
	val DiscardContext(prev, game, action) = ctx
	val state = game.state
	val DiscardAction(playerIndex, order, suitIndex, rank, _) = action
	val id = Identity(suitIndex, rank)

	if playerIndex == holder then
		if dupe.exists(game.players(holder).thoughts(_).matches(id, infer = true)) then
			Log.info("discarded dupe of own hand")
		else
			Log.warn(s"discarded useful ${state.logId(id)} but dupe was in their own hand!")
		return (DiscardResult.Mistake, false)

	val cluedTargets = state.hands(holder).filter(o => game.isTouched(o) && validTransfer(game, id)(o))

	if cluedTargets.isEmpty then
		if game.level < Level.SpecialDiscards then
			Log.info("looked like out-of-level gd/baton! ignoring")
			(DiscardResult.Mistake, true)

		else if state.isPlayable(id) || prev.common.hypoPlays.contains(order) then
			def findGD(hypoState: State, connected: Set[Int]): Option[List[Int]] =
				game.findFinesse(holder, connected) match
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
								findGD(hypoState.withPlay(i), connected + f).map: rest =>
									f +: rest
							case _ => None

			findGD(state, Set.empty) match
				case None =>
					// Try looking for a third copy
					val other = ((holder + 1) until state.numPlayers).view.flatMap { i =>
						state.hands(i).find(state.deck(_).matches(id)).map(i -> _)
					}.headOption

					other match
						case Some((otherHolder, otherDupe)) =>
							interpretTransfer(ctx, otherHolder, Some(otherDupe))

						case None if state.baseCount(id.toOrd) + 1 == state.cardCount(id.toOrd) =>
							Log.warn(s"couldn't gd to ${state.names(holder)}")
							(DiscardResult.Mistake, true)

						case None if holder != state.ourPlayerIndex =>
							interpretTransfer(ctx, state.ourPlayerIndex, None)

						case _ =>
							Log.warn(s"couldn't gd to ${state.names(holder)}")
							(DiscardResult.Mistake, true)

				case Some(orders) =>
					Log.info(s"gd to ${state.names(holder)}'s $orders")
					(DiscardResult.GentlemansDiscard(orders), false)

		else
			val baton = game.findFinesse(holder, Set.empty) match
				case None => None
				case Some(f) =>
					game.me.thoughts(f).id() match
						case None => Some(f)
						case Some(i) if i.matches(id) => Some(f)
						case _ => None

			baton match
				case None =>
					Log.warn(s"couldn't baton to ${state.names(holder)}")
					(DiscardResult.Mistake, true)
				case Some(order) =>
					Log.info(s"baton to ${state.names(holder)}'s $order")
					(DiscardResult.Baton(order), false)

	else if dupe.exists(!cluedTargets.contains(_)) then
		Log.warn(s"looks like sarcastic discard to $cluedTargets, but should be $dupe!")
		(DiscardResult.Mistake, false)

	else
		Log.info(s"sarcastic to ${state.names(holder)}'s $cluedTargets")
		(DiscardResult.Sarcastic(cluedTargets), false)

def checkUsefulDcH(ctx: DiscardContext): (DiscardResult, Boolean) =
	val DiscardContext(prev, game, action) = ctx
	val state = game.state
	val DiscardAction(playerIndex, order, suitIndex, rank, _) = action
	val id = Identity(suitIndex, rank)

	Log.info("interpreting useful dc!")

	val dupe = (0 until state.numPlayers).view.flatMap { i =>
		state.hands(i).find(state.deck(_).matches(id)).map(i -> _)
	}.headOption

	dupe match
		case Some((dupeHolder, dupeOrder)) =>
			interpretTransfer(ctx, dupeHolder, Some(dupeOrder))

		case None if playerIndex == state.ourPlayerIndex =>
			// We discarded a card that we don't see nor have the other copy of
			(DiscardResult.Mistake, true)

		case None =>
			// Since we can't find it, we must be the target
			interpretTransfer(ctx, state.ourPlayerIndex, None)

def transferWCs(ctx: DiscardContext, result: DiscardResult): HGroup =
	val DiscardContext(prev, game, action) = ctx
	val state = game.state
	val DiscardAction(playerIndex, order, suitIndex, rank, failed) = action
	val id = Identity(suitIndex, rank)

	result match
		case DiscardResult.Sarcastic(orders) if orders.length == 1 =>
			val sarcastic = orders.head
			game.copy(
				waiting = game.waiting.map: wc =>
					val connIndex = wc.connections.indexWhere(_.order == order)

					wc.when(_ => connIndex != -1): _ =>
						Log.info(s"rewriting wc ${state.logConns(wc.connections)}")
						wc.copy(connections = wc.connections.updated(connIndex, KnownConn(
							reacting = state.holderOf(sarcastic),
							order = sarcastic,
							id = id
						)))
			)
		case DiscardResult.Sarcastic(orders) =>
			game.copy(
				waiting = game.waiting.map: wc =>
					val connIndex = wc.connections.indexWhere(_.order == order)

					wc.when(_ => connIndex != -1): _ =>
						Log.info(s"rewriting wc ${state.logConns(wc.connections)}")
						wc.copy(connections = wc.connections.updated(connIndex, PlayableConn(
							reacting = state.holderOf(orders.head),
							order = orders.head,
							linked = orders,
							id = id
						)))
			)
		case DiscardResult.GentlemansDiscard(orders) =>
			game.copy(
				waiting = game.waiting.map: wc =>
					val connIndex = wc.connections.indexWhere(_.order == order)

					wc.when(_ => connIndex != -1): _ =>
						Log.info(s"rewriting wc ${state.logConns(wc.connections)}")
						wc.copy(connections = wc.connections.updated(connIndex, PlayableConn(
							reacting = state.holderOf(orders.head),
							order = orders.head,
							linked = orders,
							id = id
						)))
			)
		case _ =>
			game

def interpretUsefulDcH(ctx: DiscardContext): Option[HGroup] =
	val DiscardContext(prev, game, action) = ctx
	val DiscardAction(playerIndex, order, suitIndex, rank, failed) = action
	val id = Identity(suitIndex, rank)

	val valid = !failed &&
		action.suitIndex != -1 && action.rank != -1 &&
		!game.state.isBasicTrash(id) &&
		prev.isTouched(action.order) && prev.isDefinite(order)

	Option.when(valid):
		(checkUsefulDcH(ctx) match
			case (DiscardResult.None, _) =>
				game.copy(lastMove = Some(DiscardInterp.None), dda = None)

			case (DiscardResult.Mistake, dda) =>
				game.copy(
					waiting = game.waiting.filterNot: wc =>
						wc.connections.exists(_.order == order) ||
						wc.focus == order
					,
					lastMove = Some(DiscardInterp.Mistake),
					dda = Option.when(dda)(id)
				)

			case (DiscardResult.GentlemansDiscard(targets), _) =>
				targets.foldLeft((game, game.state)):
					case ((acc, hypoState), o) =>
						val hidden = o != targets.last
						val inferred = if hidden then hypoState.playableSet else IdentitySet.single(id)
						val newState = game.me.thoughts(o).id().fold(hypoState): i =>
							hypoState.withPlay(i)

						acc.copy(
							common = acc.common.withThought(o)(_.copy(inferred = inferred)),
							meta = acc.meta.updated(o, game.meta(o).copy(
								status = CardStatus.GentlemansDiscard,
								hidden = hidden
							))
						) -> newState
				._1
				.pipe: g =>
					val newCtx = ctx.copy(game = g)
					transferWCs(newCtx, DiscardResult.GentlemansDiscard(targets))
				.copy(
					lastMove = Some(DiscardInterp.GentlemansDiscard),
					dda = None
				)

			case (DiscardResult.Baton(order), _) =>
				game.withThought(order)(_.copy(inferred = IdentitySet.single(id)))
					.withMeta(order)(_.copy(status = CardStatus.Sarcastic))
					.copy(
						lastMove = Some(DiscardInterp.Sarcastic),
						dda = None
					)

			case (DiscardResult.Sarcastic(orders), _) =>
				game.copy(
					common = if !orders.forall(game.state.deck(_).clued) then game.common else game.common.copy(
						links = Link.Sarcastic(orders, id) +: game.common.links
					),
					lastMove = Some(DiscardInterp.Sarcastic),
					dda = None
				)
				.pipe: g =>
					val newCtx = ctx.copy(game = g)
					transferWCs(newCtx, DiscardResult.Sarcastic(orders))
		).copy(dcStatus = DcStatus.None)

def valid1ClueScream(game: HGroup, bob: Int) =
	val bobChop = game.chop(bob)
	bobChop.exists: o =>
		val hypo = game.withMeta(o)(_.copy(status = CardStatus.ChopMoved))
		hypo.common.thinksLocked(hypo, bob)

private def checkSdcm(prev: HGroup, action: DiscardAction): Option[DcStatus] =
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
		prev.players(playerIndex).thinksTrash(prev, playerIndex).contains(order)

	val result = if scream then DcStatus.Scream else DcStatus.Shout

	Option.when(scream || shout):
		if state.numPlayers == 2 then
			result
		// else if state.clueTokens == 0 && common.thinksLoaded(prev, bob) then
		// 	Log.warn(s"${state.names(playerIndex)} discarded with a playable/kt but next player was safe! (echo?)")
		// 	DcStatus.Generation
		else if cathy == state.ourPlayerIndex || common.thinksLoaded(prev, cathy) then
			result
		else
			val generation = state.clueTokens == 0 && prev.chop(cathy).exists: o =>
				state.deck(o).id().exists: id =>
					(state.isCritical(id) || state.isPlayable(id)) &&
					visibleFind(state, prev.players(playerIndex), id, infer = true, excludeOrder = o).isEmpty

			if generation then DcStatus.Generation else result

def interpretSdcm(ctx: DiscardContext): Option[HGroup] =
	val DiscardContext(prev, game, action) = ctx
	val state = game.state

	if game.level < Level.LastResorts || state.inEndgame then
		return None

	val bob = state.nextPlayerIndex(action.playerIndex)
	val bobChop = game.chop(bob)

	checkSdcm(prev, action).map: status =>
		if (status == DcStatus.Scream || status == DcStatus.Shout) && bobChop.isEmpty then
			Log.warn(s"interpreted scream/shout but ${state.names(bob)} has no chop! interpreting mistake")
			game.copy(
				lastMove = Some(DiscardInterp.Mistake),
				dcStatus = DcStatus.None,
				dda = Some(Identity(action.suitIndex, action.rank))
			)

		else
			val bobChopId = bobChop.flatMap(state.deck(_).id())
			val mistake = status.matches:
				case DcStatus.Scream =>
					bobChopId.exists(i => !state.isPlayable(i) && !state.isCritical(i))
				case DcStatus.Shout =>
					bobChopId.exists(state.isBasicTrash)

			if mistake then
				Log.warn(s"interpreted ${status.toString().toLowerCase()} but ${state.names(bob)}'s chop isn't worth saving!")
			else
				Log.info(s"interpreting ${status.toString().toLowerCase()}!")

			game.copy(
				dcStatus = status,
				lastMove = Some(if mistake then DiscardInterp.Mistake else DiscardInterp.Emergency),
				dda = Some(Identity(action.suitIndex, action.rank))
			)
			.when(_ => status != DcStatus.Generation): h =>
				h.withMeta(bobChop.get)(_.copy(status = CardStatus.ChopMoved))

private def playablePoss(game: HGroup, playerIndex: Int, discarder: Int) =
	val state = game.state
	val player = game.players(playerIndex)

	// Disregard connecting plays on discarder or target (too slow)
	val slowPlays = player.hypoPlays.filter: o =>
		val holder = state.holderOf(o)
		holder == playerIndex || holder == discarder

	Log.info(s"slow plays $slowPlays ${player.hypoPlays}")

	for
		(stack, suitIndex) <- player.hypoStacks.zipWithIndex
		id = Identity(suitIndex, stack + 1)
			if !player.isTrash(game, id, -1) && !slowPlays.exists: o =>
				state.deck(o).id().orElse(player.thoughts(o).id(infer = true)).exists(_.playedBefore(id))
	yield
		id

private def checkPosDc(ctx: DiscardContext): Option[IndexedSeq[(Int, Vector[Identity])]] =
	val DiscardContext(prev, game, action) = ctx
	val DiscardAction(playerIndex, order, _, _, failed) = action
	val state = game.state
	val thought = prev.common.thoughts(order)
	val slot = prev.state.hands(playerIndex).indexOf(order) + 1

	val expectedDc = prev.common.thinksTrash(prev, playerIndex).headOption.orElse(prev.chop(playerIndex))

	// Locked hand, blind played a chop moved card that could be good, discarded expected card
	val unintended = expectedDc.forall: o =>
		if !failed then
			order == o
		else
			prev.meta(order).status == CardStatus.ChopMoved &&
			thought.possible.exists(!state.isBasicTrash(_)) &&
			thought.possible.exists(state.isPlayable)

	if unintended then
		return None

	val numPlays = if action.failed && order != expectedDc.get then 2 else 1

	val targets = {
		for
			i <- 1 until state.numPlayers
			index = (playerIndex + i) % state.numPlayers if index != state.ourPlayerIndex && state.hands(index).length >= slot
			targetOrder = state.hands(index)(slot - 1) if !game.common.thinksPlayables(game, index).contains(targetOrder)
			poss = playablePoss(game, index, playerIndex) if poss.exists(state.deck(targetOrder).matches(_))
		yield
			// Find the latest player with an unknown playable
			(index, poss)
	}
	// If we haven't found a target, check if we can be the target.
	.when(_.length < numPlays && playerIndex != state.ourPlayerIndex): targets =>
		val poss = playablePoss(game, state.ourPlayerIndex, playerIndex)

		if state.ourHand.lift(slot - 1).exists(game.me.thoughts(_).inferred.intersect(poss).nonEmpty) then
			targets :+ (state.ourPlayerIndex, poss)
		else
			Log.info(s"can't use us ${state.ourHand.lift(slot - 1).map(game.me.strInfs(game.state, _))} ${poss.map(state.logId).mkString(",")}")
			targets

	if targets.length < numPlays then
		Log.warn(s"weird discard detected, but not enough pos dc targets! ${targets.map(t => state.names(t._1))}")
		None
	else
		// Only take the last N reacting players.
		Some(targets.takeRight(numPlays))

def interpretPosDc(ctx: DiscardContext): Option[HGroup] =
	val DiscardContext(prev, game, action) = ctx
	val DiscardAction(playerIndex, order, _, _, failed) = action
	val state = game.state
	val slot = prev.state.hands(playerIndex).indexOf(order) + 1

	if game.level < Level.Endgame || (!state.inEndgame && state.score < state.maxScore - 5) then
		return None

	checkPosDc(ctx).map: targets =>
		targets.foldRight((game, List.empty[Connection])):
			case ((reacting, poss), (g, conns)) =>
				val order = state.hands(reacting)(slot - 1)
				val newGame = g.withThought(order): t =>
					t.copy(
						oldInferred = t.inferred.toOpt,
						inferred = t.inferred.intersect(poss),
					)
				.withMeta(order): m =>
					m.copy(
						status = CardStatus.CalledToPlay,
						focused = true
					)

				val conn = PositionalConn(
					reacting,
					order,
					newGame.common.thoughts(order).inferred.toList,
					ambiguousOwn =
						if targets.contains(state.ourPlayerIndex) || playerIndex == state.ourPlayerIndex then None else
							state.ourHand.lift(slot - 1).map:
								_ -> playablePoss(g, state.ourPlayerIndex, playerIndex).toList
				)

				Log.info(s"interpreting pos on ${state.names(reacting)} slot $slot!")
				(newGame, conn +: conns)
		.pipe: (g, conns) =>
			val focus = conns.last.order
			g.copy(
				waiting = WaitingConnection(
					connections = conns,
					giver = playerIndex,
					target = targets.last._1,
					turn = state.turnCount,
					focus = focus,
					inference = state.deck(focus).id().getOrElse(Identity(0, 1))
				) +: g.waiting,
				lastMove = Some(DiscardInterp.Positional),
				dcStatus = DcStatus.None,
				dda = None
			)
