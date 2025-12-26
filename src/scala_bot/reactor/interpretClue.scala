package scala_bot.reactor

import scala_bot.basics._
import scala_bot.logger.Log
import scala_bot.utils.playersUntil

private def reactiveFocus(state: State, receiver: Int, action: ClueAction) =
	val ClueAction(_, _, list, clue) = action
	lazy val (_, focusIndex) = state.hands(receiver).zipWithIndex
		.filter((o, _) => list.contains(o))
		.maxBy((o, _) => if o == state.hands(receiver)(0) then -1 else o)

	clue.kind match
		case ClueKind.Colour =>
			if state.includesVariant(RAINBOWISH) then clue.value + 1 else focusIndex + 1
		case ClueKind.Rank =>
			if state.includesVariant(PINKISH) then clue.value else focusIndex + 1

def interpretStable(prev: Reactor, game: Reactor, action: ClueAction, stall: Boolean) =
	val ClueAction(giver, target, _, _) = action

	val (interp, newGame) = tryStable(prev, game, action, stall)
	val bob = game.state.nextPlayerIndex(giver)

	// Check for response inversion
	if target != bob && badStable(prev, newGame, action, interp.getOrElse(ClueInterp.Mistake), stall) then
		val hypoGame = prev.withState(s => s.copy(
			actionList = addAction(s.actionList, action, s.turnCount)
		))
		.onClue(action)
		.elim(goodTouch = true)

		interpretReactive(prev, hypoGame, action, bob, inverted = true)
	else
		(interp, newGame)


private def tryStable(prev: Reactor, game: Reactor, action: ClueAction, stall: Boolean): (Option[ClueInterp], Reactor) =
	Log.info(s"interpreting stable clue!")
	val state = game.state
	val ClueAction(giver, target, list, clue) = action
	val newlyTouched = list.filter(!prev.state.deck(_).clued)

	var newCommon = game.common
	var newMeta = game.meta

	if clue.kind == ClueKind.Rank && newlyTouched.nonEmpty then
		var focus = newlyTouched.max

		val trashPush = (0 until state.variant.suits.length).forall(suitIndex => state.isBasicTrash(Identity(suitIndex, clue.value)))
		lazy val playableRank = (0 until state.variant.suits.length).forall: suitIndex =>
			val id = Identity(suitIndex, clue.value)
			state.isBasicTrash(id) || state.isPlayable(id)

		if trashPush then
			newCommon = newCommon.withThought(focus)(t => t.copy(inferred = t.inferred.intersect(state.trashSet)))
			newMeta = newMeta.updated(focus, newMeta(focus).copy(trash = true))

		else if playableRank then
			game.state.hands(target).filter(!prev.state.deck(_).clued).minOption.foreach: lockOrder =>
				if state.includesVariant(PINKISH) && list.contains(lockOrder) then
					focus = lockOrder

			val unneccessaryFocus = game.common.thoughts(focus).possible.forall: i =>
				state.isBasicTrash(i) || state.hands.flatten.exists(game.common.thoughts(_).matches(i))

			if unneccessaryFocus then
				Log.info("unnecessary focus!")
			else
				val newInferred = newCommon.thoughts(focus).inferred.filter(i => state.isPlayable(i) && i.rank == clue.value)
				newCommon = newCommon.withThought(focus)(t => t.copy(
					inferred = newInferred,
					infoLock = Some(newInferred)
				))
				newMeta = newMeta.updated(focus, newMeta(focus).copy(
					focused = true,
					status = CardStatus.CalledToPlay
				))

	val newGame = game.copy(
		common = newCommon,
		meta = newMeta,
		waiting = Option.when(game.waiting.isEmpty && state.nextPlayerIndex(giver) != target):
			val receiver = target
			val focusSlot = reactiveFocus(state, receiver, action)

			Log.info("writing potential response inversion!")

			ReactorWC(
				giver,
				reacter = game.state.nextPlayerIndex(giver),
				receiver,
				receiverHand = state.hands(receiver),
				clue,
				focusSlot,
				inverted = true,
				turn = game.state.turnCount
			)
		)

	checkFix(prev, game, action) match
		case FixResult.Normal(_, _) =>
			Log.info("fix clue!")
			(Some(ClueInterp.Fix), newGame)

		case _ =>
			val common = newGame.common
			val prevPlayables = prev.common.obviousPlayables(prev, target)
				.concat(connectableSimple(prev, prev.players(giver), state.nextPlayerIndex(giver), target)).distinct
			val playables = common.obviousPlayables(newGame, target)
				.concat(connectableSimple(newGame, newGame.players(giver), state.nextPlayerIndex(giver), target)).distinct

			Log.info(s"playables $playables, prev_playables $prevPlayables")

			lazy val reveal = playables.find: o =>
				list.contains(o) &&
				!prevPlayables.contains(o) &&
				(clue.kind == ClueKind.Rank || prev.state.deck(o).clued)

			if newlyTouched.isEmpty then
				val safeActions = playables.concat(common.thinksTrash(newGame, target))
				val oldSafeActions = prevPlayables.concat(prev.common.thinksTrash(prev, target))

				// Try connecting with an unknown playable
				lazy val connectable =
					val nextIndex = state.nextPlayerIndex(giver)
					state.deck(list.max).id() match
						case Some(focusId) if nextIndex != target && state.playableAway(focusId) == 1 =>
							prev.common.obviousPlayables(prev, state.nextPlayerIndex(giver)).find:
								common.thoughts(_).inferred.contains(focusId.prev.get)
						case _ => None

				if safeActions.exists(!oldSafeActions.contains(_)) then
					Log.info(s"revealed a safe action! ${safeActions.find(!oldSafeActions.contains(_)).get}")
					(Some(ClueInterp.Reveal), newGame)

				else if stall then
					Log.info("stalling with fill-in/hard burn!")
					(Some(ClueInterp.Stall), newGame)

				else if connectable.isDefined then
					Log.info(s"connecting through unknown playable (${connectable.get})!")
					val connectedGame = newGame.withThought(connectable.get) { t =>
						 t.copy(inferred = IdentitySet.single(state.deck(list.max).id().get.prev.get))
					}
					(Some(ClueInterp.Reveal), connectedGame)

				else
					Log.warn("looked like fill-in/hard burn outside of a stalling situation!")
					(None, newGame)

			else if reveal.isDefined then
				Log.info(s"revealed a safe action! ${reveal.get} $prevPlayables")
				(Some(ClueInterp.Reveal), newGame)

			else if common.orderKt(game, newlyTouched.max) then
				// at least 1 useful unplayable brown and clue didn't touch chop
				val brownishTcm =
					state.includesVariant(BROWNISH) &&
					clue.kind == ClueKind.Rank &&
					state.variant.suits.zipWithIndex.exists: (suit, suitIndex) =>
						BROWNISH.matches(suit) && state.playStacks(suitIndex) + 1 < state.maxRanks(suitIndex) &&
						!newlyTouched.contains(state.hands(target)(0))

				if brownishTcm then
					Log.info("brown direct discard!")
					(Some(ClueInterp.Reveal), newGame)
				else
					Log.info("trash push!")
					refPlay(prev, newGame, action)

			else if clue.kind == ClueKind.Colour then
				Log.info("colour clue!")
				refPlay(prev, newGame, action)

			else
				Log.info(s"rank clue!")
				refDiscard(prev, newGame, action, stall)

/**
* Returns a non-bad touching ref play clue or a ref dc clue on trash to the clue target, if it exists.
*/
private def alternativeClue(game: Reactor, clueTarget: Int, playOnly: Boolean = false) =
	if game.noRecurse then None else
		val (common, state) = (game.common, game.state)

		state.allValidClues(clueTarget).find: clue =>
			val list = state.clueTouched(state.hands(clueTarget), clue.toBase)
			val hand = state.hands(clueTarget)
			val newlyTouched = list.filter(!state.deck(_).clued)

			newlyTouched.nonEmpty && {
				clue.kind match
					case ClueKind.Colour =>
						val playTarget = newlyTouched.map(common.refer(game, hand, _, left = true)).max
						val poss = IdentitySet.from(state.variant.touchPossibilities(clue.toBase))

						state.isPlayable(state.deck(playTarget).id().get) &&
							(newlyTouched.forall(o => !state.isBasicTrash(state.deck(o).id().get)) ||
								newlyTouched.forall(common.thoughts(_).possible.intersect(poss).forall(state.isBasicTrash)))
					case ClueKind.Rank =>
						!playOnly &&
						!hand.filter(!state.deck(_).clued).minOption.exists(list.contains) && {
							val focus = newlyTouched.max
							val focusPos = hand.indexOf(focus)
							val targetIndex = hand.zipWithIndex.indexWhere((o, i) => i > focusPos && !state.deck(o).clued)

							state.isBasicTrash(state.deck(hand(targetIndex)).id().get)
						}
			}

def badStable(prev: Reactor, game: Reactor, action: ClueAction, interp: ClueInterp, stall: Boolean = false) =
	val state = game.state
	val target = action.target

	lazy val altPlay = alternativeClue(prev, target, playOnly = true)
	lazy val alt = alternativeClue(game, target, playOnly = false)
	lazy val badPlayable = state.hands(target).find: o =>
		game.meta(o).status == CardStatus.CalledToPlay &&
		prev.meta(o).status != CardStatus.CalledToPlay &&
		!state.hasConsistentInfs(game.common.thoughts(o))

	lazy val badDiscard = state.hands(target).find: o =>
		game.meta(o).status == CardStatus.CalledToDiscard &&
		prev.meta(o).status != CardStatus.CalledToDiscard &&
		(state.isCritical(state.deck(o).id().get) ||
			(stall &&
				!state.isBasicTrash(state.deck(o).id().get) &&
				alt.isDefined))

	if interp == ClueInterp.Mistake then
		true
	else if prev.state.turnCount == 1 && action.clue.kind == ClueKind.Rank && altPlay.isDefined then
		Log.warn(s"bad turn 1 rank clue! ${altPlay.get.fmt(state)} is possible")
		true
	else if target == state.ourPlayerIndex then
		false
	else if badPlayable.isDefined then
		val bad = badPlayable.get
		Log.warn(s"bad playable on $bad ${state.logId(bad)}!")
		true
	else if badDiscard.isDefined then
		Log.warn(s"bad discard on ${badDiscard.get}!")
		true
	// Check for bad lock
	else if interp == ClueInterp.Lock && alt.isDefined then
		Log.warn(s"alternative clue ${alt.get.fmt(state)} was available!")
		true
	else if !stall then
		false
	else if interp == ClueInterp.Stall && alt.isDefined then
		Log.warn(s"alternative clue ${alt.get.fmt(state)} was available!")
		true
	else
		false

def interpretReactive(prev: Reactor, game: Reactor, action: ClueAction, reacter: Int, inverted: Boolean): (Option[ClueInterp], Reactor) =
	val state = game.state
	val ClueAction(giver = giver, target = receiver, clue = clue, list = _) = action

	Log.info("interpreting reactive clue!")
	Log.info(s"reacter: ${state.hands(reacter)} (${state.names(reacter)}), receiver: ${state.hands(receiver)} (${state.names(receiver)})")

	val focusSlot = reactiveFocus(state, receiver, action)

	val newGame = game.copy(
		waiting = Some(ReactorWC(
			giver,
			reacter,
			receiver,
			receiverHand = state.hands(receiver),
			clue,
			focusSlot,
			inverted = false,
			turn = state.turnCount
		))
	)

	if receiver == state.ourPlayerIndex then
		(Some(ClueInterp.Reactive), newGame)
	else
		clue.kind match
			case ClueKind.Colour => interpretReactiveColour(prev, newGame, action, focusSlot, reacter, inverted)
			case ClueKind.Rank   => interpretReactiveRank(prev, newGame, action, focusSlot, reacter)

def delayedPlays(game: Reactor, giver: Int, receiver: Int) =
	val (common, state, meta) = (game.common, game.state, game.meta)

	playersUntil(state.numPlayers, state.nextPlayerIndex(giver), receiver).foldLeft(List.empty[(Int, Identity)]): (acc, playerIndex) =>
		val playables = state.hands(playerIndex).find(meta(_).urgent) match
			case Some(urgent) =>
				// If they have an urgent discard, they can't play a connecting card.
				if meta(urgent).status == CardStatus.CalledToDiscard then
					List()
				// If they have an urgent playable, they can only play that card.
				else
					List(urgent)
			case _ => common.obviousPlayables(game, playerIndex).toList

		playables.foldLeft(acc): (acc, o) =>
			// Only consider playing the leftmost of similarly-possible cards
			if playables.exists(p => p > o && common.thoughts(p).possible == common.thoughts(o).possible) then
				acc
			else
				common.thoughts(o).id(infer = true) match
					case Some(id) => (o, Identity(id.suitIndex, id.rank + 1)) +: acc
					case None => common.thoughts(o).inferred.map(i => o -> Identity(i.suitIndex, i.rank + 1)) ++: acc

def refPlay(prev: Reactor, game: Reactor, action: ClueAction) =
	val hand = game.state.hands(action.target)
	val newlyTouched = action.list.filter(!prev.state.deck(_).clued)
	val target = newlyTouched.map(game.common.refer(prev, hand, _, left = true)).max

	if game.isBlindPlaying(target) then
		Log.warn("targeting an already known playable!")
		(None, game)
	else if game.meta(target).status == CardStatus.CalledToDiscard then
		Log.warn("targeting a card called to discard!")
		(None, game)
	else
		targetPlay(game, action, target, urgent = false, stable = true)


def targetPlay(game: Reactor, action: ClueAction, target: Int, urgent: Boolean = false, stable: Boolean = true) =
	val state = game.state
	val holder = state.holderOf(target)
	val possibleConns = delayedPlays(game, action.giver, holder)

	val newInferred = game.common.thoughts(target).inferred.filter(i => state.isPlayable(i) || possibleConns.exists(_._2 == i))
	var (newCommon, newMeta) = (game.common, game.meta)

	state.deck(target).id().foreach: id =>
		possibleConns.find(_._2 == id).foreach: (connOrder, _) =>
			newCommon = newCommon.withThought(connOrder)(_.copy(
				oldInferred = Some(newCommon.thoughts(connOrder).inferred),
				inferred = IdentitySet.single(id.prev.get)
			))
			val meta = newMeta(connOrder)
			newMeta = newMeta.updated(connOrder, meta.copy(
				urgent = true,
				status = CardStatus.CalledToPlay,
				by = Some(action.giver))
			.reason(state.turnCount))

			Log.info(s"updating connecting $connOrder as ${state.logId(id.prev.get)} to be urgent")

	val reset = newInferred.isEmpty
	newCommon = newCommon.withThought(target)(t => t.copy(
		oldInferred = Some(t.inferred),
		inferred = newInferred,
		infoLock = Some(newInferred)
	))

	newMeta = newMeta.updated(target, newMeta(target).reason(state.turnCount))

	if reset || !state.hasConsistentInfs(newCommon.thoughts(target)) then
		newCommon = newCommon.withThought(target)(_.resetInferences())
		Log.warn(s"target $target was reset!")

		val newGame = game.copy(common = newCommon, meta = newMeta)
		val interp = Option.when(stable && newCommon.orderKt(newGame, target))(ClueInterp.Stall)
		(interp, newGame)
	else
		newMeta = newMeta.updated(target, newMeta(target).copy(
			status = CardStatus.CalledToPlay,
			by = Some(action.giver),
			focused = true,
			urgent = urgent
		))

		Log.info(s"targeting play $target (${state.names(holder)}), infs ${newCommon.strInfs(state, target)}${if urgent then ", urgent" else ""}")
		(Some(ClueInterp.Play), game.copy(common = newCommon, meta = newMeta))

def targetDiscard(game: Reactor, action: ClueAction, target: Int, urgent: Boolean = false) =
	val meta = game.meta(target)
	val state = game.state

	val newGame = game.copy(
		common = game.common.withThought(target)(t => t.copy(
			inferred = t.inferred.filter(!state.isCritical(_))
		)),
		meta = game.meta.updated(target, meta.copy(
			status = CardStatus.CalledToDiscard,
			by = Some(action.giver),
			urgent = urgent
		).reason(state.turnCount))
	)

	Log.info(s"targeting discard $target (${state.names(action.target)}), infs ${game.common.strInfs(state, target)}${if urgent then ", urgent" else ""}")
	newGame

def refDiscard(prev: Reactor, game: Reactor, action: ClueAction, stall: Boolean): (Option[ClueInterp], Reactor) =
	val state = game.state
	val ClueAction(giver = giver, target = receiver, list = list, clue = clue) = action
	val hand = state.hands(receiver)
	val newlyTouched = list.filter(!prev.state.deck(_).clued)

	var newCommon = game.common
	var newMeta = game.meta

	hand.filter(!prev.state.deck(_).clued).minOption match
		case Some(lockOrder) if list.contains(lockOrder) =>
			if stall && state.nextPlayerIndex(receiver) == giver then
				Log.info("stall to Cathy's lock card!")
				(Some(ClueInterp.Stall), game)
			else
				Log.info("locked!")

				if clue.kind == ClueKind.Rank && state.includesVariant(PINKISH) then
					newCommon = newCommon.withThought(lockOrder)(t => t.copy(inferred = t.inferred.filter(_.rank == clue.value)))
					newMeta = newMeta.updated(lockOrder, newMeta(lockOrder).copy(focused = true))

				for order <- hand if !state.deck(order).clued do // && game.meta(order).status == CardStatus.None) {
					newMeta = newMeta.updated(order, newMeta(order).copy(
						status = CardStatus.ChopMoved,
						by = Some(giver)
					).reason(state.turnCount))
				(Some(ClueInterp.Lock), game.copy(common = newCommon, meta = newMeta))

		case _ =>
			val focus = newlyTouched.max
			val focusPos = hand.indexOf(focus)
			val targetIndex = hand.zipWithIndex.indexWhere((o, i) => i > focusPos && !state.deck(o).clued)
			Log.info(s"ref discard on ${state.names(receiver)}'s slot ${targetIndex + 1}")

			val meta = newMeta(hand(targetIndex))
			newMeta = newMeta
				.updated(hand(targetIndex), meta.copy(
					status = CardStatus.CalledToDiscard,
					by = Some(giver))
					.reason(state.turnCount))
				.updated(focus, newMeta(focus).copy(focused = true))
			(Some(ClueInterp.Discard), game.copy(common = newCommon, meta = newMeta))
