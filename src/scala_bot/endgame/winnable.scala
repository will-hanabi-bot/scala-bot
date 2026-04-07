package scala_bot.endgame

import scala_bot.basics._
import scala_bot.utils._
import java.time.Instant
// import scala_bot.logger.Log

extension[G <: Game] (solver: EndgameSolver[G])
	def cluelessWinnable(state: State, playerTurn: Int, deadline: Instant, depth: Int): Option[PerformAction] =
		if state.score == state.maxScore then
			Some(PerformAction.Play(99))

		else if Instant.now().isAfter(deadline) then
			None

		else if unwinnableState(state, playerTurn, depth) then
			None

		else
			def actionWinnable(perform: PerformAction) =
				val newState = advanceState(state, perform, playerTurn, draw = None)
				solver.cluelessWinnable(newState, state.nextPlayerIndex(playerTurn), deadline, depth + 1).isDefined

			state.hands(playerTurn).collectFirst:
				case order if state.deck(order).id().exists(state.isPlayable) && actionWinnable(PerformAction.Play(order)) =>
					PerformAction.Play(order)
			.orElse:
				if !state.canClue then None else
					val action = PerformAction.Rank(0, 0)
					Option.when(actionWinnable(action))(action)
			.orElse:
				for
					discardable <- state.hands(playerTurn).find(state.deck(_).id().isEmpty)
					action = PerformAction.Discard(discardable) if actionWinnable(action)
				yield
					action

	def winnableSimpler(state: State, playerTurn: Int, remaining: RemainingMap, deadline: Instant, depth: Int): Boolean =
		if state.score == state.maxScore then
			return true

		if unwinnableState(state, playerTurn, depth) then
			return false

		val initial = (false, if !state.canClue then Nil else List(PerformAction.Rank(0, 0)))
		val possibleActions = state.hands(playerTurn).foldRight(initial) { (order, acc) =>
			val (foundDc, actions) = acc
			state.deck(order).id() match
				case None if !foundDc =>
					(true, PerformAction.Discard(order) +: actions)

				case Some(id) if state.isPlayable(id) =>
					(foundDc, PerformAction.Play(order) +: actions)

				case Some(id) if !foundDc && state.isBasicTrash(id) =>
					(true, PerformAction.Discard(order) +: actions)

				case _ => acc
		}
		._2
		.partition:
			case PerformAction.Play(_) => true
			case _ => false
		.pipe: (plays, discards) =>
			plays ++ discards

		possibleActions.exists(winnableIf(state, playerTurn, _, remaining, deadline, depth) != SimpleResult.Unwinnable)

	def winnableIf(state: State, playerTurn: Int, perform: PerformAction, remaining: RemainingMap, deadline: Instant, depth: Int): SimpleResult =
		if Instant.now.isAfter(deadline) then
			SimpleResult.Unwinnable

		else if state.cardsLeft == 0 || perform.isClue then
			val newState = advanceState(state, perform, playerTurn, draw = None)
			// println(s"${indent(depth)}no draw, stacks ${newState.playStacks}")
			val winnable = winnableSimpler(newState, state.nextPlayerIndex(playerTurn), remaining, deadline, depth + 1)

			// println(s"${indent(depth)}winnable? $winnable")

			if winnable then SimpleResult.AlwaysWinnable else SimpleResult.Unwinnable

		else
			val remIds = remaining.keys
			val (trashIds, otherIds) = remIds.partition(state.isBasicTrash)

			def isWinnable(drawId: Identity) =
				val draw = Card(drawId.suitIndex, drawId.rank, state.nextCardOrder + 1, state.turnCount)
				val newState = advanceState(state, perform, playerTurn, Some(draw))
				// println(s"${indent(depth)}drew, stacks ${newState.playStacks}")
				val newRemaining = remaining.rem(drawId)

				winnableSimpler(newState, state.nextPlayerIndex(playerTurn), newRemaining, deadline, depth + 1)

			val trashWinnable = trashIds.isEmpty || isWinnable(trashIds.head)
			val otherWinnable = otherIds.filter(isWinnable)
			// println(s"${indent(depth)}remaining: $remaining, winnable draws: $winnableDraws")

			if !trashWinnable && otherWinnable.isEmpty then
				SimpleResult.Unwinnable
			else if trashWinnable then
				SimpleResult.WinnableWithDraws((trashIds ++ otherWinnable).toList)
			else
				SimpleResult.WinnableWithDraws(otherWinnable.toList)

def advanceState(state: State, perform: PerformAction, playerIndex: Int, draw: Option[Card]) =
	def removeAndDraw(playerIndex: Int, order: Int)(s: State) =
		val newCardOrder = s.nextCardOrder
		val newHand = newCardOrder +: s.hands(playerIndex).filter(_ != order)

		s.copy(
			hands = s.hands.updated(playerIndex, newHand),
			nextCardOrder = newCardOrder + (if s.cardsLeft > 0 then 1 else 0),
			cardsLeft = 0.max(s.cardsLeft - 1),
			endgameTurns = s.endgameTurns match
				case Some(endgameTurns)       => Some(endgameTurns - 1)
				case None if s.cardsLeft == 1 => Some(s.numPlayers)
				case _                        => None
		)
		.when(_.deck.lift(newCardOrder).flatMap(_.id()).isEmpty): st =>
			val deck = st.deck
			val newCard = draw.getOrElse(Card(-1, -1, newCardOrder, st.turnCount))
			if deck.length == newCardOrder then
				st.copy(deck = deck :+ newCard)
			else
				st.copy(deck = deck.updated(newCardOrder, newCard))

	perform match
		case PerformAction.Play(target) =>
			(state.deck(target).id() match
				case None =>
					state.copy(strikes = state.strikes + 1)
				case Some(id) =>
					if state.isPlayable(id) then
						state.withPlay(id)
					else
						state.copy(strikes = state.strikes + 1).withDiscard(id, target)
			)
			.pipe(removeAndDraw(playerIndex, target))

		case PerformAction.Discard(target) =>
			state.deck(target).id().fold(state)(state.withDiscard(_, target))
				.regainClue
				.pipe(removeAndDraw(playerIndex, target))

		case _ =>
			state.copy(
				clueTokens = state.clueTokens - 1,
				endgameTurns = state.endgameTurns.map(_ - 1)
			)
