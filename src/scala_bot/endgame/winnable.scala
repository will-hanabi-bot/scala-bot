package scala_bot.endgame

import scala_bot.basics._
import scala_bot.utils._
import scala.util.chaining.scalaUtilChainingOps
import java.time.Instant
// import scala_bot.logger.Log

extension (solver: EndgameSolver)
	def cluelessWinnable(state: State, playerTurn: Int, deadline: Instant, depth: Int): Option[PerformAction] =
		lazy val hash = state.hashCode()

		lazy val winnablePlay = state.hands(playerTurn).view.collect {
			case order if state.deck(order).id().exists(state.isPlayable) =>
				PerformAction.Play(order)
		}
		.find { action =>
			val newState = advanceState(state, action, playerTurn, draw = None)
			solver.cluelessWinnable(newState, state.nextPlayerIndex(playerTurn), deadline, depth + 1).isDefined
		}

		lazy val winnableStall = Option.when(state.canClue) {
			val action = PerformAction.Rank(0, 0)
			val newState = advanceState(state, action, playerTurn, draw = None)
			val winnable = solver.cluelessWinnable(newState, state.nextPlayerIndex(playerTurn), deadline, depth + 1).isDefined

			Option.when(winnable)(action)
		}.flatten

		lazy val discardable = state.hands(playerTurn).find(state.deck(_).id().isEmpty)

		lazy val winnableDc = discardable.flatMap { order =>
			val action = PerformAction.Discard(order)
			val newState = advanceState(state, action, playerTurn, draw = None)
			val winnable = solver.cluelessWinnable(newState, state.nextPlayerIndex(playerTurn), deadline, depth + 1).isDefined

			Option.when(winnable)(action)
		}

		if (state.score == state.maxScore)
			Some(PerformAction.Play(99))

		else if (solver.cluelessCache.contains(hash))
			solver.cluelessCache(hash)

		else if (Instant.now().isAfter(deadline))
			None

		else if (unwinnableState(state, playerTurn, depth))
			solver.cluelessCache = solver.cluelessCache.updated(hash, None)
			None

		else
			winnablePlay.orElse(winnableStall).orElse(winnableDc)

	def winnableSimpler(state: State, playerTurn: Int, remaining: RemainingMap, deadline: Instant, depth: Int): Boolean =
		if (state.score == state.maxScore)
			return true

		val hash = state.hashCode()

		if (solver.simplerCache.contains(hash))
			return solver.simplerCache(hash)

		if (unwinnableState(state, playerTurn, depth))
			solver.simplerCache = solver.simplerCache.updated(hash, false)
			return false

		val initial = (false, Option.when(state.canClue)(PerformAction.Rank(0, 0)).toList)
		val possibleActions = state.hands(playerTurn).foldRight(initial) { (order, acc) =>
			val (foundDc, actions) = acc
			state.deck(order).id().match {
				case None if !foundDc =>
					(true, PerformAction.Discard(order) +: actions)

				case Some(id) if state.isPlayable(id) =>
					(foundDc, PerformAction.Play(order) +: actions)

				case Some(id) if !foundDc && state.isBasicTrash(id) =>
					(true, PerformAction.Discard(order) +: actions)

				case _ => acc
			}
		}._2
		.sortBy {
			case PerformAction.Play(_) => 0
			case _ => 1
		}

		val winnable = possibleActions.exists(winnableIf(state, playerTurn, _, remaining, deadline, depth) match {
			case SimpleResult.Unwinnable => false
			case _ => true
		})
		solver.simplerCache = solver.simplerCache.updated(hash, winnable)
		winnable

	def winnableIf(state: State, playerTurn: Int, perform: PerformAction, remaining: RemainingMap, deadline: Instant, depth: Int): SimpleResult =
		val hash = s"${state.hashCode()},$playerTurn,$perform,${remaining.toList.sortBy(_._1.toOrd)}"

		// Log.highlight(Console.GREEN, s"${indent(depth)}checking if $perform is winning ${state.turnCount} ${solver.simplerCache.size}")

		if (solver.ifCache.contains(hash))
			solver.ifCache(hash)

		else if (Instant.now.isAfter(deadline))
			SimpleResult.Unwinnable

		else if (state.cardsLeft == 0 || perform.isClue)
			val newState = advanceState(state, perform, playerTurn, draw = None)
			// println(s"${indent(depth)}no draw, stacks ${newState.playStacks}")
			val winnable = winnableSimpler(newState, state.nextPlayerIndex(playerTurn), remaining, deadline, depth + 1)

			// println(s"${indent(depth)}winnable? $winnable")

			val res = if (winnable) SimpleResult.AlwaysWinnable else SimpleResult.Unwinnable
			solver.ifCache = solver.ifCache.updated(hash, res)
			res

		else
			val winnableDraws = remaining.keys.filter { id =>
				// TODO: Check whether this should be +1?
				val draw = Card(id.suitIndex, id.rank, state.nextCardOrder + 1, state.turnCount)
				val newState = advanceState(state, perform, playerTurn, Some(draw))
				// println(s"${indent(depth)}drew, stacks ${newState.playStacks}")
				val newRemaining = remaining.rem(id)

				winnableSimpler(newState, state.nextPlayerIndex(playerTurn), newRemaining, deadline, depth + 1)
			}

			// println(s"${indent(depth)}remaining: $remaining, winnable draws: $winnableDraws")

			val res =
				if (winnableDraws.isEmpty)
					SimpleResult.Unwinnable
				else
					SimpleResult.WinnableWithDraws(winnableDraws.toList)
			solver.ifCache = solver.ifCache.updated(hash, res)
			res

def advanceState(state: State, perform: PerformAction, playerIndex: Int, draw: Option[Card]) =
	def removeAndDraw(playerIndex: Int, order: Int)(s: State) =
		val newCardOrder = s.nextCardOrder
		val newHand = newCardOrder +: s.hands(playerIndex).filter(_ != order)

		s.copy(
			hands = s.hands.updated(playerIndex, newHand),
			nextCardOrder = newCardOrder + (if (s.cardsLeft > 0) 1 else 0),
			cardsLeft = 0.max(s.cardsLeft - 1),
			endgameTurns = s.endgameTurns match {
				case Some(endgameTurns) => Some(endgameTurns - 1)
				case None if s.cardsLeft == 1 => Some(s.numPlayers)
				case _ => None
			}
		)
		.when(_.deck.lift(newCardOrder).flatMap(_.id()).isEmpty) { st =>
			val deck = st.deck
			val newCard = draw.getOrElse(Card(-1, -1, newCardOrder, st.turnCount))
			if (deck.length == newCardOrder)
				st.copy(deck = deck :+ newCard)
			else
				st.copy(deck = deck.updated(newCardOrder, newCard))
		}

	perform match {
		case PerformAction.Play(target) =>
			(state.deck(target).id() match {
				case None =>
					state.copy(strikes = state.strikes + 1)
				case Some(id) =>
					if (state.isPlayable(id))
						state.withPlay(id)
					else
						state.copy(strikes = state.strikes + 1).withDiscard(id, target)
			})
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
	}
