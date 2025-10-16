package scala_bot.utils

import scala_bot.basics._

extension [A](a: A) {
	def cond(condition: A => Boolean)(ifTrue: A => A)(ifFalse: A => A): A =
		if (condition(a)) ifTrue(a) else ifFalse(a)

	def when(condition: A => Boolean)(f: A => A): A =
		if (condition(a)) f(a) else a

	def unless(condition: Boolean)(f: A => A): A =
		if (!condition) f(a) else a
	}

def visibleFind(
	state: State,
	player: Player,
	id: Identity,
	infer: Boolean = false,
	symmetric: Boolean = false,
	assume: Boolean = false,
	cond: (Int, Int) => Boolean = (_, _) => true
) =
	state.hands.zipWithIndex.flatMap { (hand, playerIndex) =>
		hand.filter { order =>
			player.thoughts(order).matches(
				id,
				infer,
				symmetric = symmetric || playerIndex == player.playerIndex,
				assume = assume
			) && cond(playerIndex, order)
		}
	}

def clueToPerform(clue: Clue) =
	val Clue(kind, value, target) = clue
	kind match {
		case ClueKind.Colour => PerformAction.Colour(target, value)
		case ClueKind.Rank => PerformAction.Rank(target, value)
	}

def performToAction(state: State, action: PerformAction, playerIndex: Int, deck: Option[IndexedSeq[Identity]] = None) =
	val deckId = (order: Int) =>
		state.deck(order).id().orElse(deck.flatMap(d => d(order).id()))

	val clueTouched = (orders: Seq[Int], clue: BaseClue) =>
		orders.filter(deckId(_).map(state.variant.cardTouched(_, clue)).getOrElse(false))

	action match {
		case PerformAction.Play(target) =>
			deckId(target) match {
				case Some(id) =>
					if (state.isPlayable(id))
						PlayAction(playerIndex, target, id.suitIndex, id.rank)
					else
						DiscardAction(playerIndex, target, id.suitIndex, id.rank, true)
				case None =>
					DiscardAction(playerIndex, target, -1, -1, true)
			}
		case PerformAction.Discard(target) =>
			deckId(target) match {
				case Some(id) =>
					DiscardAction(playerIndex, target, id.suitIndex, id.rank, false)
				case None =>
					DiscardAction(playerIndex, target, -1, -1, false)
			}
		case PerformAction.Colour(target, value) =>
			val clue = BaseClue(ClueKind.Colour, value)
			val list = clueTouched(state.hands(target), clue)
			ClueAction(playerIndex, target, list, clue)

		case PerformAction.Rank(target, value) =>
			val clue = BaseClue(ClueKind.Rank, value)
			val list = clueTouched(state.hands(target), clue)
			ClueAction(playerIndex, target, list, clue)

		case PerformAction.Terminate(target, value) =>
			GameOverAction(value, target)
	}

/** Returns all player indices between the start (inclusive) and end (exclusive) in play order. */
def playersUntil(numPlayers: Int, start: Int, `end`: Int) =
	Iterator.iterate(start)(i => (i + 1) % numPlayers).takeWhile(_ != `end`)
