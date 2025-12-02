package scala_bot.utils

import scala_bot.basics._

extension [A](a: A)
	def cond(condition: A => Boolean)(ifTrue: A => A)(ifFalse: A => A): A =
		if (condition(a)) ifTrue(a) else ifFalse(a)

	def when(condition: A => Boolean)(f: A => A): A =
		if (condition(a)) f(a) else a

	def unless(condition: Boolean)(f: A => A): A =
		if (!condition) f(a) else a

	def matches(cond: PartialFunction[A, Boolean]): Boolean =
		cond.applyOrElse(a, _ => false)

extension [A](a: Iterable[A])
	def existsM(cond: PartialFunction[A, Boolean]): Boolean =
		a.exists(_.matches(cond))

extension [A](seq: IndexedSeq[A])
	inline def fastMap[B](inline f: A => B): IndexedSeq[B] =
		var res = IndexedSeq.empty[B]
		var i = 0

		while (i < seq.length) {
			res = res :+ f(seq(i))
			i += 1
		}
		res

	inline def fastFilter(inline f: A => Boolean): IndexedSeq[A] =
		var res = IndexedSeq.empty[A]
		var i = 0

		while (i < seq.length) {
			if (f(seq(i)))
				res = res :+ seq(i)
			i += 1
		}
		res

	inline def fastCount(inline f: A => Boolean): Int =
		var res = 0
		var i = 0

		while (i < seq.length) {
			if (f(seq(i)))
				res += 1
			i += 1
		}
		res

	inline def fastForall(inline f: A => Boolean): Boolean =
		var i = 0
		var satisfied = true

		while (i < seq.length && satisfied) {
			if (!f(seq(i)))
				satisfied = false
			i += 1
		}
		satisfied

	inline def fastExists(inline f: A => Boolean): Boolean =
		var i = 0
		var exists = false

		while (i < seq.length && !exists) {
			if (f(seq(i)))
				exists = true
			i += 1
		}
		exists

extension [A](seq: IndexedSeq[Int])
	inline def fastSum: Int =
		var i = 0
		var sum = 0

		while (i < seq.length) {
			sum += seq(i)
			i += 1
		}
		sum

def visibleFind(
	state: State,
	player: Player,
	id: Identity,
	infer: Boolean = false,
	symmetric: Boolean = false,
	assume: Boolean = false,
	excludeOrder: Int = -1,
	cond: (playerIndex: Int, order: Int) => Boolean = (_, _) => true
) =
	state.hands.zipWithIndex.flatMap { (hand, playerIndex) =>
		hand.filter { order =>
			order != excludeOrder &&
			player.thoughts(order).matches(
				id,
				infer,
				symmetric = symmetric || playerIndex == player.playerIndex,
				assume = assume
			) &&
			!player.links.exists { l =>
				val orders = l.getOrders
				orders.contains(excludeOrder) && orders.contains(order)
			} &&
			cond(playerIndex, order)
		}
	}

def clueToPerform(clue: Clue): PerformAction.Colour | PerformAction.Rank =
	val Clue(kind, value, target) = clue
	kind match {
		case ClueKind.Colour => PerformAction.Colour(target, value)
		case ClueKind.Rank => PerformAction.Rank(target, value)
	}

def performToAction(state: State, perform: PerformAction, playerIndex: Int, deck: Option[IndexedSeq[Identity]] = None) =
	val deckId = (order: Int) =>
		state.deck(order).id().orElse(deck.flatMap(d => d(order).id()))

	val clueTouched = (orders: Seq[Int], clue: BaseClue) =>
		orders.filter(deckId(_).map(state.variant.cardTouched(_, clue)).getOrElse(false))

	perform match {
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

def clueToAction(state: State, clue: Clue, giver: Int): ClueAction =
	val list = state.clueTouched(state.hands(clue.target), clue)
	ClueAction(giver, clue.target, list, clue.toBase)

/** Returns all player indices between the start (inclusive) and end (exclusive) in play order. */
def playersUntil(numPlayers: Int, start: Int, `end`: Int) =
	Iterator.iterate(start)(i => (i + 1) % numPlayers).takeWhile(_ != `end`)

def inBetween(numPlayers: Int, playerIndex: Int, start: Int, `end`: Int) =
	playersUntil(numPlayers, start, `end`).contains(playerIndex)
