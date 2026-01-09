package scala_bot.utils

import scala_bot.basics._

inline def loop(
	inline start: Int,
	inline cond: Int => Boolean,
	inline advance: Int => Int
)(inline body: Int => Unit) =
	var i = start
	while cond(i) do
		body(i)
		i = advance(i)

inline def loopIf(
	inline start: Int,
	inline cond: Int => Boolean,
	inline advance: Int => Int,
	inline only: Int => Boolean = _ => true
)(inline body: Int => Unit) =
	var i = start
	while cond(i) do
		if only(i) then
			body(i)
		i = advance(i)

extension [A](a: A)
	def cond(condition: A => Boolean)(ifTrue: A => A)(ifFalse: A => A): A =
		if condition(a) then ifTrue(a) else ifFalse(a)

	inline def when(inline condition: A => Boolean)(inline f: A => A): A =
		if condition(a) then f(a) else a

	def matches(cond: PartialFunction[A, Boolean]): Boolean =
		cond.applyOrElse(a, _ => false)

extension[A](it: Iterator[A])
	def existsM(cond: PartialFunction[A, Boolean]): Boolean =
		it.fastExists(_.matches(cond))

	inline def fastForeach(inline f: A => Unit): Unit =
		while it.hasNext do
			f(it.next)

	inline def fastForall(inline f: A => Boolean): Boolean =
		var satisfied = true

		while it.hasNext && satisfied do
			if !f(it.next) then
				satisfied = false
		satisfied

	inline def fastCount(inline f: A => Boolean): Int =
		var res = 0
		while it.hasNext do
			if f(it.next) then
				res += 1
		res

	inline def fastExists(inline f: A => Boolean): Boolean =
		var exists = false
		while it.hasNext && !exists do
			if f(it.next) then
				exists = true
		exists

	def foldLeftOpt[B](initial: B)(reducer: (acc: B, curr: A) => Either[B, B]): B =
		var acc = initial

		while it.hasNext do
			reducer(acc, it.next) match
				case Left(res) =>   return res
				case Right(next) => acc = next
		acc

	def findSome[B](f: A => Option[B]): Option[B] =
		while it.hasNext do
			val res = f(it.next)
			if res.isDefined then
				return res

		return None

	def summing[N](f: A => N)(using numeric: Numeric[N]) =
		var res = numeric.zero
		while it.hasNext do
			res = numeric.plus(res, f(it.next))
		res

	def maximizing[N](default: N)(f: A => N)(using numeric: Numeric[N]) =
		var currMax = default
		while it.hasNext do
			currMax = numeric.max(currMax, f(it.next))
		currMax

extension [A](a: Iterable[A])
	def existsM(cond: PartialFunction[A, Boolean]): Boolean =
		a.iterator.fastExists(_.matches(cond))

	inline def fastForeach(inline f: A => Unit): Unit =
		a.iterator.fastForeach(f)

	inline def fastForall(inline f: A => Boolean): Boolean =
		a.iterator.fastForall(f)

	inline def fastCount(inline f: A => Boolean): Int =
		a.iterator.fastCount(f)

	inline def fastExists(inline f: A => Boolean): Boolean =
		a.iterator.fastExists(f)

	def foldLeftOpt[B](initial: B)(reducer: (acc: B, curr: A) => Either[B, B]): B =
		a.iterator.foldLeftOpt(initial)(reducer)

	def findSome[B](f: A => Option[B]): Option[B] =
		a.iterator.findSome(f)

	def summing[N](f: A => N)(using numeric: Numeric[N]) =
		val it = a.iterator
		var res = numeric.zero

		while it.hasNext do
			res = numeric.plus(res, f(it.next))

		res

	def maximizing[N](default: N)(f: A => N)(using numeric: Numeric[N]) =
		val it = a.iterator
		var currMax = default

		while it.hasNext do
			currMax = numeric.max(currMax, f(it.next))

		currMax

extension [A](seq: IndexedSeq[A])
	inline def fastMap[B](inline f: A => B): IndexedSeq[B] =
		var res = IndexedSeq.empty[B]
		var i = 0

		while i < seq.length do
			res = res :+ f(seq(i))
			i += 1
		res

	inline def fastFilter(inline f: A => Boolean): IndexedSeq[A] =
		var res = IndexedSeq.empty[A]
		var i = 0

		while i < seq.length do
			if f(seq(i)) then
				res = res :+ seq(i)
			i += 1
		res

extension [A](seq: IndexedSeq[Int])
	inline def fastSum: Int =
		var i = 0
		var sum = 0

		while i < seq.length do
			sum += seq(i)
			i += 1
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
	state.hands.zipWithIndex.flatMap: (hand, playerIndex) =>
		hand.filter { order =>
			order != excludeOrder &&
			player.thoughts(order).matches(
				id,
				infer,
				symmetric = symmetric || playerIndex == player.playerIndex,
				assume = assume
			) &&
			!player.links.exists: l =>
				val orders = l.getOrders
				orders.contains(excludeOrder) && orders.contains(order)
			&&
			cond(playerIndex, order)
		}

def clueToPerform(clue: Clue): PerformAction.Colour | PerformAction.Rank =
	val Clue(kind, value, target) = clue
	kind match
		case ClueKind.Colour => PerformAction.Colour(target, value)
		case ClueKind.Rank   => PerformAction.Rank(target, value)

def performToAction(state: State, perform: PerformAction, playerIndex: Int, deck: Option[IndexedSeq[Identity]] = None) =
	val deckId = (order: Int) =>
		state.deck(order).id().orElse(deck.flatMap(d => d(order).id()))

	val clueTouched = (orders: Seq[Int], clue: BaseClue) =>
		orders.filter(deckId(_).map(state.variant.cardTouched(_, clue)).getOrElse(false))

	perform match
		case PerformAction.Play(target) =>
			deckId(target) match
				case Some(id) =>
					if state.isPlayable(id) then
						PlayAction(playerIndex, target, id.suitIndex, id.rank)
					else
						DiscardAction(playerIndex, target, id.suitIndex, id.rank, true)
				case None =>
					DiscardAction(playerIndex, target, -1, -1, true)

		case PerformAction.Discard(target) =>
			deckId(target) match
				case Some(id) =>
					DiscardAction(playerIndex, target, id.suitIndex, id.rank, false)
				case None =>
					DiscardAction(playerIndex, target, -1, -1, false)

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

def clueToAction(state: State, clue: Clue, giver: Int): ClueAction =
	val list = state.clueTouched(state.hands(clue.target), clue)
	ClueAction(giver, clue.target, list, clue.toBase)

/** Returns all player indices between the start (inclusive) and end (exclusive) in play order. */
def playersUntil(numPlayers: Int, start: Int, `end`: Int) =
	Iterator.iterate(start)(i => (i + 1) % numPlayers).takeWhile(_ != `end`)

def inBetween(numPlayers: Int, playerIndex: Int, start: Int, `end`: Int) =
	playersUntil(numPlayers, start, `end`).contains(playerIndex)
