package scala_bot.endgame

import scala_bot.basics._
import scala_bot.fraction.Frac
import scala_bot.logger.{Log, Logger, LogLevel}
import scala_bot.utils._

import java.time.Instant
import scala.util.boundary, boundary.break
import java.time.Duration

type WinnableResult = Either[String, (Seq[PerformAction], Frac)]

enum SimpleResult:
	case AlwaysWinnable
	case WinnableWithDraws(draws: List[Identity])
	case Unwinnable

val UNWINNABLE = Left("")
val TIMEOUT = Left("timeout")

def indent(depth: Int) =
	(0 until depth).map(_ => "  ").mkString

type RemainingMap = Map[Identity, Int]

extension (remaining: RemainingMap)
	def hash: Int =
		remaining.foldLeft(0):
			case (acc, (id, missing)) =>
				val ord = id.toOrd
				acc + (ord * ord * ord * ord) + missing

	def rem(id: Identity) =
		remaining(id) match
			case 1 => remaining.removed(id)
			case m => remaining.updated(id, m - 1)

	def fmt2(state: State): String =
		remaining.map((id, entry) => s"${state.logId(id)} (missing $entry)").mkString(", ")

def findRemainingIds(game: Game) =
	val state = game.state

	val initial = (Map[Identity, Int](), List[(Int, Option[Identity])]())
	val (seenIds, ownIds) = (0 until state.numPlayers).foldLeft(initial): (a, i) =>
		state.hands(i).foldLeft(a): (acc, order) =>
			val (seenIds, ownIds) = acc
			// Identify all the cards we know for sure
			game.me.thoughts(order).id() match
				case Some(id) =>
					val newSeen = seenIds.updated(id, seenIds.getOrElse(id, 0) + 1)
					val newOwn = if i == state.ourPlayerIndex then (order, Some(id)) +: ownIds else ownIds
					(newSeen, newOwn)

				case None if i == state.ourPlayerIndex =>
					val newOwn = (order, None) +: ownIds
					(seenIds, newOwn)

				case None => acc

	val remainingIds = (
		for
			id <- state.variant.allIds
			missing = state.cardCount(id.toOrd) - state.baseCount(id.toOrd) - seenIds.getOrElse(id, 0)
				if missing > 0
		yield
			(id, missing)
	).toMap

	(remainingIds, ownIds)

case class GameArr(
	prob: Frac,
	remaining: RemainingMap,
	drew: Option[Identity]
)

case class Arrangement(
	ids: Vector[Identity],
	prob: Frac,
	remaining: RemainingMap
)

case class EndgameSolver[G <: Game](
	var successRate: Map[Int, Map[PerformAction, (Frac, Int)]] = Map(),
	/** If true, only picks a few random permutations of trash instead of trying all of them. May make winrate slightly off. */
	monteCarlo: Boolean = true,
	timeout: Duration = Duration.ofSeconds(2)
):
	/** Returns the action with the highest winrate by brute-forcing permutations of the deck.
	  * Will not solve if there are at least 4 unique ids we haven't seen yet.
	  * Terminates early and returns a partially-computed result if solving takes longer than 2 seconds.
	  *
	  * @param game        The game state to solve from.
	  * @param onlyAction  Whether to only find the winrate for a particular action.
	  * @return A tuple Right(bestAction, winrate) if a non-zero winrate is found, otherwise Left(reason).
	  */
	def solve(game: G, onlyAction: Option[PerformAction] = None)(using ops: GameOps[G]): Either[String, (PerformAction, Frac)] =
		val state = game.state
		if state.score + 1 == state.maxScore then
			val winningPlay = state.ourHand.find:
				game.me.thoughts(_).id(infer = true).exists(state.isPlayable)

			if winningPlay.isDefined then
				return Right(PerformAction.Play(winningPlay.get), Frac.one)

		val start = Instant.now()
		val deadline = Instant.now().plus(timeout)
		val (remainingIds, ownIds) = findRemainingIds(game)

		if remainingIds.count((id, v) => state.isUseful(id) && v == state.cardCount(id.toOrd)) > 3 then
			val missingIds = remainingIds.keys.filter(state.isUseful).map(state.logId).mkString(",")
			return Left(s"couldn't find any $missingIds!")

		val level = Logger.level
		Logger.setLevel(LogLevel.Off)

		val assumedGame = ownIds.foldLeft(game):
			case (g, (order, id)) => id.fold(g)(g.withId(order, _))

		val linkedOrders = game.me.linkedOrders(state)
		val unknownOwn = ownIds.collect:
			case (order, id) if id.isEmpty => order

		val totalUnknown = state.cardsLeft + unknownOwn.length
		Log.info(s"unknown own $unknownOwn, cards left ${state.cardsLeft}")

		if totalUnknown == 0 then
			return winnable(assumedGame, state.ourPlayerIndex, remainingIds, deadline) match
				case Left(_) =>
					Logger.setLevel(level)
					Left("couldn't find a winning strategy")

				case Right((actions, winrate)) =>
					Logger.setLevel(level)
					Log.highlight(Console.MAGENTA, s"winnable! actions ${actions.map(_.fmt(assumedGame)).mkString(",")}")
					Log.info(s"solved in ${start.until(Instant.now()).toMillis()}ms")
					Right((actions.head, winrate))

		Log.info(s"remaining ids: ${remainingIds.fmt2(state)}")

		def impossibleArr(ids: Vector[Identity], id: Identity, order: Int, tryFilter: Boolean): Boolean =
			val thought = game.me.thoughts(order)

			if state.deck(order).id().exists(_ != id) || !thought.possible.contains(id) then
				return true

			if tryFilter && !game.validArr(id, order) then
				Log.info(s"arranging: conventionally unlikely to assign ${state.logId(id)} to $order, ignoring")
				return true

			// We cannot assign a trash id if it is linked for a non-trash id and all other orders are already trash
			val linked = Option.when(state.isBasicTrash(id) && linkedOrders.contains(order)):
				game.me.links.find: l =>
					val orders = l.getOrders
					l.promise.exists(state.isUseful) && orders.contains(order) && orders.forall: o =>
						o == order ||
						ids.zipWithIndex.exists((id2, i) => o == unknownOwn(i) && state.isBasicTrash(id2))
			.flatten

			linked match
				case Some(link) =>
					Log.info(s"arranging: $order is linked to ${link.getOrders} for ${state.logId(link.promise.get)} but others are already trash, can't assign ${state.logId(id)}")
					return true
				case None =>
					return false

		def expandArr(arrangement: Arrangement, tryFilter: Boolean): Iterable[Arrangement] =
			if Instant.now.isAfter(deadline) then
				return Seq(arrangement)

			val Arrangement(ids, prob, remaining) = arrangement
			val totalCards = remaining.values.sum

			remaining.collect:
				case (id, missing) if !impossibleArr(ids, id, unknownOwn(ids.length), tryFilter) =>
					val newRemaining = remaining.rem(id)
					val newIds = ids :+ id
					val newProb = prob * missing / totalCards
					Arrangement(newIds, newProb, newRemaining)

		val initialArr = Arrangement(Vector.empty, Frac.one, remainingIds)
		val allArrs = Iterator.iterate(Iterator.single(initialArr))(_.flatMap(expandArr(_, true))).drop(unknownOwn.length).next()
			.when(!_.hasNext): _ =>
				Log.highlight(Console.YELLOW, s"trying again with no filter")
				Iterator.iterate(Iterator.single(initialArr))(_.flatMap(expandArr(_, false))).drop(unknownOwn.length).next()

		if Instant.now.isAfter(deadline) then
			Logger.setLevel(level)
			return TIMEOUT

		val arrs =
			if monteCarlo then
				val (sumProb, grouped) = allArrs.foldLeft((Frac.zero, Map.empty[String, Arrangement])) { case ((sumProb, acc), arr) =>
					val nonTrashArr = arr.ids.map(id => if state.isBasicTrash(id) then "_" else state.logId(id)).mkString

					// Accumulate matching non-trash arrangements into one probability
					val newEntry = acc.get(nonTrashArr) match
						case Some(existingArr) => existingArr.copy(prob = existingArr.prob + arr.prob)
						case None => arr

					(sumProb + arr.prob, acc.updated(nonTrashArr, newEntry))
				}

				// Normalize all probabilities: some of the potential generated ones may be impossible, so the total prob may be less than 1.
				grouped.valuesIterator.map(a => a.copy(prob = a.prob / sumProb))
			else
				allArrs
		.toList
		.sortBy(a => -a.prob)
		.when(_.isEmpty): _ =>
			List(Arrangement(Vector.empty, Frac.one, remainingIds))

		Log.info(s"arrangements ${arrs.map(_._1.map(state.logId).mkString(","))}")

		val hypos = arrs.map:
			case Arrangement(ids, prob, remaining) =>
				val hypo = (0 until ids.length).foldLeft(assumedGame): (hypo, i) =>
					val order = unknownOwn(i)
					hypo.withId(order, ids(i))

				val actions = possibleActions(hypo, state.ourPlayerIndex, remaining, deadline)
					.when(_.isEmpty): _ =>
						possibleActions(hypo, state.ourPlayerIndex, remaining, deadline, infer = true)

				val gameArrs = genArrs(hypo, remaining, actions.forall(_._1.isClue))

				(hypo, actions, gameArrs, prob)

		if onlyAction.isDefined then
			val winrate = hypos.summing: (hypo, actions, gameArrs, prob) =>
				actions.find(_._1 == onlyAction.get) match
					case None => Frac.zero
					case Some(perform) =>
						val (undrawn, drawn) = gameArrs
						prob * actionWinrate(hypo, if perform._1.isClue then undrawn else drawn, perform, state.ourPlayerIndex, deadline)

			return Right((onlyAction.get, winrate))

		val allActions =
			for
				(_, actions, _, _) <- hypos
				(perform, _)       <- actions
			yield
				perform
		.distinct

		val (firstHypo, firstActions, firstArrs, firstProb) = hypos.head

		Log.info(s"all actions: ${allActions.map(_.fmt(game))}")
		Log.info(s"initial arrangement: ${firstHypo.state.ourHand.map(firstHypo.state.logId).mkString(",")} $firstProb")

		val initialActions =
			val init = optimizeFull(firstHypo, firstArrs, firstActions, state.ourPlayerIndex, deadline)

			// Apply probability of first arrangement
			init.map(e => e._1 -> e._2 * firstProb) ++
				// Add all invalid actions with winrate 0 at the end.
				allActions.filterNot(a => init.exists(_._1 == a)).map(_ -> Frac.zero)

		if initialActions.isEmpty then
			Logger.setLevel(level)
			return Left("couldn't find any winning actions")

		@annotation.tailrec
		def loop2(actions: Seq[(PerformAction, Frac)], best: (PerformAction, Frac)): (PerformAction, Frac) =
			if actions.isEmpty || Instant.now.isAfter(deadline) then
				best
			else
				val (_, bestWinrate) = best
				val (action, winrate) = actions.head

				Log.highlight(Console.GREEN, s"\ntesting action: ${action.fmt(game)}")

				@annotation.tailrec
				def inner(remHypos: Seq[(G, Seq[(PerformAction, Seq[Identity])], (Seq[GameArr], Seq[GameArr]), Frac)], winrate: Frac, remProb: Frac): Frac =
					if remHypos.isEmpty then
						winrate
					else if winrate + remProb < bestWinrate then
						Log.info(s"action ${action.fmt(game)} has winrate $winrate $remProb, can't add up to $bestWinrate")
						winrate
					else
						val (hypo, validActions, gameArrs, prob) = remHypos.head
						val hypoWinrate = validActions.find(_._1 == action).fold(Frac.zero): perform =>
							val (undrawn, drawn) = gameArrs
							Log.highlight(Console.MAGENTA, s"\narrangement ${hypo.state.ourHand.map(hypo.state.logId).mkString(",")} $prob")
							prob * actionWinrate(hypo, if perform._1.isClue then undrawn else drawn, perform, state.ourPlayerIndex, deadline)

						inner(remHypos.tail, winrate + hypoWinrate, remProb - prob)

				val totalWinrate = inner(hypos.tail, winrate, Frac.one - winrate)

				if totalWinrate == Frac.one then
					(action, totalWinrate)
				else if totalWinrate > bestWinrate then
					loop2(actions.tail, (action, totalWinrate))
				else
					loop2(actions.tail, best)

		if hypos.length > 1 then
			val (bestPerform, bestWinrate) = loop2(initialActions, initialActions.head)
			Logger.setLevel(level)

			if bestWinrate == Frac.zero then
				Left("couldn't find any winning actions")
			else
				Log.info(s"endgame winnable! ${bestPerform.fmt(game)} (winrate $bestWinrate)")
				Log.info(s"solved in ${start.until(Instant.now()).toMillis()}ms")
				Right((bestPerform, bestWinrate))

		else
			Logger.setLevel(level)
			val (bestAction, winrate) = initialActions.head
			Log.info(s"endgame winnable! ${bestAction.fmt(game)} (winrate $winrate)")
			Log.info(s"solved in ${start.until(Instant.now()).toMillis()}ms")
			Right(initialActions.head)

	def winnable(game: G, playerTurn: Int, remaining: RemainingMap, deadline: Instant, depth: Int = 0)(using ops: GameOps[G]): WinnableResult =
		val state = game.state

		if Instant.now().isAfter(deadline) then
			return TIMEOUT

		val trivialWin = triviallyWinnable(game, playerTurn)

		if trivialWin.isRight then
			Log.info(s"${indent(depth)}trivially winnable!")
			return trivialWin

		val viableClueless =
			for
				suitIndex <- 0 until state.variant.suits.length
				rank      <- state.playStacks(suitIndex) + 1 to state.maxRanks(suitIndex)
			yield
				Identity(suitIndex, rank)
		.forall: id =>
			val order = state.hands.flatten.find(game.common.thoughts(_).matches(id, infer = true))
			order.exists(state.deck(_).id().forall(_.matches(id)))

		if viableClueless then
			val cluelessState = state.hands.flatten.foldLeft(state): (acc, order) =>
				game.common.thoughts(order).id().fold(acc): id =>
					val newCard = acc.deck(order).copy(suitIndex = id.suitIndex, rank = id.rank)
					acc.copy(deck = acc.deck.updated(order, newCard))

			val cluelessWin = this.cluelessWinnable(cluelessState, playerTurn, deadline, depth)
			if cluelessWin.isDefined then
				Log.info(s"${indent(depth)}clueless winnable!")

				// Replace dummy action
				if cluelessWin.get == PerformAction.Rank(0, 0) then
					return Right((ops.findAllClues(game, playerTurn).take(1), Frac.one))
				else
					return Right((List(cluelessWin.get), Frac.one))

		val bottomDecked = remaining.nonEmpty && remaining.keys.forall(id => state.isCritical(id) && id.rank != 5)

		if bottomDecked || unwinnableState(state, playerTurn, depth) then
			return UNWINNABLE

		val performs = possibleActions(game, playerTurn, remaining, deadline, depth)

		if performs.isEmpty then
			return UNWINNABLE

		if state.score + 1 == state.maxScore then
			val winningPlay = performs.find: (p, _) =>
				p.matchesP:
					case PerformAction.Play(target) => state.isPlayable(state.deck(target).id().get)

			if winningPlay.isDefined then
				return Right((winningPlay.map(_._1).toList, Frac.one))

		val oneBDRLeft =
			state.score == state.maxScore - 2 &&
			state.playStacks.zipWithIndex.count((stack, i) => stack == state.maxRanks(i)) == state.variant.suits.length - 1 &&
			state.isCritical(state.playableSet.head)

		if oneBDRLeft then
			val bdrId = state.playableSet.head
			val unseen = visibleFind(state, game.me, bdrId).isEmpty

			val finalId = bdrId.next.get
			val known5 = visibleFind(state, game.me, finalId).exists: o =>
				game.players(state.holderOf(o)).thoughts(o).matches(finalId)

			val navigable = state.clueTokens + state.pace > state.numPlayers &&
				state.cardsLeft > 2

			if unseen && known5 && navigable then
				return Right((Seq(performs.last._1), Frac(state.cardsLeft - 1, state.cardsLeft)))

		Log.highlight(Console.GREEN, s"${indent(depth)}actions: ${performs.map((p, _) => p.fmtObj(game, playerTurn)).mkString(", ")}")

		val arrs = genArrs(game, remaining, false)
		val result = optimize(game, arrs, performs, playerTurn, deadline, depth)
		result

	def possibleActions(game: G, playerTurn: Int, remaining: RemainingMap, deadline: Instant, depth: Int = 0, infer: Boolean = false)(using ops: GameOps[G]): Seq[(PerformAction, Seq[Identity])] =
		boundary:
			val state = game.state

			def tryAction(perform: PerformAction) =
				this.winnableIf(state, playerTurn, perform, remaining, deadline, depth) match
					case SimpleResult.Unwinnable               => None
					case SimpleResult.WinnableWithDraws(draws) => Some((perform, draws))
					case SimpleResult.AlwaysWinnable           => Some((perform, Nil))

			val urgentAction = state.hands(playerTurn).find(game.meta(_).urgent).flatMap: urgent =>
				val perform = game.meta(urgent).status match
					case CardStatus.CalledToPlay => PerformAction.Play(urgent)
					case _                       => PerformAction.Discard(urgent)
				tryAction(perform)

			if urgentAction.isDefined then
				return Seq(urgentAction.get)

			val playables = if infer || game.goodTouch || game.state.endgameTurns.isDefined then
				game.players(playerTurn).thinksPlayables(game, playerTurn, excludeTrash = true)
			else
				game.players(playerTurn).obviousPlayables(game, playerTurn)

			val playActions = playables.map: order =>
				if Instant.now.isAfter(deadline) then
					break(Nil)

				state.deck(order).id() match
					case None =>
						// Log.info(s"can't identify $order ${game.players(playerTurn).thoughts(order)}")
						None
					case _ => tryAction(PerformAction.Play(order))
			.flatten

			val defaultClue = PerformAction.Rank(0, 0)
			val tooManyClues = state.actionList.flatten.reverse
				.takeWhile(!_.requiresDraw)
				.count(_.isInstanceOf[ClueAction]) > game.state.numPlayers

			val clueWinnable = state.canClue &&
				!tooManyClues &&
				(this.winnableIf(state, playerTurn, defaultClue, remaining, deadline, depth) match
					case SimpleResult.Unwinnable => false
					case SimpleResult.AlwaysWinnable => true
					case _ => throw new IllegalStateException(s"Shouldn't return WinnableWithDraws from giving a clue!"))

			val clueActions = if !clueWinnable then Nil else
				// If everyone knows exactly where all the remaining useful cards are, clues are only useful for stalling, so we only need to consider 1 clue
				val fullyKnown = (remaining.isEmpty || (remaining.size == 1 && state.isBasicTrash(remaining.head._1))) &&
					state.hands.flatten.forall: o =>
						state.deck(o).id().forall: id =>
							state.isBasicTrash(id) || game.common.thoughts(o).matches(id, infer = true)

				val allClues = ops.findAllClues(game, playerTurn).map(_ -> Nil)

				if fullyKnown then allClues.take(1) else allClues

			if Instant.now.isAfter(deadline) then
				break(Nil)

			val ignoreDc = state.pace == 0 ||
				state.clueTokens == 8 ||
				playables.exists: p =>
					game.players(playerTurn).thoughts(p).id(infer = true).exists: id =>
						// Always play a 5 over dc
						id.rank == 5 ||
						(state.hands(playerTurn).exists(o => o != p && state.deck(o).clued && game.common.thoughts(o).possible.forall(state.isCritical)) && {
							state.isCritical(id) ||
							// Always play a duped card in hand over dc
							state.hands(playerTurn).exists(o => o != p && game.players(playerTurn).thoughts(o).matches(id, infer = true))
						})

			val dcActions = if ignoreDc then Nil else
				ops.findAllDiscards(game, playerTurn).map(tryAction).flatten

			// If no playables are visible, try discarding before cluing
			if state.hands.zipWithIndex.forall((hand, i) => i == playerTurn || hand.forall(o => !state.isPlayable(state.deck(o).id().get))) then
				playActions.concat(dcActions).concat(clueActions)
			else
				playActions.concat(clueActions).concat(dcActions)

	def actionWinrate(game: G, arrs: Seq[GameArr], action: (PerformAction, Seq[Identity]), playerTurn: Int, deadline: Instant)(using ops: GameOps[G]): Frac =
		if Instant.now.isAfter(deadline) then
			return Frac.zero

		val (perform, winnableDraws) = action
		val nextPlayerIndex = game.state.nextPlayerIndex(playerTurn)

		arrs.summing:
			case GameArr(_, _, drew) if drew.exists(!winnableDraws.contains(_)) =>
				Frac.zero

			case GameArr(prob, remaining, drew) =>
				val newGame = game.simulateAction(perform.toAction(game.state, playerTurn), drew)

				if newGame.state.maxScore < game.state.maxScore then Frac.zero else
					val newState = newGame.state
					val (cardsLeft, endgameTurns) = (newState.cardsLeft, newState.endgameTurns)

					if perform.isClue then
						Log.info(s"${perform.fmtObj(game, playerTurn)} cards left $cardsLeft endgame turns $endgameTurns")
					else
						val hand = newState.hands(playerTurn)
						Log.info(s"drawing ${newState.logId(drew)} (${hand(0)}) after ${perform.fmtObj(game, playerTurn)} ${hand.map(newState.logId).mkString(",")} cards left $cardsLeft endgame turns $endgameTurns")

					winnable(newGame, nextPlayerIndex, remaining, deadline, 1) match
						case Left(msg) =>
							Log.highlight(Console.YELLOW, s"} ${perform.fmtObj(game, playerTurn)} unwinnable ($msg)")
							Frac.zero

						case Right((performs, wr)) =>
							Log.highlight(Console.YELLOW, s"} ${performs.map(_.fmtObj(game, nextPlayerIndex)).mkString(", ")} prob $prob winrate $wr")
							prob * wr

	def optimizeFull(game: G, arrs: (Seq[GameArr], Seq[GameArr]), actions: Seq[(PerformAction, Seq[Identity])], playerTurn: Int, deadline: Instant)(using ops: GameOps[G]): Seq[(PerformAction, Frac)] =
		boundary:
			val (undrawn, drawn) = arrs

			actions.map: (perform, winnableDraws) =>
				val winrate = actionWinrate(game, if perform.isClue then undrawn else drawn, (perform, winnableDraws), playerTurn, deadline)
				(perform, winrate)

			.toList
			.sortBy(-_._2)

	def optimize(game: G, arrs: (Seq[GameArr], Seq[GameArr]), actions: Seq[(PerformAction, Seq[Identity])], playerTurn: Int, deadline: Instant, depth: Int = 0)(using ops: GameOps[G]): WinnableResult =
		boundary:
			val (undrawn, drawn) = arrs
			val nextPlayerIndex = game.state.nextPlayerIndex(playerTurn)

			val sortedActions = successRate.lift(depth) match
				case None => actions
				case Some(sr) => actions.sortBy(a => sr.lift(a._1).map((frac, _) => -frac).getOrElse(-Frac.one))

			val initial = (List.empty[PerformAction], Frac.zero)
			val (bestActions, bestWinrate) = sortedActions.foldLeft(initial) { case (acc, (perform, winnableDraws)) =>
				val (bestActions, bestWinrate) = acc

				if Instant.now.isAfter(deadline) then
					break(Right(acc))

				@annotation.tailrec
				def calcWinrate(arrs: Seq[GameArr], winrate: Frac, remProb: Frac): Frac =
					if arrs.isEmpty || winrate + remProb < bestWinrate then
						return winrate

					val GameArr(prob, remaining, drew) = arrs.head
					val newProb = remProb - prob
					lazy val newGame = game.simulateAction(perform.toAction(game.state, playerTurn), drew)

					val unwinnable = drew.exists(!winnableDraws.contains(_)) || newGame.state.maxScore < game.state.maxScore
					if unwinnable then
						calcWinrate(arrs.tail, winrate, newProb)
					else
						val newState = newGame.state
						val (cardsLeft, endgameTurns) = (newState.cardsLeft, newState.endgameTurns)

						if perform.isClue then
							Log.info(s"${indent(depth)}${perform.fmtObj(game, playerTurn)} cards left $cardsLeft endgame turns $endgameTurns")
						else
							val hand = newState.hands(playerTurn)
							Log.info(s"${indent(depth)}drawing ${newState.logId(drew)} (${hand(0)}) after ${perform.fmtObj(game, playerTurn)} ${hand.map(newState.logId).mkString(",")} cards left $cardsLeft endgame turns $endgameTurns")

						val colour = if depth == 0 then Console.YELLOW else Console.WHITE
						winnable(newGame, nextPlayerIndex, remaining, deadline, depth + 1) match
							case Left(msg) =>
								Log.highlight(colour, s"${indent(depth)}} ${perform.fmtObj(game, playerTurn)} unwinnable ($msg)")
								calcWinrate(arrs.tail, winrate, remProb)

							case Right((performs, wr)) =>
								val newWinrate = winrate + prob * wr
								if newWinrate > Frac.one then
									throw new IllegalStateException(s"Winrate exceeds 100% $prob $newWinrate | ${arrs.map(_.prob).mkString(",")}")

								Log.highlight(colour, s"${indent(depth)}} ${performs.map(_.fmtObj(game, nextPlayerIndex)).mkString(", ")} prob $prob winrate $wr")
								calcWinrate(arrs.tail, newWinrate, remProb)

				val winrate = calcWinrate(if perform.isClue then undrawn else drawn, Frac.zero, Frac.one)

				val newEntry = successRate.lift(depth) match
					case Some(entry) => entry.lift(perform) match
						case Some((frac, times)) =>
							val newFrac = (frac * times + winrate) / (times + 1)
							entry.updated(perform, (newFrac, times + 1))
						case None =>
							entry.updated(perform, (winrate, 1))
					case None =>
						Map(perform -> (winrate, 1))
				successRate = successRate.updated(depth, newEntry)

				val bdrs = game.state.allIds.difference(game.state.trashSet).filter: id =>
					game.state.isUseful(id) && game.state.isCritical(id) && id.rank != 5

				val cardsLeft = game.state.cardsLeft

				if winrate == Frac.one || (bdrs.length == 1 && cardsLeft > 1 && winrate == Frac(cardsLeft - 1, cardsLeft)) then
					break(Right((List(perform), winrate)))

				if winrate > bestWinrate then
					(List(perform), winrate)
				else if winrate > Frac.zero && winrate == bestWinrate then
					(bestActions :+ perform, winrate)
				else
					acc
			}

			if bestActions.isEmpty then
				Left("no action wins")
			else
				Right((bestActions, bestWinrate))
