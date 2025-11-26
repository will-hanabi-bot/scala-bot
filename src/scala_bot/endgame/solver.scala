package scala_bot.endgame

import scala_bot.basics._
import scala_bot.fraction.Frac
import scala_bot.logger.{Log, Logger, LogLevel}
import scala_bot.utils._

import java.time.Instant
import scala.util.chaining.scalaUtilChainingOps
import scala.util.boundary, boundary.break

type WinnableResult = Either[String, (List[PerformAction], Frac)]

enum SimpleResult:
	case AlwaysWinnable
	case WinnableWithDraws(draws: List[Identity])
	case Unwinnable

val UNWINNABLE = Left("")
val TIMEOUT = Left("timeout")

def indent(depth: Int) =
	(0 until depth).map(_ => "  ").mkString

case class RemainingEntry(missing: Int, all: Boolean)

type RemainingMap = Map[Identity, RemainingEntry]

extension(remaining: RemainingMap)
	def rem(id: Identity) =
		remaining(id).missing match {
			case 1 => remaining.removed(id)
			case m => remaining.updated(id, remaining(id).copy(missing = m - 1))
		}

def findRemainingIds(game: Game) =
	val state = game.state

	val initial = (Map[Identity, Int](), List[(Int, Option[Identity])](), Map[Identity, Vector[Int]]())
	val (seenIds, ownIds) = (0 until state.numPlayers).foldLeft(initial) { (a, i) =>
		state.hands(i).foldLeft(a) { (acc, order) =>
			val (seenIds, ownIds, inferIds) = acc
			// Identify all the cards we know for sure
			game.me.thoughts(order).id() match {
				case Some(id) =>
					val newSeen = seenIds.updated(id, seenIds.lift(id).map(_ + 1).getOrElse(1))
					val newOwn = if (i == state.ourPlayerIndex) (order, Some(id)) +: ownIds else ownIds
					(newSeen, newOwn, inferIds)

				case None if i == state.ourPlayerIndex =>
					game.me.thoughts(order).id() match {
						case Some(id) =>
							val newInfer = inferIds.updated(id, inferIds.lift(id).map(_ :+ order).getOrElse(Vector(order)))
							(seenIds, ownIds, newInfer)
						case None =>
							val newOwn = (order, None) +: ownIds
							(seenIds, newOwn, inferIds)
					}

				case None => acc
			}
		}
	}
	.pipe { case (seen, own, infers) =>
		infers.foldLeft((seen, own)) { case (acc, (id, orders)) =>
			val (seenIds, ownIds) = acc
			val seen = seenIds.lift(id).getOrElse(0)
			val tooMany = seen + orders.length + state.baseCount(id.toOrd) > state.cardCount(id.toOrd)

			val newSeen = if (tooMany) seenIds else seenIds.updated(id, seen + orders.length)
			val newOwn = orders.foldLeft(ownIds)((a, o) => (o, Option.when(!tooMany)(id)) +: a)
			(newSeen, newOwn)
		}
	}

	val remainingIds =(
		for
			id <- state.variant.allIds
			total = state.cardCount(id.toOrd)
			missing = total - state.baseCount(id.toOrd) - seenIds.getOrElse(id, 0)
				if missing > 0
		yield
			(id, RemainingEntry(missing, missing == total))
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
	var simpleCache: Map[Int, WinnableResult] = Map(),
	var simplerCache: Map[Int, Boolean] = Map(),
	var cluelessCache: Map[Int, Option[PerformAction]] = Map(),
	var ifCache: Map[String, SimpleResult] = Map(),
	var successRate: Map[Int, Map[PerformAction, (Frac, Int)]] = Map(),
	monteCarlo: Boolean = true
):
	def solve(game: G)(using ops: GameOps[G]): Either[String, (PerformAction, Frac)] =
		val state = game.state
		if (state.score + 1 == state.maxScore)
			val winningPlay = state.ourHand.find {
				game.me.thoughts(_).id(infer = true).exists(state.isPlayable)
			}

			if (winningPlay.isDefined)
				return Right(PerformAction.Play(winningPlay.get), Frac.one)

		val start = Instant.now()
		val deadline = Instant.now().plusSeconds(2)
		val (remainingIds, ownIds) = findRemainingIds(game)

		if (remainingIds.count((id, v) => !state.isBasicTrash(id) && v.all) > 2)
			val missingIds = remainingIds.keys.filter(!state.isBasicTrash(_)).map(state.logId).mkString(",")
			return Left(s"couldn't find any $missingIds!")

		val level = Logger.level
		Logger.setLevel(LogLevel.Off)

		val assumedGame = ownIds.foldLeft(game) { case (g, (order, id)) =>
			id.fold(g)(g.withId(order, _))
		}

		val linkedOrders = game.me.linkedOrders(state)
		val unknownOwn = ownIds.collect {
			case (order, id) if id.isEmpty => order
		}
		val totalUnknown = state.cardsLeft + unknownOwn.length
		Log.info(s"unknown own $unknownOwn, cards left ${state.cardsLeft}")

		if (totalUnknown == 0)
			winnable(assumedGame, state.ourPlayerIndex, remainingIds, deadline) match {
				case Left(_) =>
					Logger.setLevel(level)
					return Left("couldn't find a winning strategy")
				case Right((actions, winrate)) =>
					Logger.setLevel(level)
					Log.highlight(Console.MAGENTA, s"winnable! actions ${actions.map(_.fmt(assumedGame)).mkString(",")}")
					Log.info(s"solved in ${start.until(Instant.now()).toMillis()}ms")
					return Right((actions.head, winrate))
			}

		Log.info(s"remaining ids: ${remainingIds.map((id, entry) => s"${state.logId(id)} (missing ${entry.missing})").mkString(", ")}")

		def impossibleArr(ids: Vector[Identity], id: Identity, order: Int) =
			val thought = game.me.thoughts(order)

			state.deck(order).id().exists(_ != id) ||
			!thought.possible.contains(id) ||
			(// if (!state.isBasicTrash(id))
				// !thought.possible.contains(id)
			// else
				// thought.possibilities.nonEmpty && !thought.possibilities.exists(state.isBasicTrash) &&
				// We cannot assign a trash id if it is linked and all other orders are already trash
				(linkedOrders.contains(order) && game.me.links.exists { l =>
					val orders = l.getOrders
					orders.contains(order) && orders.forall { o =>
						o == order ||
						ids.zipWithIndex.exists((id, i) => o == unknownOwn(i) && state.isBasicTrash(id))
					}
				}))

		def expandArr(arrangement: Arrangement): Iterable[Arrangement] =
			if (Instant.now.isAfter(deadline))
				return Seq(arrangement)

			val Arrangement(ids, prob, remaining) = arrangement
			val totalCards = remaining.values.map(_.missing).sum

			remaining.collect {
				case (id, RemainingEntry(missing, _)) if !impossibleArr(ids, id, unknownOwn(ids.length)) =>
					val newRemaining = remaining.rem(id)
					val newIds = ids :+ id
					val newProb = prob * missing / totalCards
					Arrangement(newIds, newProb, newRemaining)
			}

		val initialArr = List(Arrangement(Vector.empty, Frac.one, remainingIds))
		val allArrs = Iterator.iterate(initialArr)(_.flatMap(expandArr)).drop(unknownOwn.length).next()

		if (Instant.now.isAfter(deadline))
			return TIMEOUT

		Log.info(s"all arrs ${allArrs.length}")

		// Normalize all probabilities: some of the potential generated ones may be impossible, so the total prob may be less than 1.
		val sumProb = allArrs.map(_.prob).sum
		val normalArrs = allArrs.map { arr =>
			assert(arr.remaining.values.map(_.missing).sum == state.cardsLeft, s"arrangement not generated correctly: ${arr.remaining.values.map(_.missing)} ${state.cardsLeft}")
			arr.copy(prob = arr.prob / sumProb)
		}

		val arrs = normalArrs.when(_ => monteCarlo) {
			_.foldLeft(Map[String, List[Arrangement]]()) { (acc, arr) =>
				val trashArr = arr.ids.zipWithIndex.collect {
					case (id, i) if state.isBasicTrash(id) => i
				}.mkString
				acc.updated(trashArr, arr :: acc.getOrElse(trashArr, List.empty))
			}
			.pipe { a =>
				a.removed("")
					.flatMap { case (_, arrs) =>
						val totalWinrate = arrs.map(_.prob).sum
						val amt = arrs.length.min(3)
						arrs.take(amt).map(arr => arr.copy(prob = totalWinrate / amt))
					}
					.concat(a.getOrElse("", List.empty))
					.toList
			}
		}.sortBy(a => -a.prob)

		Log.info(s"arrangements ${arrs.length}")

		def evalPerforms(bestPerforms: Map[PerformAction, (Frac, Int)], eGame: G, arr: GameArr) =
			val GameArr(prob, remaining, _) = arr
			val eState = eGame.state
			Log.highlight(Console.MAGENTA, s"\narrangement ${eState.ourHand.map(eState.logId).mkString(",")} $prob")
			val allActions = {
				val as = possibleActions(eGame, state.ourPlayerIndex, remaining, deadline)

				if (as.nonEmpty) as else
					possibleActions(eGame, state.ourPlayerIndex, remaining, deadline, infer = true)
			}

			if (allActions.isEmpty)
				Log.info("couldn't find any valid actions")
				bestPerforms
			else
				Log.highlight(Console.GREEN, s"actions: ${allActions.map((action, _) => action.fmt(eGame)).mkString(", ")}")

				val arrs = genArrs(eGame, remaining, allActions.forall(_._1.isClue))

				optimize(eGame, arrs, allActions, state.ourPlayerIndex, deadline) match {
					case Right((performs, winrate)) =>
						Log.info(s"arrangement winnable! ${performs.map(_.fmt(eGame)).mkString(", ")} | winrate: $winrate")
						performs.foldLeft(bestPerforms) { (acc, perform) =>
							val entry = acc.lift(perform) match {
								case Some((w, i)) => (w + winrate * prob, i)
								case None => (winrate * prob, bestPerforms.size)
							}
							acc.updated(perform, entry)
						}
					case Left(_) => bestPerforms
				}

		val bestPerforms =
			if (arrs.isEmpty)
				evalPerforms(Map(), assumedGame, GameArr(Frac.one, Map(), None))
			else
				arrs.foldLeft(Map[PerformAction, (Frac, Int)]()) { case (acc, Arrangement(ids, prob, remaining)) =>
					val hypoGame = (0 until ids.length).foldLeft(assumedGame) { (hypo, i) =>
						val order = unknownOwn(i)
						hypo.withId(order, ids(i))
					}

					evalPerforms(acc, hypoGame, GameArr(prob, remaining, None))
				}

		Logger.setLevel(level)

		if (bestPerforms.isEmpty)
			Left("couldn't find any winning actions")
		else
			val (bestAction, (winrate, _)) = bestPerforms.maxBy { case (_, (winrate, index)) => winrate * 1000 - index }
			Log.info(s"endgame winnable! ${bestAction.fmt(game)} (winrate $winrate)")
			Log.info(s"solved in ${start.until(Instant.now()).toMillis()}ms")
			Right((bestAction, winrate))

	def winnable(game: G, playerTurn: Int, remaining: RemainingMap, deadline: Instant, depth: Int = 0)(using ops: GameOps[G]): WinnableResult =
		val state = game.state
		val hash = game.hashCode

		if (simpleCache.contains(hash))
			return simpleCache(hash)

		if (Instant.now().isAfter(deadline))
			return TIMEOUT

		val trivialWin = triviallyWinnable(game, playerTurn)

		if (trivialWin.isRight)
			Log.info(s"${indent(depth)}trivially winnable!")
			simpleCache = simpleCache.updated(hash, trivialWin)
			return trivialWin

		val viableClueless =
			(for
				suitIndex <- 0 until state.variant.suits.length
				rank <- state.playStacks(suitIndex) + 1 to state.maxRanks(suitIndex)
			yield
				Identity(suitIndex, rank))
		.forall { id =>
			val order = state.hands.flatten.find(game.common.thoughts(_).matches(id, infer = true))
			order.exists(state.deck(_).id().forall(_.matches(id)))
		}

		if (viableClueless)
			val cluelessState = state.hands.flatten.foldLeft(state) { (acc, order) =>
				game.common.thoughts(order).id().fold(acc) { id =>
					val newCard = acc.deck(order).copy(suitIndex = id.suitIndex, rank = id.rank)
					acc.copy(deck = acc.deck.updated(order, newCard))
				}
			}

			val cluelessWin = this.cluelessWinnable(cluelessState, playerTurn, deadline, depth)
			if (cluelessWin.isDefined)
				Log.info(s"${indent(depth)}clueless winnable!")
				return Right((List(cluelessWin.get), Frac.one))

		val bottomDecked = remaining.nonEmpty && remaining.keys.forall(id => state.isCritical(id) && id.rank != 5)
		lazy val performs = possibleActions(game, playerTurn, remaining, deadline, depth)

		if (bottomDecked || unwinnableState(state, playerTurn, depth) || performs.isEmpty)
			simpleCache = simpleCache.updated(hash, UNWINNABLE)
			return UNWINNABLE

		if (state.score + 1 == state.maxScore)
			val winningPlay = performs.find { (p, _) => p match {
				case PerformAction.Play(target) => state.isPlayable(state.deck(target).id().get)
				case _ => false
			}}

			if (winningPlay.isDefined)
				return Right((winningPlay.map(_._1).toList, Frac.one))

		Log.highlight(Console.GREEN, s"${indent(depth)}actions: ${performs.map((p, _) => p.fmtObj(game, playerTurn)).mkString(", ")}")

		val arrs = genArrs(game, remaining, false, depth)
		val result = optimize(game, arrs, performs, playerTurn, deadline, depth)
		simpleCache = simpleCache.updated(hash, result)
		result

	def possibleActions(game: G, playerTurn: Int, remaining: RemainingMap, deadline: Instant, depth: Int = 0, infer: Boolean = false)(using ops: GameOps[G]): List[(PerformAction, List[Identity])] =
		boundary:
			val state = game.state

			def tryAction(perform: PerformAction) =
				this.winnableIf(state, playerTurn, perform, remaining, deadline, depth) match {
					case SimpleResult.Unwinnable => None
					case SimpleResult.WinnableWithDraws(draws) => Some((perform, draws))
					case SimpleResult.AlwaysWinnable => Some((perform, List()))
				}

			val urgentAction = state.hands(playerTurn).find(game.meta(_).urgent).map { urgent =>
				val perform = game.meta(urgent).status match {
					case CardStatus.CalledToPlay => PerformAction.Play(urgent)
					case _ => PerformAction.Discard(urgent)
				}
				tryAction(perform).toList
			}

			if (urgentAction.isDefined)
				return urgentAction.get

			val playables = if (infer || game.goodTouch)
				game.players(playerTurn).thinksPlayables(game, playerTurn)
			else
				game.players(playerTurn).obviousPlayables(game, playerTurn)

			val playActions = playables.map { order =>
				if (Instant.now.isAfter(deadline))
					break(List())

				state.deck(order).id() match {
					case None =>
						// Log.info(s"can't identify $order ${game.players(playerTurn).thoughts(order)}")
						None
					case _ =>
						val perform = PerformAction.Play(order)
						tryAction(perform)
				}
			}.flatten.toList

			val defaultClue = PerformAction.Rank(0, 0)
			val tooManyClues = state.actionList.flatten.reverse
				.takeWhile(!_.requiresDraw)
				.count(_.isInstanceOf[ClueAction]) > game.state.numPlayers

			val clueWinnable = state.canClue &&
				!tooManyClues &&
				(this.winnableIf(state, playerTurn, defaultClue, remaining, deadline, depth) match {
					case SimpleResult.Unwinnable => false
					case SimpleResult.AlwaysWinnable => true
					case _ => throw new IllegalStateException(s"Shouldn't return WinnableWithDraws from giving a clue!")
				})

			val clueActions = if (!clueWinnable) List() else
				// If everyone knows exactly where all the remaining useful cards are, clues are only useful for stalling, so we only need to consider 1 clue
				val fullyKnown = (remaining.isEmpty || (remaining.size == 1 && state.isBasicTrash(remaining.head._1))) &&
					state.hands.flatten.forall { o =>
						state.deck(o).id() match {
							case None => true
							case Some(id) => state.isBasicTrash(id) || game.common.thoughts(o).matches(id, infer = true)
						}
					}

				val allClues = ops.findAllClues(game, playerTurn).map(_ -> List())

				(if (fullyKnown) allClues.take(1) else allClues).toList

			if (Instant.now.isAfter(deadline))
				break(List())

			val dcActions = if (state.pace <= 0) List() else
				ops.findAllDiscards(game, playerTurn).map(tryAction).flatten.toList

			// If every hand other than ours is trash, try discarding before cluing
			if (state.hands.zipWithIndex.forall((hand, i) => i == playerTurn || hand.forall(o => state.isBasicTrash(state.deck(o).id().get))))
				playActions.concat(dcActions).concat(clueActions)
			else
				playActions.concat(clueActions).concat(dcActions)

	def optimize(game: G, arrs: (List[GameArr], List[GameArr]), actions: List[(PerformAction, List[Identity])], playerTurn: Int, deadline: Instant, depth: Int = 0)(using ops: GameOps[G]): WinnableResult =
		boundary:
			val (undrawn, drawn) = arrs
			val nextPlayerIndex = game.state.nextPlayerIndex(playerTurn)

			val sortedActions = successRate.lift(depth) match {
				case None => actions
				case Some(sr) => actions.sortBy(a => sr.lift(a._1).map((frac, _) => -frac).getOrElse(Frac.zero))
			}

			val initial = (List[PerformAction](), Frac.zero)
			val (bestActions, bestWinrate) = sortedActions.foldLeft(initial) { case (acc, (perform, winnableDraws)) =>
				val (bestActions, bestWinrate) = acc

				if (Instant.now.isAfter(deadline))
					break(TIMEOUT)

				def calcWinrate(arrs: List[GameArr], winrate: Frac, remProb: Frac): Frac =
					if (arrs.isEmpty || winrate + remProb < bestWinrate)
						return winrate

					val GameArr(prob, remaining, drew) = arrs.head
					val newProb = remProb - prob
					lazy val newGame = game.simulateAction(performToAction(game.state, perform, playerTurn, None), drew)

					val unwinnable = drew.exists(!winnableDraws.contains(_)) || newGame.state.maxScore < game.state.maxScore
					if (unwinnable)
						calcWinrate(arrs.tail, winrate, newProb)
					else
						val newState = newGame.state
						val (cardsLeft, endgameTurns) = (newState.cardsLeft, newState.endgameTurns)

						if (perform.isClue)
							Log.info(s"${indent(depth)}${perform.fmtObj(newGame, playerTurn)} cards left $cardsLeft endgame turns $endgameTurns")
						else
							val hand = newState.hands(playerTurn)
							Log.info(s"${indent(depth)}drawing ${newState.logId(drew)} (${hand(0)}) after ${perform.fmtObj(newGame, playerTurn)} ${hand.map(newState.logId).mkString(",")} cards left $cardsLeft endgame turns $endgameTurns")

						val colour = if (depth == 0) Console.YELLOW else Console.WHITE
						winnable(newGame, nextPlayerIndex, remaining, deadline, depth + 1) match {
							case Left(msg) =>
								Log.highlight(colour, s"${indent(depth)}} ${perform.fmtObj(game, playerTurn)} unwinnable ($msg)")
								calcWinrate(arrs.tail, winrate, remProb)

							case Right((performs, wr)) =>
								val newWinrate = winrate + prob * wr
								if (newWinrate > Frac.one)
									throw new IllegalStateException(s"Winrate exceeds 100% $prob $newWinrate | ${arrs.map(_.prob).mkString(",")}")

								Log.highlight(colour, s"${indent(depth)}} ${performs.map(_.fmtObj(game, nextPlayerIndex)).mkString(", ")} prob $prob winrate $wr")
								calcWinrate(arrs.tail, newWinrate, remProb)
						}

				val arrs = if (perform.isClue) undrawn else drawn
				val winrate = calcWinrate(arrs, Frac.zero, Frac.one)

				val newEntry = successRate.lift(depth) match {
					case Some(entry) => entry.lift(perform) match {
						case Some((frac, times)) =>
							val newFrac = (frac * times + winrate) / (times + 1)
							entry.updated(perform, (newFrac, times + 1))
						case None =>
							entry.updated(perform, (winrate, 1))
						}
					case None =>
						Map(perform -> (winrate, 1))
				}
				successRate = successRate.updated(depth, newEntry)

				if (winrate == Frac.one)
					break(Right((List(perform), Frac.one)))

				if (winrate > bestWinrate)
					(List(perform), winrate)
				else if (winrate > Frac.zero && winrate == bestWinrate)
					(bestActions :+ perform, winrate)
				else
					acc
			}

			if (bestActions.isEmpty)
				Left("no action wins")
			else
				Right((bestActions, bestWinrate))
