package scala_bot.basics

import scala.collection.mutable
import scala_bot.utils._
import scala_bot.logger.Log

class MatchEntry(
	var order: Int,
	var unknownSelf: Boolean
)

class IdEntry(
	var order: Int,
	var playerIndex: Int
)

extension (p: Player) {
	def cardElim(state: State) =
		val certainMap = mutable.Map[Identity, mutable.Buffer[MatchEntry]]()
		var crossElimCandidates = mutable.Buffer[IdEntry]()
		val resets = mutable.Buffer[Int]()
		val updates = mutable.Map[Int, Thought]()

		var allPossible = p.allPossible
		var allInferred = p.allInferred

		def thoughts(order: Int) =
			updates.get(order).getOrElse(p.thoughts(order))

		for (playerIndex <- 0 until state.numPlayers; order <- state.hands(playerIndex)) {
			val thought = p.thoughts(order)
			val id = thought.id(symmetric = p.isCommon)
			val unknownSelf = thought.id(symmetric = true).isEmpty
			val possible = thought.possible

			id.foreach(i =>
				if (!certainMap.contains(i))
					certainMap(i) = mutable.Buffer.empty
				certainMap(i) += MatchEntry(order, unknownSelf))

			if (possible.length > 1 &&
				possible.exists(!state.isBasicTrash(_)) &&
				state.remainingMultiplicity(possible) <= 8) {
					crossElimCandidates += IdEntry(order, playerIndex)
			}
		}

		def updateMap(id: Identity, exclude: Set[Int]): (Boolean, IdentitySet) =
			var changed = false
			var recursiveIds = IdentitySet.empty
			val crossElimRemovals = mutable.Buffer[Int]()

			for ((hand, playerIndex) <- state.hands.zipWithIndex if !exclude.contains(playerIndex)) {
				for (order <- hand) {
					val thought = thoughts(order)
					val noElim =
						!thought.possible.contains(id) ||
						certainMap.get(id).exists(_.exists(e => e.order == order || (state.holderOf(e.order) == playerIndex && e.unknownSelf)))

					if (!noElim)
						changed = true
						val newInferred = thought.inferred.retain(_ != id)
						val newPossible = thought.possible.retain(_ != id)
						val reset = newInferred.isEmpty && !thought.reset

						updates(order) = if (reset)
							thought.resetInferences()
						else
							thought.copy(inferred = newInferred, possible = newPossible)

						if (reset)
							resets += order

						// Card can be further eliminated
						if (newPossible.length == 1)
							val recursiveId = newPossible.head
							certainMap.get(recursiveId) match {
								case Some(certains) =>
									certains.find(_.order == order) match {
										case Some(entry) => entry.unknownSelf = false
										case None => certains += MatchEntry(order, false)
									}
								case None =>
									certainMap += recursiveId -> mutable.Buffer(MatchEntry(order, false))
							}

							recursiveIds = recursiveIds.union(recursiveId)
							crossElimRemovals += order
				}
			}

			crossElimCandidates = crossElimCandidates.filterNot(c => crossElimRemovals.contains(c.order))
			(changed, recursiveIds)

		/**
		 * The "typical" empathy operation. If there are enough known instances of an identity, it is removed from every card (including future cards).
		 * Returns true if at least one card was modified.
		 */
		def basicElim(ids: IdentitySet): Boolean =
			var changed = false
			var recursiveIds = IdentitySet.empty
			var eliminated = IdentitySet.empty

			for (id <- ids) {
				val knownCount = state.baseCount(id) + certainMap.get(id).map(_.size).getOrElse(0)

				if (knownCount == state.cardCount(id.toOrd)) {
					val (innerChanged, innerRecursiveIds) = updateMap(id, Set())

					eliminated = eliminated.union(id)
					changed ||= innerChanged
					recursiveIds = recursiveIds.union(innerRecursiveIds)
				}
			}

			if (!recursiveIds.isEmpty)
				changed ||= basicElim(recursiveIds)

			allPossible = allPossible.difference(eliminated)
			allInferred = allPossible.difference(eliminated)
			changed

		/**
		 * The "sudoku" emathy operation, involving 2 parts:
		 * Symmetric info - if Alice has [r5,g5] and Bob has [r5,g5], then everyone knows how r5 and g5 are distributed.
		 * Naked pairs - If Alice has 3 cards with [r4,g5], then everyone knows that both r4 and g5 cannot be elsewhere (will be eliminated in basic_elim).
		 * Returns true if at least one card was modified.
		 */
		def performCrossElim(entries: Set[IdEntry], ids: IdentitySet) =
			var changed = false
			val groups = entries.groupBy(e => state.deck(e.order).id())

			for ((id, group) <- groups) {
				id.foreach { id =>
					val certains = certainMap.get(id).map(_.filter(c => !group.exists(_.order == c.order)).length).getOrElse(0)

					if (group.size == state.remainingMultiplicity(List(id)) - certains) {
						val (innerChanged, _) = updateMap(id, group.map(_.playerIndex))
						changed ||= innerChanged
					}
				}
			}

			// Now elim all the cards outside of this entry
			for (id <- ids) {
				val (innerChanged, _) = updateMap(id, entries.map(_.playerIndex))
				changed ||= innerChanged
			}

			basicElim(ids) || changed

		def crossElim(contained: Set[IdEntry], accIds: IdentitySet, certains: Set[Int], nextIndex: Int): Boolean =
			val multiplicity = state.remainingMultiplicity(accIds)
			val impossibleMultiplicity = multiplicity - certains.size >
				contained.size + (crossElimCandidates.length - nextIndex)

			if (crossElimCandidates.length <= 1 || impossibleMultiplicity)
				return false

			if (contained.size >= 2 && multiplicity - certains.size == contained.size)
				val innerChanged = performCrossElim(contained, accIds)
				if (innerChanged)
					return true

			if (nextIndex >= crossElimCandidates.length)
				return false

			val item = crossElimCandidates(nextIndex)
			val order = item.order
			val newAccIds = accIds.union(thoughts(order).possible)

			val nextContained = contained + item
			val nextCertains = certains ++ thoughts(order).possible.difference(accIds)
				.flatMap(id => certainMap.get(id).map(_.map(_.order)).getOrElse(mutable.Buffer.empty))
				.filter(o => !nextContained.exists(_.order == o))

			val included = crossElim(nextContained, newAccIds, nextCertains, nextIndex + 1)
			if (included)
				return true

			// Check all remaining subsets that skip the next item
			crossElim(contained, accIds, certains, nextIndex + 1)

		val allIds = IdentitySet.from(state.variant.allIds)
		val _ = basicElim(allIds)
		while (crossElim(Set(), IdentitySet.empty, Set(), 0)) {}

		val newPlayer = p.copy(
			thoughts = updates.foldLeft(p.thoughts) {
				case (acc, (id, update)) => acc.updated(id, update)
			},
			allPossible = allPossible,
			allInferred = allInferred
		)

		(resets, newPlayer)

	def goodTouchElim(game: Game) =
		val elimCandidates = game.state.hands.flatten.filter { order =>
			val thought = p.thoughts(order)

			!game.meta(order).trash &&
				!thought.reset &&
				!thought.id(symmetric = true).isDefined &&
				!thought.inferred.isEmpty &&
				thought.possible.exists(!game.state.isBasicTrash(_)) &&
				game.isTouched(order)
		}

		val trashIds = IdentitySet.from(game.state.variant.allIds).retain(game.state.isBasicTrash(_))

		val (newThoughts, resets) = elimCandidates.foldLeft((p.thoughts, List[Int]())) {
			case ((thoughts, resets), order) => {
				val thought = thoughts(order)
				val newInferred = thought.inferred.retain(!trashIds.contains(_))
				val reset = newInferred.isEmpty && !thought.reset

				val newThought = if (reset)
					thought.resetInferences()
				else
					thought.copy(inferred = newInferred)
				(thoughts.updated(order, newThought), if (reset) order +: resets else resets)
			}
		}

		(resets, p.copy(thoughts = newThoughts))

	def elimLink(game: Game, matches: Seq[Int], focus: Int, id: Identity, goodTouch: Boolean) =
		Log.info(s"eliminating ${game.state.logId(id)} link from focus! original $matches, final $focus")

		val newThoughts = matches.foldLeft(p.thoughts) { (thoughts, order) =>
			val thought = thoughts(order)
			val newInferred = if (order == focus)
				IdentitySet.single(id)
			else
				thought.inferred.retain(_ != id)

			val newThought =
				if (newInferred.isEmpty && !thought.reset)
					thought.resetInferences()
						.when(_ => goodTouch) { t =>
							t.copy(inferred = t.inferred.retain(!p.isTrash(game, _, order)))
						}
				else
					thought.copy(inferred = newInferred)

			thoughts.updated(order, newThought)
		}
		p.copy(thoughts = newThoughts)

	def findLinks(game: Game, goodTouch: Boolean) =
		val linkedOrders = p.links.flatMap(_.getOrders).toSet

		val linkableOrders = game.state.hands.flatten.filter { o =>
			val thought = p.thoughts(o)
			thought.id(symmetric = true).isEmpty &&
				(0 to 3).contains(thought.inferred.length) &&
				!thought.inferred.forall(game.state.isBasicTrash)
		}

		val (newPlayer, _) = linkableOrders.foldLeft((p, linkedOrders)) { case ((player, linked), order) =>
			val thought = player.thoughts(order)
			val inferred = thought.inferred

			if (linked.contains(order))
				(player, linked)
			else
				// Find all cards with the same inferences
				val matches = linkableOrders.filter(player.thoughts(_).inferred == inferred)
				val focusedMatches = matches.filter(game.meta(_).focused)

				if (matches.length == 1)
					(player, linked)
				else if (focusedMatches.length == 1 && inferred.length == 1)
					(player.elimLink(game, matches, focusedMatches.head, inferred.head, goodTouch), linked)
				else if (matches.length > inferred.length)
					// We have enough inferred cards to elim elsewhere
					Log.info(s"adding link ${matches} infs ${inferred.map(game.state.logId).mkString(",")} (${p.name})")
					(
						player.copy(links = Link.Unpromised(matches.toList, inferred.toList) +: player.links),
						linked.concat(matches)
					)
				else
					(player, linked)
		}
		newPlayer

	def refreshLinks(game: Game, goodTouch: Boolean) =
		val state = game.state

		val initial = (p, List[Link](), List[Int]())
		val (newPlayer, newLinks, sarcastics) = p.links.foldRight(initial) { (link, acc) =>
			val (player, newLinks, sarcastics) = acc
			link match {
				case Link.Promised(orders, id, target) =>
					lazy val viableOrders = orders.filter(player.thoughts(_).possible.contains(id))

					val skip = orders.exists(player.thoughts(_).matches(id)) ||	// At least 1 card matches, promise resolved
						player.thoughts(target).possible.exists(_.suitIndex == id.suitIndex) ||
						viableOrders.isEmpty

					if (skip)
						acc
					else if (viableOrders.length == 1)
						(player.withThought(viableOrders.head)(_.copy(inferred = IdentitySet.single(id))), newLinks, sarcastics)
					else
						Log.info(s"updating promised link for ${state.logId(id)} to $viableOrders (${p.name})")
						(player, Link.Promised(viableOrders, id, target) +: newLinks, sarcastics)

				case Link.Sarcastic(orders, id) =>
					lazy val viableOrders = orders.filter(player.thoughts(_).possible.contains(id))

					// At least 1 card matches, promise resolved
					if (orders.exists(player.thoughts(_).matches(id)))
						acc
					else if (viableOrders.isEmpty)
						Log.warn(s"promised sarcastic ${state.logId(id)} not found among cards $orders, rewind?")
						acc
					else if (viableOrders.length == 1)
						(player.withThought(viableOrders.head)(_.copy(inferred = IdentitySet.single(id))),
							newLinks,
							viableOrders.head +: sarcastics)
					else
						Log.info(s"updating sarcastic link for ${state.logId(id)} to $viableOrders (${p.name})")
						(player, Link.Sarcastic(viableOrders, id) +: newLinks, sarcastics)

				case Link.Unpromised(orders, ids) =>
					val revealed = orders.filter { o =>
						val thought = player.thoughts(o)
						thought.id().nonEmpty && ids.exists(!thought.possible.contains(_))
					}
					lazy val focused = orders.filter(game.meta(_).focused)

					if (revealed.nonEmpty)
						acc
					else
						val newPlayer = player.when(_ => focused.length == 1 && ids.length == 1)
								(_.elimLink(game, orders, focused.head, ids.head, goodTouch))

						ids.find(i => orders.exists(!newPlayer.thoughts(_).inferred.contains(i))) match {
							case Some(lostInf) =>
								Log.info(s"linked orders $orders lost inference ${state.logId(lostInf)}")
								(newPlayer, newLinks, sarcastics)
							case None =>
								(newPlayer, link +: newLinks, sarcastics)
						}
			}
		}
		(sarcastics, newPlayer.copy(links = newLinks).findLinks(game, goodTouch))
}
