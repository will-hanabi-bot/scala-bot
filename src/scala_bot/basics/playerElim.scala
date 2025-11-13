package scala_bot.basics

import scala.collection.mutable
import scala_bot.utils._
import scala_bot.logger.Log

class MatchEntry(
	var order: Int,
	var unknownTo: Int
)

class IdEntry(
	var order: Int,
	var playerIndex: Int
)

extension (p: Player) {
	def cardElim(state: State) =
		val certainMap = Array.fill[mutable.Buffer[MatchEntry]](state.variant.suits.length * 5)(mutable.Buffer.empty)
		var crossElimCandidates = mutable.Buffer[IdEntry]()
		val resets = mutable.Buffer[Int]()
		val thoughts = p.thoughts.toArray

		var allPossible = p.allPossible
		var allInferred = p.allInferred

		var playerIndex = 0
		while (playerIndex < state.numPlayers) {
			var i = 0
			while (i < state.hands(playerIndex).length) {
				val order = state.hands(playerIndex)(i)
				val thought = thoughts(order)
				val id = thought.id(symmetric = p.isCommon)
				val possible = thought.possible

				if (id.isDefined)
					val unknownTo = if (thought.id(symmetric = true).isEmpty) playerIndex else -1
					certainMap(id.get.toOrd) += MatchEntry(order, unknownTo)

				val canCrossElim = possible.length > 1 &&
					possible.difference(state.trashSet).nempty &&
					state.remainingMultiplicity(possible) <= 8

				if (canCrossElim)
					crossElimCandidates += IdEntry(order, playerIndex)

				i += 1
			}
			playerIndex += 1
		}

		def updateMap(id: Identity, exclude: mutable.BitSet): (Boolean, IdentitySet) =
			var changed = false
			var recursiveIds = IdentitySet.empty
			val crossElimRemovals = mutable.BitSet.empty

			var playerIndex = 0
			while (playerIndex < state.hands.length) {
				if (!exclude.contains(playerIndex))
					val hand = state.hands(playerIndex)
					var i = 0

					while (i < hand.length) {
						val order = hand(i)
						val thought = thoughts(order)
						val noElim =
							!thought.possible.contains(id) ||
							certainMap(id.toOrd).exists(e => e.order == order || e.unknownTo == playerIndex)

						if (!noElim)
							changed = true
							val newInferred = thought.inferred.difference(id)
							val newPossible = thought.possible.difference(id)
							val reset = newInferred.isEmpty && !thought.reset

							thoughts(order) = if (reset)
								thought.resetInferences()
							else if (thought.infoLock.isDefined)
								thought.copy(
									inferred = newInferred,
									possible = newPossible,
									infoLock = Some(thought.infoLock.get.difference(id))
								)
							else
								thought.copy(
									inferred = newInferred,
									possible = newPossible
								)

							if (reset)
								resets += order

							// Card can be further eliminated
							if (newPossible.length == 1)
								val recursiveId = newPossible.head
								val certains = certainMap(recursiveId.toOrd)

								if (certains.isEmpty)
									certains += MatchEntry(order, -1)
								else
									certains.find(_.order == order) match {
										case Some(entry) => entry.unknownTo = -1
										case None => certains += MatchEntry(order, -1)
									}

								recursiveIds = recursiveIds.union(recursiveId)
								crossElimRemovals += order
						i += 1
					}
				playerIndex += 1
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

			ids.foreachFast { id =>
				val knownCount = state.baseCount(id.toOrd) + certainMap(id.toOrd).size

				if (knownCount == state.cardCount(id.toOrd)) {
					val (innerChanged, innerRecursiveIds) = updateMap(id, mutable.BitSet())

					eliminated = eliminated.union(id)
					changed ||= innerChanged
					recursiveIds = recursiveIds.union(innerRecursiveIds)
				}
			}

			if (!recursiveIds.isEmpty)
				changed = basicElim(recursiveIds) || changed

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
					val certains = certainMap(id.toOrd).filter(c => !group.exists(_.order == c.order)).length

					if (group.size == state.remainingMultiplicity(IdentitySet.single(id)) - certains) {
						val (innerChanged, _) = updateMap(id, mutable.BitSet.fromSpecific(group.map(_.playerIndex)))
						changed ||= innerChanged
					}
				}
			}

			// Now elim all the cards outside of this entry
			for (id <- ids) {
				val (innerChanged, _) = updateMap(id, mutable.BitSet.fromSpecific(entries.map(_.playerIndex)))
				changed ||= innerChanged
			}

			basicElim(ids) || changed

		def crossElim(contained: Set[IdEntry], accIds: IdentitySet, certains: Set[Int], nextIndex: Int): Boolean =
			lazy val multiplicity = state.remainingMultiplicity(accIds)
			lazy val impossibleMultiplicity = multiplicity - certains.size >
				contained.size + (crossElimCandidates.length - nextIndex)

			if (crossElimCandidates.length <= 1 || impossibleMultiplicity)
				return false

			// Impossible to reach multiplicity
			if (multiplicity - certains.size > contained.size + (crossElimCandidates.length - nextIndex))
				return false

			if (contained.size >= 2 && multiplicity - certains.size == contained.size)
				val innerChanged = performCrossElim(contained, accIds)
				if (innerChanged)
					return true

			if (nextIndex >= crossElimCandidates.length)
				return false

			// Check all remaining subsets that contain the next item
			val item = crossElimCandidates(nextIndex)
			val order = item.order
			val newAccIds = accIds.union(thoughts(order).possible)

			val nextContained = contained + item
			val nextCertains = {
				val delta = thoughts(order).possible.difference(accIds)

				val allCertains = if (delta.isEmpty)
					certains
				else
					var mCertains = certains
					delta.foreachFast { id =>
						mCertains = mCertains ++ certainMap(id.toOrd).map(_.order)
					}
					mCertains

				allCertains.filter(o => !nextContained.exists(_.order == o))
			}

			val included = crossElim(nextContained, newAccIds, nextCertains, nextIndex + 1)
			if (included)
				return true

			// Check all remaining subsets that skip the next item
			crossElim(contained, accIds, certains, nextIndex + 1)

		val allIds = IdentitySet.from(state.variant.allIds)
		val _ = basicElim(allIds)
		while (crossElim(Set(), IdentitySet.empty, Set(), 0)) {}

		val newPlayer = p.copy(
			thoughts = thoughts.toVector,
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

		val trashIds = game.state.trashSet

		val (newThoughts, resets) = elimCandidates.foldLeft((p.thoughts, List[Int]())) {
			case ((thoughts, resets), order) => {
				val thought = thoughts(order)
				val newInferred = thought.inferred.difference(trashIds)
				val reset = newInferred.isEmpty && !thought.reset

				val newThought = if (reset)
					thought.resetInferences()
				else
					thought.copy(inferred = newInferred)
				(thoughts.updated(order, newThought), if (reset) order +: resets else resets)
			}
		}

		(resets, p.copy(thoughts = newThoughts))

	def elimLink(game: Game, matches: Seq[Int], focus: Int, id: Identity, goodTouch: Boolean): Player =
		Log.info(s"eliminating ${game.state.logId(id)} link from focus (${p.name})! $matches --> $focus")

		val newThoughts = matches.foldLeft(p.thoughts) { (thoughts, order) =>
			val thought = thoughts(order)
			val newInferred = if (order == focus)
				IdentitySet.single(id)
			else
				thought.inferred.difference(id)

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
		val linkableOrders = {
			var i = 0
			val orders = mutable.ListBuffer.empty[(Int, Int)]

			while (i < game.state.numPlayers) {
				val hand = game.state.hands(i)
				var j = 0

				while (j < hand.length) {
					val thought = p.thoughts(hand(j))
					val numInfs = thought.inferred.length
					val linkable = thought.id(symmetric = true).isEmpty &&
						(numInfs == 0 || numInfs == 1 || numInfs == 2) &&
						thought.inferred.difference(game.state.trashSet).nempty

					if (linkable)
						orders += hand(j) -> i
					j += 1
				}
				i += 1
			}
			orders
		}

		var newPlayer = p
		var linked = p.links.flatMap(_.getOrders).toSet
		var i = 0

		while (i < linkableOrders.length) {
			val (order, index) = linkableOrders(i)
			val thought = newPlayer.thoughts(order)
			val inferred = thought.inferred

			if (!linked.contains(order))
				// Find all cards with the same inferences
				val matches = linkableOrders.filter((o, i) => i == index && newPlayer.thoughts(o).inferred == inferred).map(_._1)
				val focusedMatches = matches.filter(game.meta(_).focused)

				if (matches.length != 1)
					if (focusedMatches.length == 1 && inferred.length == 1)
						newPlayer = newPlayer.elimLink(game, matches.toSeq, focusedMatches.head, inferred.head, goodTouch)

					else if (matches.length > inferred.length)
						// We have enough inferred cards to elim elsewhere
						Log.info(s"adding link ${matches} infs ${inferred.map(game.state.logId).mkString(",")} (${p.name})")
						newPlayer = newPlayer.copy(links = Link.Unpromised(matches.toList, inferred.toList) +: newPlayer.links)
						linked = linked ++ matches
			i += 1
		}

		newPlayer

	def refreshLinks(game: Game, goodTouch: Boolean) =
		val state = game.state

		val initial = (p.copy(links = List.empty), List[Int]())
		val (newPlayer, sarcastics) = p.links.foldRight(initial) { (link, acc) =>
			val (player, sarcastics) = acc
			link match {
				case Link.Promised(orders, id, target) =>
					lazy val viableOrders = orders.filter(player.thoughts(_).possible.contains(id))

					val skip = orders.exists(player.thoughts(_).matches(id, symmetric = true)) ||	// At least 1 card matches, promise resolved
						!player.thoughts(target).possible.exists(_.suitIndex == id.suitIndex) ||
						viableOrders.isEmpty

					if (skip)
						acc
					else if (viableOrders.length == 1)
						Log.info(s"resolving promised link for ${state.logId(id)} to $viableOrders (${p.name})")
						(player.withThought(viableOrders.head)(_.copy(inferred = IdentitySet.single(id))), sarcastics)
					else
						if (viableOrders.length < orders.length)
							Log.info(s"updating promised link for ${state.logId(id)} to $viableOrders (${p.name})")
						(player.copy(links = Link.Promised(viableOrders, id, target) +: player.links), sarcastics)

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
							viableOrders.head +: sarcastics)
					else
						if (viableOrders.length < orders.length)
							Log.info(s"updating sarcastic link for ${state.logId(id)} to $viableOrders (${p.name})")
						(player.copy(links = Link.Sarcastic(viableOrders, id) +: player.links), sarcastics)

				case Link.Unpromised(orders, ids) =>
					val revealed = orders.filter { o =>
						val thought = player.thoughts(o)

						thought.id(symmetric = true).nonEmpty ||
						ids.exists(!thought.possible.contains(_)) ||
						!state.hands.flatten.contains(o)		// An unknown card was played/discarded hypothetically; in hypo, we would know what it is
					}

					lazy val focused = orders.filter(game.meta(_).focused)

					if (revealed.nonEmpty)
						acc
					else if (focused.length == 1 && ids.length == 1)
						Log.info(s"resolving unpromised link for ${state.logId(ids.head)} to ${focused.head} (${p.name})")
						(player.elimLink(game, orders, focused.head, ids.head, goodTouch), sarcastics)
					else
						ids.find(i => orders.exists(!player.thoughts(_).inferred.contains(i))) match {
							case Some(lostInf) =>
								Log.info(s"linked orders $orders lost inference ${state.logId(lostInf)}")
								(player, sarcastics)
							case None =>
								(player.copy(links = link +: player.links), sarcastics)
						}
			}
		}
		(sarcastics, newPlayer.findLinks(game, goodTouch))

	def refreshPlayLinks(game: Game) =
		p.playLinks.foldRight(p.copy(playLinks = Nil)) { case (PlayLink(orders, prereqs, target), acc) =>
			val remOrders = orders.filter(o => game.state.hands.exists(h => h.contains(o)))
			if (remOrders.isEmpty)
				// The target must be playable now.
				acc.withThought(target) { t =>
					t.copy(inferred = t.inferred.intersect(game.state.playableSet))
				}
			else
				acc.copy(playLinks = PlayLink(remOrders, prereqs, target) +: acc.playLinks)
		}
}
