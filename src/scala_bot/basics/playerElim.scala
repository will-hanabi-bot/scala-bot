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
		var certainMap = p.certainMap

		var crossElimCandidates = mutable.Buffer[IdEntry]()
		val resets = mutable.Buffer[Int]()
		val thoughts = p.thoughts.toArray

		var allPossible = p.allPossible
		var allInferred = p.allInferred
		var dirty = p.dirty
		// var dirtyIds = IdentitySet.empty

		var d = 0
		val dirtyArr = dirty.toArray
		while (d < dirtyArr.length) {
			val order = dirtyArr(d)
			val thought = thoughts(order)
			val id = thought.id(symmetric = p.isCommon)

			if (id.isDefined)
				val unknownTo = if (thought.id(symmetric = true).isEmpty) state.holderOf(order) else -1
				val certains = certainMap.getOrElse(id.get, Nil)
				val index = certains.indexWhere(_.order == order)

				certainMap =
					if (index != -1)
						if (thought.possible.length == 1 && certains(index).unknownTo != -1)
							certainMap.updated(id.get, certains.updated(index, MatchEntry(order, -1)))
						else
							certainMap
					else
						certainMap.updated(id.get, MatchEntry(order, unknownTo) +: certains)
				// dirtyIds = dirtyIds.union(id.get)
			d += 1
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
							certainMap.getOrElse(id, Nil).exists(e => e.order == order || e.unknownTo == playerIndex)

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

							dirty = dirty + order

							if (reset)
								resets += order

							// Card can be further eliminated
							if (newPossible.length == 1)
								val recursiveId = newPossible.head
								val certains = certainMap.getOrElse(recursiveId, Nil)

								certainMap = certainMap.updated(recursiveId,
									if (certains.isEmpty)
										 List(MatchEntry(order, -1))
									else
										val index = certains.indexWhere(_.order == order)
										if (index == -1)
											MatchEntry(order, -1) +: certains
										else
											certains.updated(index, MatchEntry(order, -1)))

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
				val knownCount = certainMap.getOrElse(id, Nil).size

				if (knownCount == state.cardCount(id.toOrd)) {
					val (innerChanged, innerRecursiveIds) = updateMap(id, mutable.BitSet.empty)

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
					val certains = certainMap.getOrElse(id, Nil).filter(c => !group.exists(_.order == c.order)).length

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
						val newCertains = certainMap.getOrElse(id, Nil)
						var i = 0
						while (i < newCertains.length) {
							mCertains = mCertains + newCertains(i).order
							i += 1
						}
					}
					mCertains

				allCertains.filter(o => !nextContained.exists(_.order == o))
			}

			val included = crossElim(nextContained, newAccIds, nextCertains, nextIndex + 1)
			if (included)
				return true

			// Check all remaining subsets that skip the next item
			crossElim(contained, accIds, certains, nextIndex + 1)

		val _ = basicElim(state.allIds)

		var playerIndex = 0
		while (playerIndex < state.numPlayers) {
			var i = 0
			while (i < state.hands(playerIndex).length) {
				val order = state.hands(playerIndex)(i)
				val thought = thoughts(order)
				val possible = thought.possible

				val canCrossElim = possible.length > 1 &&
					possible.difference(state.trashSet).nonEmpty &&
					state.remainingMultiplicity(possible) <= 8

				if (canCrossElim)
					crossElimCandidates += IdEntry(order, playerIndex)

				i += 1
			}
			playerIndex += 1
		}

		while (crossElim(Set(), IdentitySet.empty, Set(), 0)) {}

		val newPlayer = p.copy(
			thoughts = thoughts.toVector,
			allPossible = allPossible,
			allInferred = allInferred,
			dirty = dirty,
			certainMap = certainMap
		)

		(resets, newPlayer)

	def goodTouchElim(game: Game) =
		val state = game.state

		def canElim(order: Int) =
			val thought = p.thoughts(order)

			!game.meta(order).trash &&
			!thought.reset &&
			!thought.id(symmetric = true).isDefined &&
			!thought.inferred.isEmpty &&
			thought.possible.difference(state.trashSet).nonEmpty &&
			game.isTouched(order)

		var dirty = p.dirty
		val resets = mutable.Buffer.empty[Int]
		var newThoughts = p.thoughts

		var i = 0
		while (i < state.numPlayers) {
			val hand = state.hands(i)
			var j = 0

			while (j < hand.length) {
				val order = hand(j)
				if (canElim(order))
					val thought = newThoughts(order)
					val newInferred = thought.inferred.difference(state.trashSet)
					val reset = newInferred.isEmpty && !thought.reset

					dirty = dirty + order

					val newThought = if (reset)
						thought.resetInferences()
					else
						thought.copy(inferred = newInferred)
					newThoughts = newThoughts.updated(order, newThought)

					if (reset)
						resets += order
				j += 1
			}
			i += 1
		}

		(resets.toList, p.copy(thoughts = newThoughts, dirty = dirty))

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
							t.copy(inferred = t.inferred.filter(!p.isTrash(game, _, order)))
						}
				else
					thought.copy(inferred = newInferred)

			thoughts.updated(order, newThought)
		}
		p.copy(thoughts = newThoughts, dirty = p.dirty ++ matches)

	def findLinks(game: Game, goodTouch: Boolean) =
		val state = game.state

		def linkable(order: Int) =
			val thought = p.thoughts(order)

			thought.id(symmetric = true).isEmpty &&
			(thought.inferred.length <= 2) &&
			thought.inferred.difference(state.trashSet).nonEmpty &&
			!p.links.exists(_.getOrders.contains(order))

		var newPlayer = p

		var i = 0
		while (i < state.numPlayers) {
			val hand = state.hands(i)

			val infMap = hand.foldLeft(Map.empty[IdentitySet, List[Int]]) { (map, o) =>
				if (!linkable(o))
					map
				else
					val infs = newPlayer.thoughts(o).inferred
					map.updated(infs, o +: map.getOrElse(infs, Nil))
			}

			newPlayer = infMap.foldLeft(newPlayer) { case (newPlayer, (inferred, orders)) =>
				if (orders.length == 1)
					newPlayer
				else
					val focused = orders.filter(game.meta(_).focused)
					if (focused.length == 1 && inferred.length == 1)
						newPlayer.elimLink(game, orders, focused.head, inferred.head, goodTouch)

					else if (orders.length > inferred.length)
						// We have enough inferred cards to elim elsewhere
						Log.info(s"adding link $orders infs ${inferred.fmt(state)} (${p.name})")
						newPlayer.copy(links = Link.Unpromised(orders, inferred.toList) +: newPlayer.links)
					else
						newPlayer
			}
			i += 1
		}
		newPlayer

	def refreshLinks(game: Game, goodTouch: Boolean) =
		val state = game.state

		val initial = (p.copy(links = Nil), List[Int]())
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
