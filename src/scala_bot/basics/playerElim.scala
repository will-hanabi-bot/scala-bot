package scala_bot.basics

import scala.collection.immutable.BitSet
import scala_bot.utils._
import scala_bot.logger.Log

case class MatchEntry(order: Int, unknownTo: Int)

extension (p: Player) {
	def cardElim(state: State) =
		var certainMap = p.certainMap

		var crossElimCandidates = List.empty[Int]
		var resets = List.empty[Int]
		var thoughts = p.thoughts

		var allPossible = p.allPossible
		var allInferred = p.allInferred
		var dirty = p.dirty
		// var dirtyIds = IdentitySet.empty

		dirty.toArray.fastForeach { order =>
			val thought = thoughts(order)
			val id = thought.id(symmetric = p.isCommon)

			if (id.isDefined)
				val unknownTo = if (thought.id(symmetric = true).isEmpty) state.holderOf(order) else -1
				val certains = certainMap(id.get.toOrd)
				val index = certains.indexWhere(_.order == order)

				certainMap =
					if (index != -1)
						if (thought.possible.length == 1 && certains(index).unknownTo != -1)
							certainMap.updated(id.get.toOrd, certains.updated(index, MatchEntry(order, -1)))
						else
							certainMap
					else
						certainMap.updated(id.get.toOrd, MatchEntry(order, unknownTo) +: certains)
				// dirtyIds = dirtyIds.union(id.get)
		}

		def updateMap(id: Identity, exclude: BitSet): (Boolean, IdentitySet) =
			var changed = false
			var recursiveIds = IdentitySet.empty
			var crossElimRemovals = BitSet.empty

			loop(0, _ < state.numPlayers, _ + 1, !exclude.contains(_)) { playerIndex =>
				val hand = state.hands(playerIndex)
				hand.fastForeach { order =>
					val thought = thoughts(order)
					val noElim =
						!thought.possible.contains(id) ||
						certainMap(id.toOrd).exists(e => e.order == order || e.unknownTo == playerIndex)

					if (!noElim)
						changed = true
						val newInferred = thought.inferred.difference(id)
						val newPossible = thought.possible.difference(id)
						val reset = newInferred.isEmpty && !thought.reset

						thoughts = thoughts.updated(order,
							if (reset)
								thought.copy(possible = newPossible).resetInferences()
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
						)

						dirty = dirty + order

						if (reset)
							resets = order +: resets

						// Card can be further eliminated
						if (newPossible.length == 1)
							val recursiveId = newPossible.head
							val certains = certainMap(recursiveId.toOrd)

							certainMap = certainMap.updated(recursiveId.toOrd,
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
				}
			}

			crossElimCandidates = crossElimCandidates.filterNot(crossElimRemovals.contains)
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
				val knownCount = certainMap(id.toOrd).length

				if (knownCount == state.cardCount(id.toOrd)) {
					val (innerChanged, innerRecursiveIds) = updateMap(id, BitSet.empty)

					eliminated = eliminated.union(id)
					changed ||= innerChanged
					recursiveIds = recursiveIds.union(innerRecursiveIds)
				}
			}

			if (!recursiveIds.isEmpty)
				changed = basicElim(recursiveIds) || changed

			allPossible = allPossible.difference(eliminated)
			allInferred = allInferred.difference(eliminated)
			changed

		/**
		 * The "sudoku" emathy operation, involving 2 parts:
		 * Symmetric info - if Alice has [r5,g5] and Bob has [r5,g5], then everyone knows how r5 and g5 are distributed.
		 * Naked pairs - If Alice has 3 cards with [r4,g5], then everyone knows that both r4 and g5 cannot be elsewhere (will be eliminated in basic_elim).
		 * Returns true if at least one card was modified.
		 */
		def performCrossElim(entries: BitSet, ids: IdentitySet) =
			var changed = false
			val groups = entries.groupBy(state.deck(_).id())

			for ((id, group) <- groups) {
				id.foreach { id =>
					val certains = certainMap(id.toOrd).filter(c => !group.contains(c.order)).length

					if (group.size == state.cardCount(id.toOrd) - certains) {
						val (innerChanged, _) = updateMap(id, BitSet.fromSpecific(group.map(state.holderOf)))
						changed ||= innerChanged
					}
				}
			}

			// Now elim all the cards outside of this entry
			for (id <- ids) {
				val (innerChanged, _) = updateMap(id, BitSet.fromSpecific(entries.map(state.holderOf)))
				changed ||= innerChanged
			}

			basicElim(ids) || changed

		def crossElim(candidates: List[Int]): Boolean =
			def cross(remaining: List[Int], contained: BitSet = BitSet.empty, accIds: IdentitySet = IdentitySet.empty, certains: BitSet = BitSet.empty): Boolean =
				val multiplicity = state.multiplicity(accIds)
				val impossibleMultiplicity = multiplicity - certains.size > contained.size + remaining.length

				if (impossibleMultiplicity)
					return false

				if (contained.size > 1 && multiplicity - certains.size == contained.size)
					val innerChanged = performCrossElim(contained, accIds)
					if (innerChanged)
						return true

				if (remaining.isEmpty)
					return false

				// Check all remaining subsets that contain the next item
				val order = remaining.head
				val newAccIds = accIds.union(thoughts(order).possible)

				val nextContained = contained + order
				val nextCertains = {
					val delta = thoughts(order).possible.difference(accIds)

					val allCertains = if (delta.isEmpty)
						certains
					else
						var mCertains = certains
						delta.foreachFast { id =>
							certainMap(id.toOrd).fastForeach { c =>
								mCertains = mCertains + c.order
							}
						}
						mCertains

					allCertains -- nextContained
				}

				val included = cross(remaining.tail, nextContained, newAccIds, nextCertains)
				if (included)
					return true

				// Check all remaining subsets that skip the next item
				cross(remaining.tail, contained, accIds, certains)

			cross(candidates)

		val _ = basicElim(state.allIds)

		loop(0, _ < state.numPlayers, _ + 1) { playerIndex =>
			state.hands(playerIndex).fastForeach { order =>
				val thought = thoughts(order)
				val possible = thought.possible

				val canCrossElim = possible.length > 1 &&
					possible.difference(state.trashSet).nonEmpty &&
					state.multiplicity(possible) <= 8

				if (canCrossElim)
					crossElimCandidates = order +: crossElimCandidates
			}
		}

		val candidates = crossElimCandidates.filter { order =>
			val thought = thoughts(order)
			var certains = BitSet.empty
			thought.possible.foreachFast { id =>
				certainMap(id.toOrd).fastForeach { c =>
					certains = certains + c.order
				}
			}

			state.multiplicity(thought.possible) - certains.size <= crossElimCandidates.length
		}

		while (candidates.length > 1 && crossElim(candidates)) {}

		val newPlayer = p.copy(
			thoughts = thoughts,
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
		var resets = List.empty[Int]
		var newThoughts = p.thoughts

		state.hands.fastForeach { hand =>
			hand.fastForeach { order =>
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
						resets = order +: resets
			}
		}

		(resets, p.copy(thoughts = newThoughts, dirty = dirty))

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

		state.hands.fastForeach { hand =>
			var infMap = Map.empty[IdentitySet, List[Int]]
			hand.fastForeach { o =>
				if (linkable(o))
					val infs = newPlayer.thoughts(o).inferred
					infMap = infMap.updated(infs, o +: infMap.getOrElse(infs, Nil))
			}

			val keys = infMap.keysIterator
			while (keys.nonEmpty) {
				val inferred = keys.next()
				val orders = infMap(inferred)
				if (orders.length > 1)
					val focused = orders.filter(game.meta(_).focused)
					if (focused.length == 1 && inferred.length == 1)
						newPlayer = newPlayer.elimLink(game, orders, focused.head, inferred.head, goodTouch)

					else if (orders.length > inferred.length)
						// We have enough inferred cards to elim elsewhere
						Log.info(s"adding link $orders infs ${inferred.fmt(state)} (${p.name})")
						newPlayer = newPlayer.copy(links = Link.Unpromised(orders, inferred.toList) +: newPlayer.links)
			}
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
