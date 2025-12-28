package scala_bot.basics

import scala.collection.immutable.BitSet
import scala_bot.utils._
import scala_bot.logger.Log

case class MatchEntry(order: Int, unknownTo: Int)

case class CardElimResult(
	player: Player,
	changed: Boolean = false,
	removals: BitSet = BitSet.empty,
	resets: BitSet = BitSet.empty,
	recursiveIds: IdentitySet = IdentitySet.empty
):
	def merge(other: CardElimResult): CardElimResult =
		CardElimResult(
			player = other.player,
			changed = changed || other.changed,
			removals = removals.union(other.removals),
			resets = resets.union(other.resets),
			recursiveIds = recursiveIds.union(other.recursiveIds)
		)

extension (p: Player)
	private def updateMap(state: State, id: Identity, exclude: BitSet): CardElimResult =
		var changed = false
		var recursiveIds = IdentitySet.empty
		var crossElimRemovals = BitSet.empty

		var certainMap = p.certainMap
		var thoughts = p.thoughts
		var dirty = p.dirty
		var resets = BitSet.empty

		loop(0, _ < state.numPlayers, _ + 1, !exclude.contains(_)): playerIndex =>
			state.hands(playerIndex).fastForeach: order =>
				val thought = thoughts(order)
				val noElim =
					!thought.possible.contains(id) ||
					certainMap(id.toOrd).exists(e => e.order == order || e.unknownTo == playerIndex)

				if !noElim then
					changed = true
					val newInferred = thought.inferred.difference(id)
					val newPossible = thought.possible.difference(id)
					val reset = newInferred.isEmpty && !thought.reset

					thoughts = thoughts.updated(order,
						if reset then
							thought.copy(possible = newPossible).resetInferences()
						else if thought.infoLock.isDefined then
							thought.copy(
								inferred = newInferred,
								possible = newPossible,
								infoLock = thought.infoLock.get.difference(id).toOpt
							)
						else
							thought.copy(
								inferred = newInferred,
								possible = newPossible
							)
					)

					dirty = dirty.incl(order)

					if reset then
						resets = resets.incl(order)

					// Card can be further eliminated
					if newPossible.length == 1 then
						val recursiveId = newPossible.head
						val certains = certainMap(recursiveId.toOrd)

						certainMap = certainMap.updated(recursiveId.toOrd,
							if certains.isEmpty then
								 List(MatchEntry(order, -1))
							else
								val index = certains.indexWhere(_.order == order)
								if index == -1 then
									MatchEntry(order, -1) +: certains
								else
									certains.updated(index, MatchEntry(order, -1)))

						recursiveIds = recursiveIds.union(recursiveId)
						crossElimRemovals = crossElimRemovals.incl(order)

		// crossElimCandidates = crossElimCandidates.filterNot(crossElimRemovals.contains)
		val newPlayer = p.copy(
			certainMap = certainMap,
			thoughts = thoughts,
			dirty = dirty
		)
		CardElimResult(newPlayer, changed, crossElimRemovals, resets, recursiveIds)

	/**
	 * The "typical" empathy operation. If there are enough known instances of an identity, it is removed from every card (including future cards).
	 * Returns true if at least one card was modified.
	 */
	private def basicElim(state: State, ids: IdentitySet): CardElimResult =
		var res = CardElimResult(p)
		var eliminated = IdentitySet.empty

		ids.foreach: id =>
			val knownCount = res.player.certainMap(id.toOrd).length

			if knownCount == state.cardCount(id.toOrd) then
				val innerResult = res.player.updateMap(state, id, BitSet.empty)

				res = res.merge(innerResult)
				eliminated = eliminated.union(id)

		if res.recursiveIds.nonEmpty then
			val innerResult = res.player.basicElim(state, res.recursiveIds)
			res = res.merge(innerResult)

		res.copy(player = res.player.copy(
			allPossible = res.player.allPossible.difference(eliminated),
			allInferred = res.player.allInferred.difference(eliminated)))

	/**
	 * The "sudoku" emathy operation, involving 2 parts:
	 * Symmetric info - if Alice has [r5,g5] and Bob has [r5,g5], then everyone knows how r5 and g5 are distributed.
	 * Naked pairs - If Alice has 3 cards with [r4,g5], then everyone knows that both r4 and g5 cannot be elsewhere (will be eliminated in basic_elim).
	 * Returns true if at least one card was modified.
	 */
	private def performCrossElim(state: State, entries: BitSet, holders: BitSet, ids: IdentitySet): CardElimResult =
		val groups = Array.fill[List[Int]](state.variant.suits.length * 5)(Nil)
		var groupIds = IdentitySet.empty

		var res = CardElimResult(p)

		entries.fastForeach: o =>
			val id = state.deck(o).id()
			if id.isDefined then
				val ord = id.get.toOrd
				groups(ord) = o +: groups(ord)
				groupIds = groupIds.union(id.get)

		groupIds.foreach: id =>
			val group = groups(id.toOrd)
			val certains = res.player.certainMap(id.toOrd).filter(c => !group.contains(c.order)).length

			if group.size == state.cardCount(id.toOrd) - certains then
				val innerResult = res.player.updateMap(state, id, BitSet.fromSpecific(group.map(state.holderOf)))
				res = res.merge(innerResult)

		// Now elim all the cards outside of this entry
		for id <- ids do
			val innerResult = res.player.updateMap(state, id, holders)
			res = res.merge(innerResult)

		val innerResult = res.player.basicElim(state, ids)
		res.merge(innerResult)

	private def crossElim(
		state: State,
		remaining: List[Int],
		contained: BitSet = BitSet.empty,
		holders: BitSet = BitSet.empty,
		accIds: IdentitySet = IdentitySet.empty,
		certains: BitSet = BitSet.empty
	): CardElimResult =
		val multiplicity = state.multiplicity(accIds)
		val impossibleMultiplicity = multiplicity - certains.size > contained.size + remaining.length

		var res = CardElimResult(p)

		if impossibleMultiplicity then
			return res

		if contained.size > 1 && multiplicity - certains.size == contained.size then
			val innerResult = performCrossElim(state, contained, holders, accIds)
			if innerResult.changed then
				return innerResult
			else
				res = res.merge(innerResult)

		if remaining.isEmpty then
			res
		else
			// Check all remaining subsets that contain the next item
			val order = remaining.head
			val newAccIds = accIds.union(res.player.thoughts(order).possible)

			val nextContained = contained.incl(order)
			val nextCertains =
				val delta = res.player.thoughts(order).possible.difference(accIds)

				val allCertains = if delta.isEmpty then
					certains
				else
					var mCertains = certains
					delta.foreach: id =>
						res.player.certainMap(id.toOrd).fastForeach: c =>
							mCertains = mCertains.incl(c.order)
					mCertains

				allCertains.diff(nextContained)

			val nextHolders = holders.incl(state.holderOf(order))
			val innerResult = res.player.crossElim(state, remaining.tail, nextContained, nextHolders, newAccIds, nextCertains)

			if innerResult.changed then
				res.merge(innerResult)
			else
				res.merge(innerResult).merge(res.player.crossElim(state, remaining.tail, contained, holders, accIds, certains))

	def cardElim(state: State): (BitSet, Player) =
		var certainMap = p.certainMap
		// var dirtyIds = IdentitySet.empty

		p.dirty.toArray.fastForeach: order =>
			val thought = p.thoughts(order)
			val id = thought.id(symmetric = p.isCommon)

			if id.isDefined then
				val unknownTo = if thought.id(symmetric = true).isEmpty then state.holderOf(order) else -1
				val certains = certainMap(id.get.toOrd)
				val index = certains.indexWhere(_.order == order)

				certainMap =
					if index != -1 then
						if thought.possible.length == 1 && certains(index).unknownTo != -1 then
							certainMap.updated(id.get.toOrd, certains.updated(index, MatchEntry(order, -1)))
						else
							certainMap
					else
						certainMap.updated(id.get.toOrd, MatchEntry(order, unknownTo) +: certains)
				// dirtyIds = dirtyIds.union(id.get)

		var newPlayer = p.copy(certainMap = certainMap)
		var crossElimCandidates = List.empty[Int]
		var resets = BitSet.empty

		val CardElimResult(player, _, _, basicResets, _) = newPlayer.basicElim(state, state.allIds)
		newPlayer = player
		resets = resets.union(basicResets)

		loop(0, _ < state.numPlayers, _ + 1): playerIndex =>
			state.hands(playerIndex).fastForeach: order =>
				val thought = newPlayer.thoughts(order)
				val possible = thought.possible

				val canCrossElim = possible.length > 1 &&
					possible.difference(state.trashSet).nonEmpty &&
					state.multiplicity(possible) <= 8

				if canCrossElim then
					crossElimCandidates = order +: crossElimCandidates

		var candidates = crossElimCandidates.filter: order =>
			val thought = newPlayer.thoughts(order)
			var certains = BitSet.empty
			thought.possible.foreach: id =>
				newPlayer.certainMap(id.toOrd).fastForeach: c =>
					certains = certains.incl(c.order)

			state.multiplicity(thought.possible) - certains.size <= crossElimCandidates.length

		var changed = true

		while candidates.length > 1 && changed do
			val CardElimResult(innerPlayer, innerChanged, removals, innerResets, _) = newPlayer.crossElim(state, candidates)

			changed = innerChanged
			candidates = candidates.filterNot(removals.contains)
			resets = resets.union(innerResets)
			newPlayer = innerPlayer

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
		var resets = BitSet.empty
		var newThoughts = p.thoughts

		state.hands.fastForeach: hand =>
			hand.fastForeach: order =>
				if canElim(order) then
					val thought = newThoughts(order)
					val newInferred = thought.inferred.difference(state.trashSet)
					val reset = newInferred.isEmpty && !thought.reset

					dirty = dirty + order

					val newThought = if reset then
						thought.resetInferences()
					else
						thought.copy(inferred = newInferred)
					newThoughts = newThoughts.updated(order, newThought)

					if reset then
						resets = resets.incl(order)

		(resets, p.copy(thoughts = newThoughts, dirty = dirty))

	def elimLink(game: Game, matches: Seq[Int], focus: Int, id: Identity, goodTouch: Boolean): Player =
		Log.info(s"eliminating ${game.state.logId(id)} link from focus (${p.name})! $matches --> $focus")

		val newThoughts = matches.foldLeft(p.thoughts): (thoughts, order) =>
			val thought = thoughts(order)
			val newInferred = if order == focus then
				IdentitySet.single(id)
			else
				thought.inferred.difference(id)

			val newThought =
				if newInferred.isEmpty && !thought.reset then
					thought.resetInferences()
						.when(_ => goodTouch): t =>
							t.copy(inferred = t.inferred.filter(!p.isTrash(game, _, order)))
				else
					thought.copy(inferred = newInferred)

			thoughts.updated(order, newThought)

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

		state.hands.fastForeach: hand =>
			var infMap = Map.empty[IdentitySet, List[Int]]
			hand.fastForeach: o =>
				if linkable(o) then
					val infs = newPlayer.thoughts(o).inferred
					infMap = infMap.updated(infs, o +: infMap.getOrElse(infs, Nil))

			val keys = infMap.keysIterator
			while keys.nonEmpty do
				val inferred = keys.next()
				val orders = infMap(inferred)
				if orders.length > 1 then
					val focused = orders.filter(game.meta(_).focused)
					if focused.length == 1 && inferred.length == 1 then
						newPlayer = newPlayer.elimLink(game, orders, focused.head, inferred.head, goodTouch)

					else if orders.length > inferred.length then
						// We have enough inferred cards to elim elsewhere
						Log.info(s"adding link $orders infs ${inferred.fmt(state)} (${p.name})")
						newPlayer = newPlayer.copy(links = Link.Unpromised(orders, inferred.toList) +: newPlayer.links)

		newPlayer

	def refreshLinks(game: Game, goodTouch: Boolean) =
		val state = game.state

		val initial = (p.copy(links = Nil), List.empty[Int])
		val (newPlayer, sarcastics) = p.links.foldRight(initial): (link, acc) =>
			val (player, sarcastics) = acc
			link match
				case Link.Promised(orders, id, target) =>
					lazy val viableOrders = orders.filter(player.thoughts(_).possible.contains(id))

					val skip = orders.exists(player.thoughts(_).matches(id, symmetric = true)) ||	// At least 1 card matches, promise resolved
						!player.thoughts(target).possible.exists(_.suitIndex == id.suitIndex) ||
						viableOrders.isEmpty

					if skip then
						acc
					else if viableOrders.length == 1 then
						Log.info(s"resolving promised link for ${state.logId(id)} to $viableOrders (${p.name})")
						(player.withThought(viableOrders.head)(_.copy(inferred = IdentitySet.single(id))), sarcastics)
					else
						if viableOrders.length < orders.length then
							Log.info(s"updating promised link for ${state.logId(id)} to $viableOrders (${p.name})")
						(player.copy(links = Link.Promised(viableOrders, id, target) +: player.links), sarcastics)

				case Link.Sarcastic(orders, id) =>
					lazy val viableOrders = orders.filter(player.thoughts(_).possible.contains(id))

					// At least 1 card matches, promise resolved
					if orders.exists(player.thoughts(_).matches(id)) then
						acc
					else if viableOrders.isEmpty then
						Log.warn(s"promised sarcastic ${state.logId(id)} not found among cards $orders, rewind?")
						acc
					else if viableOrders.length == 1 then
						(player.withThought(viableOrders.head)(_.copy(inferred = IdentitySet.single(id))),
							viableOrders.head +: sarcastics)
					else
						if viableOrders.length < orders.length then
							Log.info(s"updating sarcastic link for ${state.logId(id)} to $viableOrders (${p.name})")
						(player.copy(links = Link.Sarcastic(viableOrders, id) +: player.links), sarcastics)

				case Link.Unpromised(orders, ids) =>
					val revealed = orders.filter: o =>
						val thought = player.thoughts(o)

						thought.id(symmetric = true).nonEmpty ||
						ids.exists(!thought.possible.contains(_)) ||
						!state.hands.flatten.contains(o)		// An unknown card was played/discarded hypothetically; in hypo, we would know what it is

					lazy val focused = orders.filter(game.meta(_).focused)

					if revealed.nonEmpty then
						acc
					else if focused.length == 1 && ids.length == 1 then
						Log.info(s"resolving unpromised link for ${state.logId(ids.head)} to ${focused.head} (${p.name})")
						(player.elimLink(game, orders, focused.head, ids.head, goodTouch), sarcastics)
					else
						ids.find(i => orders.exists(!player.thoughts(_).inferred.contains(i))) match
							case Some(lostInf) =>
								Log.info(s"linked orders $orders lost inference ${state.logId(lostInf)}")
								(player, sarcastics)
							case None =>
								(player.copy(links = link +: player.links), sarcastics)
		(sarcastics, newPlayer.findLinks(game, goodTouch))

	def refreshPlayLinks(game: Game) =
		p.playLinks.foldRight(p.copy(playLinks = Nil)):
			case (PlayLink(orders, prereqs, target), acc) =>
				val remOrders = orders.filter(o => game.state.hands.exists(h => h.contains(o)))
				if remOrders.isEmpty then
					// The target must be playable now.
					acc.withThought(target): t =>
						t.copy(inferred = t.inferred.intersect(game.state.playableSet))
				else
					acc.copy(playLinks = PlayLink(remOrders, prereqs, target) +: acc.playLinks)
