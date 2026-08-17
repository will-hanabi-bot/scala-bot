package scala_bot.basics

import scala_bot.lib.{CertainMapEntry, FastBitSet}
import scala_bot.utils._
import scala_bot.logger.{Logger, LogLevel}

enum Link:
	/** A link where one of the cards are conventionally promised to be the identity.
	  * @example With multiple clued 1s and a play clue on r2, a Promised link would exist for r1.
	  * @param orders The orders of the cards involved.
	  * @param id     The promised identity.
	  * @param target The order of the card this link connects to.
	  */
	case Promised(orders: FastBitSet, id: Identity, target: Int)
	/** A link created from a *Sarcastic Discard*. */
	case Sarcastic(orders: FastBitSet, id: Identity)
	/** A link created from *Good Touch*. It's possible none of the cards are the identity.
	  * @example With two blue cards when the blue stack is at 4, an Unpromised link would exist for r5.
	  */
	case Unpromised(orders: FastBitSet, ids: IdentitySet)

	def getOrders: FastBitSet = this match
		case Promised(orders, _, _) => orders
		case Sarcastic(orders, _) => orders
		case Unpromised(orders, _) => orders

	def promise: Option[Identity] = this match
		case Promised(_, id, _) => Some(id)
		case Sarcastic(_, id) => Some(id)
		case _ => None

/** A link where we only know playing a set of cards lets another card play.
  * @example With multiple clued 1s and receiving a play clue on a 2 in our hand.
  */
case class PlayLink(
	orders: Seq[Int],
	/** The set of identities that, if played, would allow the target to play
	  * (allowing other cards to play instead of those in [[PlayLink.orders]])
	  */
	prereqs: IdentitySet,
	/** The order of the card this link connects to. */
	target: Int
)

case class Player(
	playerIndex: Int,
	/** The display name of the player (their username on hanab.live). */
	name: String,
	/** The set of possible identities a newly drawn card could be. Changes as more cards become visible to this player. */
	allPossible: IdentitySet,
	/** The play stacks if all delayed playable cards were played. */
	hypoStacks: Vector[Int],

	isCommon: Boolean,
	thoughts: Vector[Thought] = Vector(),

	links: List[Link] = Nil,
	playLinks: List[PlayLink] = Nil,
	/** The set of orders that are known playable, without knowing their identities. */
	unknownPlays: FastBitSet = FastBitSet.empty,
	/** The set of orders that are known delayed playable. */
	hypoPlays: FastBitSet = FastBitSet.empty,
	/** The total number of hypo plays that are from links (and thus aren't assigned to particular orders). */
	linkedPlays: Int = 0,

	/** The set of orders whose inferences have been modified since the last call to elim(). */
	dirty: FastBitSet = FastBitSet.empty,
	/** Maps each identity (by ordinal) to the orders known to be that identity. */
	certainMap: Array[CertainMapEntry],

	/** Maps each identity (by ordinal) to the orders that could be that identity. */
	possibleMap: Array[FastBitSet] = Array()
):
	def withThought(order: Int)(f: Thought => Thought) =
		copy(
			thoughts = thoughts.updated(order, f(thoughts(order))),
			dirty = dirty.incl(order)
		)

	/** A string of this player's inferences on the card with the given order (e.g. "r1,r4,r5"). */
	def strInfs(state: State, order: Int) =
		thoughts(order).inferred.toList.sortBy(_.toOrd).map(state.logId).mkString(",")

	/** A string of this player's possible ids on the card with the given order (e.g. "r1,r4,r5"). */
	def strPoss(state: State, order: Int) =
		thoughts(order).possible.toList.sortBy(_.toOrd).map(state.logId).mkString(",")

	/** Returns the order in the hand "referred to" by the given order. By default, refers to the left. */
	def refer(game: Game, hand: Vector[Int], order: Int, left: Boolean = false) =
		val offset = if left then -1 else 1
		val index = hand.indexOf(order)
		var targetIndex = (index + offset + hand.length) % hand.length

		while game.isTouched(hand(targetIndex)) && targetIndex != index do
			targetIndex = (targetIndex + offset + hand.length) % hand.length

		hand(targetIndex)

	def chopNewest(game: Game, playerIndex: Int) =
		game.state.hands(playerIndex).find: o =>
			!game.state.deck(o).clued && (game.meta(o).status == CardStatus.None || game.meta(o).status == CardStatus.PermissionToDiscard)

	/** Returns true if the given identity has been "sieved" (visible and will never go on chop). */
	def isSieved(game: Game, id: Identity, excludeOrder: Int) =
		!game.state.isCritical(id) &&
		(0 until game.state.numPlayers).exists: playerIndex =>
			val loaded = thinksLoaded(game, playerIndex)
			val chop = chopNewest(game, playerIndex)

			game.state.hands(playerIndex).exists: o =>
				o != excludeOrder && thoughts(o).matches(id, infer = true) &&
				(if loaded then
					game.meta(o).status != CardStatus.CalledToDiscard
				else
					chop.forall(_ != o))
		||
		links.exists:
			case Link.Unpromised(orders, ids) =>
				!orders.contains(excludeOrder) && ids.contains(id)
			case link =>
				!link.getOrders.contains(excludeOrder) && link.promise.contains(id)

	/** Returns true if the given identity has been "duplicated", excluding the given order.
	  * For conventions without good touch, the duplicate must be in the same hand.
	  * Otherwise, the duplicate must be additionally touched.
	  */
	def isDuped(game: Game, id: Identity, excludeOrder: Int) =
		val candidates = if game.goodTouch then
			game.state.heldOrders.filter(game.isTouched).toList
		else
			game.state.hands(game.state.holderOf(excludeOrder))

		candidates.exists: o =>
			o != excludeOrder &&
			thoughts(o).matches(id, infer = true) &&
			game.state.deck(o).matches(id, assume = true) &&		// card must actually match
			game.me.thoughts(o).possible.contains(id) &&
			// Not sharing a link
			!links.exists:
				case Link.Unpromised(orders, ids) =>
					(excludeOrder == -1 || orders.contains(excludeOrder)) && orders.contains(o) && ids.contains(id)
				case link =>
					(excludeOrder == -1 || link.getOrders.contains(excludeOrder)) && link.getOrders.contains(o) && link.promise.contains(id)

	/** Returns true if the given identity is either basic trash or duplicated (see [[Player.isDuped]]).*/
	def isTrash(game: Game, id: Identity, excludeOrder: Int) =
		game.state.isBasicTrash(id) || isDuped(game, id, excludeOrder)

	/** Returns true if this order is "kt" (see [[Player.orderKt]]) or has permission to discard (may not actually be trash). */
	def orderTrash(game: Game, order: Int) =
		val meta = game.meta(order)
		val thought = thoughts(order)

		if orderKt(game, order) then
			true
		else if thought.possible.difference(game.state.criticalSet).isEmpty then
			false
		else
			val conventionalTrash =
				thought.possible.forall(isTrash(game, _, order)) ||
				thought.infoLock.existsO(_.forall(isTrash(game, _, order))) ||
				meta.trash ||
				meta.status == CardStatus.CalledToDiscard ||
				(game.loadedOnPtd && meta.status == CardStatus.PermissionToDiscard)

			conventionalTrash || thought.possibilities.forall(isTrash(game, _, order))

	/** Returns true if this order is known trash, conventonally promised trash, or duplicated in the same hand. */
	def orderKt(game: Game, order: Int) =
		val thought = thoughts(order)

		(game.meta(order).trash && thought.possible.intersect(game.state.criticalSet).isEmpty) ||
		thought.possible.forall: id =>
			game.state.isBasicTrash(id) ||
			game.state.hands(game.state.holderOf(order)).exists: o =>
				o != order &&
				thoughts(o).matches(id) &&
				(!game.meta(order).focused || game.meta(o).focused)
		// (thought.inferred.isEmpty && thought.reset)

	/** Returns true if this order is known playable or conventionally promised playable.
	  * @param excludeTrash If true, trash identities are excluded from the inferences.
	  */
	def orderKp(game: Game, order: Int, excludeTrash: Boolean = false): Boolean =
		val state = game.state
		val thought = thoughts(order)

		if thought.possible.intersect(state.playableSet).isEmpty then
			false
		else if thought.possible.difference(state.playableSet).isEmpty then
			true
		else
			inline def possPlayable(poss: IdentitySet) =
				val p = if excludeTrash then poss.difference(state.trashSet) else poss
				p.nonEmpty && p.intersect(state.playableSet) == p

			game.meta(order).status match
				case CardStatus.CalledToPlay =>
					thought.possible.intersect(state.playableSet).nonEmpty &&
					thought.infoLock.forallO(_.intersect(state.playableSet).nonEmpty)

				case CardStatus.CalledToDiscard =>
					thought.possible.intersect(state.playableSet).nonEmpty && {
						val ids = thought.inferred.intersect(state.playableSet)
						ids.nonEmpty && ids.forall(state.isInverted)
					}

				case CardStatus.Sarcastic | CardStatus.GentlemansDiscard =>
					possPlayable(thought.inferred)

				case _ if isLinked(state, order, unpromisedOnly = true) =>
					false

				case _ =>
					possPlayable(thought.possible) || thought.infoLock.existsO(possPlayable)

	/** Returns true if this order is obviously playable (see [[Player.orderKp]]) or inferred to be playable.
	  * @param excludeTrash If true, trash identities are excluded from the inferences.
	  */
	def orderPlayable(game: Game, order: Int, excludeTrash: Boolean = false) =
		val state = game.state

		if orderKp(game, order) then
			true
		else if game.meta(order).trash then
			false
		else if isLinked(state, order, unpromisedOnly = true) && !thoughts(order).possible.forall(_.rank == 1) then
			false
		else
			val infer =
				// (playerIndex != state.ourPlayerIndex || state.strikes != 2 || game.meta(order).focused || game.isBlindPlaying(order)) &&
				game.meta(order).status != CardStatus.CalledToDiscard

			val poss = if infer then thoughts(order).possibilities else thoughts(order).possible
			val p = if excludeTrash then poss.difference(state.trashSet) else poss

			p.nonEmpty && p.intersect(state.playableSet) == p

	/** Returns the known playable and conventionally promised playable orders in the given player's hand. */
	def obviousPlayables(game: Game, playerIndex: Int) =
		game.state.hands(playerIndex).filter(orderKp(game, _))
			.pipe(game.filterPlayables(this, playerIndex, _))

	/** Returns the obvious and inferred playable orders in the given player's hand. (See [[orderPlayable]].) */
	def thinksPlayables(game: Game, playerIndex: Int, excludeTrash: Boolean = false, assume: Boolean = true) =
		val hand = game.state.hands(playerIndex)

		// -1 = not playable, -2 = playable and always kept, >=0 = playable with an inferred id
		val idOrds = Array.fill(hand.length)(-1)
		var certainIds = IdentitySet.empty

		loop(0, _ < hand.length, _ + 1): i =>
			val order = hand(i)
			if orderPlayable(game, order, excludeTrash = !thoughts(order).reset && excludeTrash && game.isTouched(order)) then
				thoughts(order).id() match
					case Some(id) =>
						certainIds = certainIds.union(id)
						idOrds(i) = -2
					case None =>
						idOrds(i) = thoughts(order).id(infer = true).map(_.toOrd).getOrElse(-2)

		// Exclude unknown cards if there is a duplicate that is fully known.
		var unduplicatedPlays = List.empty[Int]
		loop(hand.length - 1, _ >= 0, _ - 1): i =>
			val ord = idOrds(i)
			if ord == -2 || (ord >= 0 && !certainIds.contains(Identity.fromOrd(ord))) then
				unduplicatedPlays = hand(i) +: unduplicatedPlays

		game.filterPlayables(this, playerIndex, unduplicatedPlays, assume)

	/** Returns the trash orders in the given player's hand. (See [[Player.orderTrash]].) */
	def thinksTrash(game: Game, playerIndex: Int) =
		game.state.hands(playerIndex).filter(orderTrash(game, _))

	/** Returns the orders that could be discarded from the player's hand (may not be trash).*/
	def discardable(game: Game, playerIndex: Int, allowLockedSacrifice: Boolean = false) =
		game.state.hands(playerIndex).filter: order =>
			orderTrash(game, order) ||
			thoughts(order).possibilities.forall(isSieved(game, _, order)) || {
				allowLockedSacrifice &&
				game.state.deck(order).clued &&
				thoughts(order).possibilities.intersect(game.state.criticalSet).isEmpty &&
				game.common.thinksLocked(game, playerIndex)
			}

	/** Returns true if the given player has at least one playable or trash order in their hand. */
	def thinksLoaded(game: Game, playerIndex: Int) =
		thinksPlayables(game, playerIndex).nonEmpty || thinksTrash(game, playerIndex).nonEmpty

	/** Returns true if the player is not loaded and all of their cards are touched. */
	def thinksLocked(game: Game, playerIndex: Int) =
		!thinksLoaded(game, playerIndex) &&
		game.state.hands(playerIndex).forall: order =>
			val status = game.meta(order).status

			game.state.deck(order).clued || (
				status != CardStatus.None &&
				status != CardStatus.PermissionToDiscard &&
				status != CardStatus.CalledToDiscard &&
				!(status == CardStatus.Finessed && game.meta(order).hidden)
			)

	/** Returns true if the given player has at least one obvious playable or trash in their hand. */
	def obviousLoaded(game: Game, playerIndex: Int) =
		obviousPlayables(game, playerIndex).nonEmpty || thinksTrash(game, playerIndex).nonEmpty

	/** Returns true if the given player is not obviously loaded and all of their cards are touched. */
	def obviousLocked(game: Game, playerIndex: Int) =
		!obviousLoaded(game, playerIndex) &&
		game.state.hands(playerIndex).forall: order =>
			val status = game.meta(order).status

			game.state.deck(order).clued || {
				status != CardStatus.None &&
				status != CardStatus.PermissionToDiscard &&
				status != CardStatus.CalledToDiscard
			}

	/** Returns whether the given order is a valid prompt for the given identity. */
	def validPrompt(prev: Game, order: Int, id: Identity, connected: FastBitSet = FastBitSet.empty, forcePink: Boolean = false) =
		val state = prev.state
		val card = state.deck(order)
		val thought = thoughts(order)

		!connected.contains(order) &&				// not already connected
		state.deck(order).clued &&
		thought.possible.contains(id) &&			// must be a possibility
		thought.infoLock.forallO(_.contains(id)) &&
		(thought.inferred.length != 1 || thought.inferred.contains(id)) &&	// not info-locked on a different id
		card.clues.exists(state.variant.idTouched(id, _)) &&	// at least one clue matches
		(
			// Not trying to prompt a pink id, or forcing pink prompt
			if !state.variant.suits(id.suitIndex).suitType.pinkish || forcePink then
				true
			else
				val clues = card.clues
				val misranked =
					clues.forall(_.isEq(clues.head)) &&
					clues.head.kind == ClueKind.Rank &&
					clues.head.value != id.rank

				!misranked && prev.knownAs(order, PINKISH)
		)

	/** Returns the order in the given player's hand that would be prompted for the given identity, if it exists.
	  * @param connected  The orders that are already connected in this connection (and thus can't be used).
	  * @param ignore     Orders that cannot be used.
	  * @param forcePink  Whether to force a prompt for a pink identity, even if the Pink Prompt Rank Exception applies.
	  * @param rightmost  Whether to prompt from the right (defaults to false).
	  */
	def findPrompt(prev: Game, playerIndex: Int, id: Identity, connected: FastBitSet = FastBitSet.empty, ignore: FastBitSet = FastBitSet.empty, forcePink: Boolean = false, rightmost: Boolean = false) =
		val state = prev.state
		val hand = state.hands(playerIndex).when(_ => rightmost)(_.reverse)
		val validPrompts = hand.filter(validPrompt(prev, _, id, connected, forcePink))

		def posInfo(order: Int): Int =
			case class PosInfo(
				ranksClued: FastBitSet,
				coloursClued: FastBitSet,
				rankInfo: Int,
				colourInfo: Int
			)
			val initial = PosInfo(FastBitSet.empty, FastBitSet.empty, 0, 0)
			val info = state.deck(order).clues.foldLeftOpt(initial): (acc, clue) =>
				if acc.rankInfo == 2 && acc.colourInfo == 2 then
					Left(acc)
				else if clue.kind == ClueKind.Rank && acc.rankInfo < 2 && !acc.ranksClued.contains(clue.value) then
					val omniXorPink =
						state.variant.suits.exists(s => s.suitType.pinkish && s.suitType.rainbowish) ^
						state.variant.suits.exists(s => s.suitType.pinkish && !s.suitType.rainbowish)

					Right(acc.copy(
						ranksClued = acc.ranksClued.incl(clue.value),
						rankInfo = acc.rankInfo + 1,
						colourInfo = if omniXorPink then 2 else acc.colourInfo
					))
				else if clue.kind == ClueKind.Colour && acc.colourInfo < 2 && !acc.coloursClued.contains(clue.value) then
					val omniWithoutRainbow =
						state.variant.suits.exists(s => s.suitType.pinkish && s.suitType.rainbowish) &&
						!state.variant.suits.exists(s => !s.suitType.pinkish && s.suitType.rainbowish)

					Right(acc.copy(
						coloursClued = acc.coloursClued.incl(clue.value),
						colourInfo = acc.colourInfo + 1,
						rankInfo = if omniWithoutRainbow || state.variant.colourableSuits(clue.value).suitType.pinkish then 2 else acc.rankInfo
					))
				else
					Right(acc)

			info.rankInfo + info.colourInfo

		// Prompt the card with the most positive information
		validPrompts.maxByOption(posInfo)
			.filter(!ignore.contains(_))

	def findClued(prev: Game, playerIndex: Int, id: Identity, ignore: FastBitSet = FastBitSet.empty) =
		val state = prev.state
		state.hands(playerIndex).filter: order =>
			val thought = thoughts(order)

			!ignore.contains(order) &&				// not already connected
			state.deck(order).clued &&
			thought.possible.contains(id) &&			// must be a possibility
			thought.infoLock.forallO(_.contains(id)) &&
			(thought.inferred.length != 1 || thought.inferred.contains(id))	// not info-locked on a different id

	/** Returns how far the identity is from playable (through cards known by this player).
	  * 0 means that it is playable.
	  */
	def playableAway(id: Identity) =
		id.rank - (hypoStacks(id.suitIndex) + 1)

	/** Returns the order of the card least likely to be critical in the given player's hand. */
	def lockedDiscard(state: State, playerIndex: Int) =
		val critPercents = state.hands(playerIndex).map { o =>
			val poss = thoughts(o).possibilities
			val percent = poss.intersect(state.criticalSet).length.toDouble / poss.length
			(o, percent)
		}.sortBy(_._2)

		val leastCrits = critPercents.filter(_._2 == critPercents(0)._2)

		leastCrits.maxBy { (order, percent) =>
			thoughts(order).possibilities.summing: p =>
				if state.isBasicTrash(p) then 5 else
					(if percent == 1 then p.rank * 5 else 0) + p.rank - hypoStacks(p.suitIndex)
		}._1

	/** Returns the card most likely to be playable, breaking ties by leftmost.
	  * If all cards are known unplayble, returns `None`.
	  */
	def anxietyPlay(state: State, playerIndex: Int): Option[Int] =
		val hand = state.hands(playerIndex)
		val playableExists = hand.exists: o =>
			thoughts(o).possibilities.intersect(state.playableSet).nonEmpty

		Option.when(playableExists):
			hand.zipWithIndex.maxBy: (o, i) =>
				val poss = thoughts(o).possibilities
				val percent = poss.intersect(state.playableSet).length.toDouble / poss.length
				percent * 1000 - i
			._1

	/** Returns how many copies of the given id are unseen by this player. */
	def unknownIds(state: State, id: Identity) =
		val visibleCount = state.heldOrders.filter(thoughts(_).matches(id)).size
		state.cardCount(id.toOrd) - state.baseCount(id.toOrd) - visibleCount

	def isLinked(state: State, order: Int, unpromisedOnly: Boolean = false) =
		links.exists: link =>
			link match
				case Link.Promised(orders, id, _) =>
					!unpromisedOnly && orders.contains(order) && orders.size > unknownIds(state, id)

				case Link.Sarcastic(orders, id) if !unpromisedOnly =>
					!unpromisedOnly && orders.contains(order) && orders.size > unknownIds(state, id)

				case Link.Unpromised(orders, ids) =>
					orders.contains(order) && orders.size > ids.iter.summing(unknownIds(state, _))

				case _ => false

	def linkedOrders(state: State, unpromisedOnly: Boolean = false): FastBitSet =
		links.foldLeft(FastBitSet.empty): (acc, link) =>
			link match
				case Link.Promised(orders, id, _) if !unpromisedOnly =>
					if orders.size > unknownIds(state, id) then acc.union(orders) else acc

				case Link.Sarcastic(orders, id) if !unpromisedOnly =>
					if orders.size > unknownIds(state, id) then acc.union(orders) else acc

				case Link.Unpromised(orders, ids) =>
					if orders.size > ids.iter.summing(unknownIds(state, _)) then acc.union(orders) else acc

				case _ => acc

	def thinksInverted(state: State, order: Int) =
		thoughts(order).possibilities.forall(state.isInverted)

	def tryPlay(game: Game, order: Int) =
		if game.assumeInverted(this, order) && this.thoughts(order).possibilities.difference(game.state.playableSet).isEmpty then
			Action.dragDiscard(game.state, playerIndex, order)
		else
			Action.dragPlay(game.state, playerIndex, order)

	def tryDiscard(game: Game, order: Int) =
		if game.assumeInverted(this, order) then
			Action.dragPlay(game.state, playerIndex, order)
		else
			Action.dragDiscard(game.state, playerIndex, order)

	/** Returns the predicted score if all delayed playable cards are played. */
	def hypoScore = hypoStacks.sum + unknownPlays.size - linkedPlays

	/** Returns a new Player with the hypo stacks (and hypo plays, unknown plays, etc.) updated based on current information. */
	def updateHypoStacks[G <: Game](game: G)(using ops: GameOps[G]): Player =
		if this.dirty.isEmpty then this else
			var hypo =
				if isCommon then
					ops.copyWith(game, GameUpdates(noRecurse = Some(true), common = Some(this)))
				else
					ops.copyWith(game, GameUpdates(noRecurse = Some(true), players = Some(game.players.updated(playerIndex, this))))
			.pipe(ops.cleanHypo)

			var unknownPlays = FastBitSet.empty
			var played = FastBitSet.empty
			var attempted = FastBitSet.empty
			var linkedPlays = 0

			def player = if isCommon then hypo.common else hypo.players(playerIndex)
			def state = hypo.state

			def play(order: Int) =
				val holder = hypo.state.holderOf(order)

				// NOTE: symmetric = true?
				player.thoughts(order).id(infer = true) match
					case None if state.deck(order).id().exists(state.isInverted) =>
						// println(s"not known orange! $order")
					case None =>
						links.foreach: link =>
							val orders = link.getOrders
							if orders.contains(order) && orders.forall(o => o == order || played.contains(o)) then
								hypo = link.promise.fold(hypo): id =>
									if !hypo.state.isPlayable(id) then
										// Log.warn(s"tried to add linked ${state.logId(id)} ($order) onto hypo stacks, but they were at ${hypo.state.playStacks} $played ($name)")
										hypo
									else
										linkedPlays += 1
										hypo.withState(_.withPlay(id))

						unknownPlays = unknownPlays.incl(order)
						played = played.incl(order)

					case Some(id) =>
						if hypo.state.isPlayable(id) then
							val playAction = PlayAction(holder, order, id.suitIndex, id.rank)

							val level = Logger.level
							Logger.setLevel(LogLevel.Error.min(level))

							hypo = hypo.onPlay(playAction)
								.pipe(ops.refreshAfterPlay(hypo, _, playAction))

							Logger.setLevel(level)
							played = played.incl(order)
						else
							// Log.warn(s"tried to add ${state.logId(id)} ($order) onto hypo stacks, but they were at ${hypo.state.playStacks} $played ($name)")
							attempted = attempted.incl(order)

				hypo = hypo.withState(s => s.copy(hands = s.hands.updated(holder, s.hands(holder).filter(_ != order))))

			var changed = true

			while changed do
				changed = false

				loop(0, _ < state.numPlayers, _ + 1): i =>
					val playables =
						val normalPlays =
							if game.goodTouch then
								player.thinksPlayables(hypo, i, excludeTrash = true)
							else
								player.obviousPlayables(hypo, i)

						val invertedPlays =
							player.thinksTrash(hypo, i).filter: o =>
								// This player knows that the card is inverted
								player.thoughts(o).id().exists(state.isInverted)

						normalPlays ++ invertedPlays

					val it = playables.iterator

					while (it.hasNext && !changed) do
						val o = it.next()
						if !played.contains(o) && !attempted.contains(o) && hypo.state.hasConsistentInfs(thoughts(o)) then
							play(o)
							changed = true

				player.playLinks.foreach: link =>
					val allPlayed = link.orders.forall(played.contains)

					if allPlayed && !played.contains(link.target) && state.heldOrders.contains(link.target) then
						val order = link.target
						val id = state.deck(order).id()

						if id.isEmpty || state.isUseful(id.get) then
							play(link.target)

			this.copy(
				hypoStacks = hypo.state.playStacks,
				unknownPlays = unknownPlays,
				hypoPlays = played,
				linkedPlays = linkedPlays
			)

object Player:
	def apply(
		playerIndex: Int,
		name: String,
		allPossible: IdentitySet,
		hypoStacks: Vector[Int]
	): Player =
		Player(
			playerIndex = playerIndex,
			name = name,
			allPossible = allPossible,
			hypoStacks = hypoStacks,
			isCommon = playerIndex == -1,
			certainMap = CertainMapEntry.filledArray(allPossible.length)
		)
