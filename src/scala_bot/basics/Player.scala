package scala_bot.basics

import scala_bot.utils._
import scala_bot.logger.Log

import scala.collection.immutable.BitSet
import scala.util.chaining.scalaUtilChainingOps

enum Link:
	case Promised(orders: Seq[Int], id: Identity, target: Int)
	case Sarcastic(orders: Seq[Int], id: Identity)
	case Unpromised(orders: Seq[Int], ids: List[Identity])

	def getOrders: Seq[Int] = this match {
		case Promised(orders, _, _) => orders
		case Sarcastic(orders, _) => orders
		case Unpromised(orders, _) => orders
	}

	def promise: Option[Identity] = this match {
		case Promised(_, id, _) => Some(id)
		case Sarcastic(_, id) => Some(id)
		case _ => None
	}

case class PlayLink(
	orders: List[Int],
	prereqs: IdentitySet,
	target: Int
)

case class Player(
	playerIndex: Int,
	name: String,
	allPossible: IdentitySet,
	hypoStacks: Vector[Int],

	isCommon: Boolean,
	thoughts: Vector[Thought] = Vector(),
	allInferred: IdentitySet,

	links: List[Link] = Nil,
	playLinks: List[PlayLink] = Nil,
	unknownPlays: BitSet = BitSet.empty,
	hypoPlays: BitSet = BitSet.empty,

	dirty: BitSet = BitSet.empty,
	certainMap: Vector[List[MatchEntry]]
):
	def withThought(order: Int)(f: Thought => Thought) =
		copy(
			thoughts = thoughts.updated(order, f(thoughts(order))),
			dirty = dirty + order
		)

	def strInfs(state: State, order: Int) =
		thoughts(order).inferred.toList.sortBy(_.toOrd).map(state.logId).mkString(",")

	def strPoss(state: State, order: Int) =
		thoughts(order).possible.toList.sortBy(_.toOrd).map(state.logId).mkString(",")

	def refer(game: Game, hand: Vector[Int], order: Int, left: Boolean = false) =
		val offset = if left then -1 else 1
		val index = hand.indexOf(order)
		var targetIndex = (index + offset + hand.length) % hand.length

		while game.isTouched(hand(targetIndex)) && targetIndex != index do
			targetIndex = (targetIndex + offset + hand.length) % hand.length

		hand(targetIndex)

	def chopNewest(game: Game, playerIndex: Int) =
		game.state.hands(playerIndex).find(o =>
			!game.state.deck(o).clued && game.meta(o).status == CardStatus.None)

	def isSieved(game: Game, id: Identity, order: Int) =
		(0 until game.state.numPlayers).exists(playerIndex =>
			val loaded = thinksLoaded(game, playerIndex)
			val chop = chopNewest(game, playerIndex)

			game.state.hands(playerIndex).exists(o =>
				o != order && thoughts(o).matches(id, infer = true) &&
				(if loaded then
					game.meta(o).status != CardStatus.CalledToDiscard
				else
					chop.forall(_ != o))
			)
		) ||
		links.exists {
			case Link.Unpromised(orders, ids) =>
				!orders.contains(order) && ids.contains(id)
			case link =>
				!link.getOrders.contains(order) && link.promise.contains(id)
		}

	def isDuped(game: Game, id: Identity, order: Int) =
		val candidates = if game.goodTouch then
			game.state.hands.flatten
		else
			game.state.hands(game.state.holderOf(order))

		candidates.exists { o =>
			o != order &&
			thoughts(o).matches(id, infer = true) &&
			// Not sharing a link
			!links.exists {
				case Link.Unpromised(orders, ids) =>
					orders.contains(order) && orders.contains(o) && ids.contains(id)
				case link =>
					link.getOrders.contains(order) && link.getOrders.contains(o) && link.promise.contains(id)
			}
		}

	def isTrash(game: Game, id: Identity, order: Int) =
		game.state.isBasicTrash(id) || isDuped(game, id, order)

	def orderTrash(game: Game, order: Int) =
		val meta = game.meta(order)
		val thought = thoughts(order)

		lazy val conventionalTrash =
			thought.possible.forall(isTrash(game, _, order) ||
			thought.infoLock.exists(_.forall(isTrash(game, _, order))) ||
			meta.trash ||
			meta.status == CardStatus.CalledToDiscard)

		if thought.possible.forall(game.state.isCritical) then
			false
		else
			conventionalTrash || thought.possibilities.forall(isTrash(game, _, order))

	def orderKt(game: Game, order: Int) =
		val thought = thoughts(order)

		(game.meta(order).trash && thought.possible.forall(id => !game.state.isCritical(id))) ||
		thought.possible.forall(game.state.isBasicTrash) ||
		(thought.inferred.isEmpty && thought.reset)

	def orderKp(game: Game, order: Int, excludeTrash: Boolean = false) =
		val state = game.state
		val thought = thoughts(order)

		inline def possPlayable(poss: IdentitySet) =
			val p = if excludeTrash then poss.difference(state.trashSet) else poss
			p.intersect(state.playableSet) == p

		game.meta(order).status match {
			case CardStatus.CalledToPlay =>
				thought.possible.intersect(state.playableSet).nonEmpty &&
				thought.infoLock.forall(_.intersect(state.playableSet).nonEmpty)

			case CardStatus.Sarcastic | CardStatus.GentlemansDiscard =>
				possPlayable(thought.inferred)

			case _ =>
				possPlayable(thought.possible) ||
				thought.infoLock.exists(possPlayable)
		}

	def orderPlayable(game: Game, order: Int, excludeTrash: Boolean = false) =
		val state = game.state

		if orderKp(game, order, excludeTrash) then
			true
		else if game.meta(order).trash || links.exists(l => l.getOrders.contains(order) && l.getOrders.max != order) then
			false
		else
			val infer =
				(playerIndex != state.ourPlayerIndex || state.strikes != 2 || game.meta(order).focused) &&
				game.meta(order).status != CardStatus.CalledToDiscard

			val poss = if infer then thoughts(order).possibilities else thoughts(order).possible
			val p = if excludeTrash then poss.difference(state.trashSet) else poss

			p.intersect(state.playableSet) == p

	def obviousPlayables(game: Game, playerIndex: Int) =
		game.state.hands(playerIndex).filter(orderKp(game, _))

	def thinksPlayables(game: Game, playerIndex: Int) =
		game.state.hands(playerIndex).filter(orderPlayable(game, _))
			.pipe { playables =>
				// Exclude unknown cards if there is a duplicate that is fully known.
				playables.filterNot { p1 =>
					thoughts(p1).id().isEmpty &&
					playables.exists { p2 =>
						p1 != p2 &&
						thoughts(p2).id().exists(thoughts(p1).matches(_, infer = true))
					}
				}
			}
			.pipe(game.filterPlayables(this, playerIndex, _))

	def thinksTrash(game: Game, playerIndex: Int) =
		game.state.hands(playerIndex).filter(orderTrash(game, _))

	def discardable(game: Game, playerIndex: Int) =
		game.state.hands(playerIndex).filter { order =>
			orderTrash(game, order) || thoughts(order).possibilities.forall(isSieved(game, _, order))
		}

	def thinksLoaded(game: Game, playerIndex: Int) =
		thinksPlayables(game, playerIndex).nonEmpty || thinksTrash(game, playerIndex).nonEmpty

	def thinksLocked(game: Game, playerIndex: Int) =
		!thinksLoaded(game, playerIndex) && game.state.hands(playerIndex).forall { order =>
			game.state.deck(order).clued ||
			game.isBlindPlaying(order) ||
			game.meta(order).status == CardStatus.GentlemansDiscard ||
			game.meta(order).cm
		}

	def obviousLoaded(game: Game, playerIndex: Int) =
		obviousPlayables(game, playerIndex).nonEmpty || thinksTrash(game, playerIndex).nonEmpty

	def obviousLocked(game: Game, playerIndex: Int) =
		!obviousLoaded(game, playerIndex) && game.state.hands(playerIndex).forall { order =>
			game.state.deck(order).clued ||
			game.meta(order).status == CardStatus.CalledToPlay ||
			game.meta(order).cm
		}

	def findPrompt(prev: Game, playerIndex: Int, id: Identity, connected: List[Int] = Nil, ignore: Set[Int] = Set(), forcePink: Boolean = false, rightmost: Boolean = false) =
		val state = prev.state
		val hand = state.hands(playerIndex).pipe(h => if rightmost then h.reverse else identity(h))
		val order = hand.find { order =>
			val card = state.deck(order)
			val thought = thoughts(order)

			!connected.contains(order) &&				// not already connected
			state.deck(order).clued &&
			thought.possible.contains(id) &&			// must be a possibility
			thought.infoLock.forall(_.contains(id)) &&
			(thought.inferred.length != 1 || thought.inferred.contains(id)) &&	// not info-locked on a different id
			card.clues.exists(state.variant.idTouched(id, _)) &&	// at least one clue matches
			{
				// Not trying to prompt a pink id, or forcing pink prompt
				if !PINKISH.matches(state.variant.suits(id.suitIndex)) || forcePink then
					true
				else
					val clues = card.clues
					val misranked =
						clues.forall(_.isEq(clues.head)) &&
						clues.head.kind == ClueKind.Rank &&
						clues.head.value != id.rank

					lazy val colourMatch = card.clues.exists { c =>
						c.kind == ClueKind.Colour &&
						PINKISH.matches(state.variant.colourableSuits(c.value))
					}

					!misranked && colourMatch
			}
		}

		order.filter(!ignore.contains(_))


	def findClued(prev: Game, playerIndex: Int, id: Identity, ignore: Set[Int] = Set()) =
		val state = prev.state
		state.hands(playerIndex).filter { order =>
			val thought = thoughts(order)

			!ignore.contains(order) &&				// not already connected
			state.deck(order).clued &&
			thought.possible.contains(id) &&			// must be a possibility
			thought.infoLock.forall(_.contains(id)) &&
			(thought.inferred.length != 1 || thought.inferred.contains(id))	// not info-locked on a different id
		}

	/**
	* Returns how far the identity is from playable (through cards known by this player).
	* 0 means that it is playable.
	*/
	def playableAway(id: Identity) =
		id.rank - (hypoStacks(id.suitIndex) + 1)

	def lockedDiscard(state: State, playerIndex: Int) =
		val critPercents = state.hands(playerIndex).map { o =>
			val poss = thoughts(o).possibilities
			val percent = poss.intersect(state.criticalSet).length / poss.length
			(o, percent)
		}.sortBy(_._2)

		val leastCrits = critPercents.filter(_._2 == critPercents(0)._2)

		leastCrits.maxBy { (order, percent) =>
			thoughts(order).possibilities.map { p =>
				val critDistance = (if percent == 1 then p.rank * 5 else 0) + p.rank - hypoStacks(p.suitIndex)
				if critDistance < 0 then 5 else critDistance
			}.sum
		}._1

	def unknownIds(state: State, id: Identity) =
		val visibleCount = state.hands.flatten.filter(thoughts(_).matches(id)).length
		state.cardCount(id.toOrd) - state.baseCount(id.toOrd) - visibleCount

	def linkedOrders(state: State) =
		links.flatMap {
			case Link.Promised(orders, id, _) =>
				if orders.length > unknownIds(state, id) then orders else List()
			case Link.Sarcastic(orders, id) =>
				if orders.length > unknownIds(state, id) then orders else List()
			case Link.Unpromised(orders, ids) =>
				if orders.length > ids.map(unknownIds(state, _)).sum then orders else List()
		}

	def updateHypoStacks[G <: Game](game: G)(using ops: GameOps[G]): Player =
		val state = game.state
		var hypo = game
		var unknownPlays = BitSet.empty
		var played = BitSet.empty

		var endIndex = 0
		val viableOrders = Array.ofDim[Int](state.numPlayers * HAND_SIZE(state.numPlayers))

		state.hands.fastForeach { hand =>
			hand.fastForeach { order =>
				val id = state.deck(order).id()

				if id.isEmpty || !state.isBasicTrash(id.get) then
					viableOrders(endIndex) = order
					endIndex += 1
			}
		}

		def play(order: Int) =
			thoughts(order).id(infer = true, symmetric = true) match {
				case None =>
					links.fastForeach { link =>
						val orders = link.getOrders
						if orders.contains(order) && orders.forall(o => o == order || played.contains(o)) then
							hypo = link.promise.fold(hypo) { id =>
								if !hypo.state.isPlayable(id) then
									Log.warn(s"tried to add linked ${state.logId(id)} ($order) onto hypo stacks, but they were at ${hypo.state.playStacks} $played ($name)")
									hypo
								else
									hypo.withState(_.withPlay(id))
							}
					}
					unknownPlays = unknownPlays + order
					played = played + order

				case Some(id) =>
					if hypo.state.isPlayable(id) then
						hypo = hypo.withState(_.withPlay(id))
						played = played + order
					else
						Log.warn(s"tried to add ${state.logId(id)} ($order) onto hypo stacks, but they were at ${hypo.state.playStacks} $played ($name)")
			}

		var changed = true

		while changed do {
			changed = false
			var viableIndex = 0
			loop(0, _ < endIndex, _ + 1) { i =>
				val order = viableOrders(i)
				val thought = thoughts(order)

				val playable = hypo.state.hasConsistentInfs(thought) &&
					(if game.goodTouch then orderPlayable(hypo, order) else orderKp(hypo, order))

				// Should this be symmetric? Checking whether a playable or not should be, but another player can know what id it is
				if playable && !played.contains(order) then
					play(order)
					changed = true
				else
					viableOrders(viableIndex) = order
					viableIndex += 1
			}
			endIndex = viableIndex

			playLinks.fastForeach { link =>
				val allPlayed = link.orders.fastForall(played.contains)

				if allPlayed && !played.contains(link.target) then
					val order = link.target
					val id = state.deck(order).id()

					if id.isEmpty || !state.isBasicTrash(id.get) then
						play(link.target)
			}
		}

		this.copy(
			hypoStacks = hypo.state.playStacks,
			unknownPlays = unknownPlays,
			hypoPlays = played
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
			allInferred = allPossible,
			certainMap = Vector.fill(allPossible.length)(Nil)
		)
