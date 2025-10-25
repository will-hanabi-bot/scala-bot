package scala_bot.basics

import scala_bot.logger.Log

enum Link:
	case Promised(orders: List[Int], id: Identity, target: Int)
	case Sarcastic(orders: List[Int], id: Identity)
	case Unpromised(orders: List[Int], ids: List[Identity])

	def getOrders: List[Int] = this match {
		case Promised(orders, _, _) => orders
		case Sarcastic(orders, _) => orders
		case Unpromised(orders, _) => orders
	}

	def promise: Option[Identity] = this match {
		case Promised(_, id, _) => Some(id)
		case Sarcastic(_, id) => Some(id)
		case _ => None
	}

case class Player(
	playerIndex: Int,
	name: String,
	allPossible: IdentitySet,
	hypoStacks: Vector[Int],

	isCommon: Boolean,
	thoughts: Vector[Thought] = Vector(),
	allInferred: IdentitySet,

	links: List[Link] = List(),
	unknownPlays: Set[Int] = Set(),
	hypoPlays: Set[Int] = Set(),

	waiting: List[WaitingConnection] = List()
):
	def withThought(order: Int)(f: Thought => Thought) =
		copy(thoughts = thoughts.updated(order, f(thoughts(order))))

	def strInfs(state: State, order: Int) =
		thoughts(order).inferred.toSeq.sortBy(_.toOrd).map(state.logId).mkString(",")

	def strPoss(state: State, order: Int) =
		thoughts(order).possible.toSeq.sortBy(_.toOrd).map(state.logId).mkString(",")

	def refer(game: Game, hand: Vector[Int], order: Int, left: Boolean = false) =
		val offset = if (left) -1 else 1
		val index = hand.indexOf(order)
		var targetIndex = (index + offset + hand.length) % hand.length

		while (game.isTouched(hand(targetIndex)) && targetIndex != index)
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
				(if (loaded)
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
		game.state.hands(game.state.holderOf(order)).exists { o =>
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

		if (thought.possible.forall(game.state.isCritical))
			false
		else if (thought.possible.forall(isTrash(game, _, order) || meta.trash || meta.status == CardStatus.CalledToDiscard))
			true
		else
			thought.possibilities.forall(isTrash(game, _, order))

	def orderKt(game: Game, order: Int) =
		val thought = thoughts(order)

		(game.meta(order).trash && thought.possible.forall(id => !game.state.isCritical(id))) ||
		thought.possible.forall(game.state.isBasicTrash)

	def orderKp(game: Game, order: Int) =
		val state = game.state
		val thought = thoughts(order)

		game.meta(order).status match {
			case CardStatus.CalledToPlay | CardStatus.Finessed =>
				thought.possible.exists(state.isPlayable) &&
				thought.infoLock.forall(_.exists(state.isPlayable))

			case CardStatus.Sarcastic | CardStatus.GentlemansDiscard =>
				thought.inferred.forall(state.isPlayable)

			case _ =>
				thought.possible.forall(state.isPlayable) ||
				thought.infoLock.exists(_.forall(state.isPlayable))
		}

	def orderPlayable(game: Game, order: Int) =
		val state = game.state
		lazy val unsafeLink = links.exists(link =>
			link.getOrders.contains(order) && link.getOrders.max != order)

		if (orderKp(game, order))
			true
		else if (game.meta(order).trash || unsafeLink)
			false
		else
			val infer =
				(playerIndex != state.ourPlayerIndex || state.strikes != 2 || game.meta(order).focused) &&
				game.meta(order).status != CardStatus.CalledToDiscard

			val poss = if (infer) thoughts(order).possibilities else thoughts(order).possible
			poss.forall(state.isPlayable)

	def obviousPlayables(game: Game, playerIndex: Int) =
		game.state.hands(playerIndex).filter(orderKp(game, _))

	def thinksPlayables(game: Game, playerIndex: Int) =
		game.state.hands(playerIndex).filter(orderPlayable(game, _))

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
			game.meta(order).status == CardStatus.CalledToPlay ||
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

	/**
	* Returns how far the identity is from playable (through cards known by this player).
	* 0 means that it is playable.
	*/
	def playableAway(id: Identity) =
		id.rank - (hypoStacks(id.suitIndex) + 1)

	def lockedDiscard(state: State, playerIndex: Int) =
		val critPercents = state.hands(playerIndex).map { o =>
			val poss = thoughts(o).possibilities
			val percent = poss.retain(state.isCritical).length / poss.length
			(o, percent)
		}.sortBy(_._2)

		val leastCrits = critPercents.filter(_._2 == critPercents(0)._2)

		leastCrits.maxBy { (order, percent) =>
			thoughts(order).possibilities.map { p =>
				val critDistance = (if (percent == 1) p.rank * 5 else 0) + p.rank - hypoStacks(p.suitIndex)
				if (critDistance < 0) 5 else critDistance
			}.sum
		}._1

	def unknownIds(state: State, id: Identity) =
		val visibleCount = state.hands.flatten.filter(thoughts(_).matches(id)).length
		state.cardCount(id.toOrd) - state.baseCount(id.toOrd) - visibleCount

	def linkedOrders(state: State) =
		links.flatMap {
			case Link.Promised(orders, id, _) =>
				if (orders.length > unknownIds(state, id)) orders else List()
			case Link.Sarcastic(orders, id) =>
				if (orders.length > unknownIds(state, id)) orders else List()
			case Link.Unpromised(orders, ids) =>
				if (orders.length > ids.map(unknownIds(state, _)).sum) orders else List()
		}

	def dependentConns(order: Int) =
		waiting.filter { wc =>
			wc.connections.zipWithIndex.exists((c, i) => i > wc.connIndex && c.order == order)
		}

	def updateHypoStacks[G <: Game](game: G)(using ops: GameOps[G]): Player =
		case class HypoStruct(
			hypo: G,
			unknownPlays: Set[Int],
			played: Set[Int],
			viable: Set[Int]
		)

		def processRound(struct: HypoStruct): HypoStruct =
			struct._4.foldLeft(struct) { (acc, order) =>
				val HypoStruct(hypo, unknownPlays, played, viable) = acc
				val thought = thoughts(order)

				val skip = !hypo.state.hasConsistentInfs(thought) || !(if (game.goodTouch) orderPlayable(hypo, order) else orderKp(hypo, order))

				if (skip) acc else thought.id(infer = true, symmetric = true) match {
					case None =>
						val fulfilledLinks = links.filter(l => l.getOrders.contains(order) && l.getOrders.forall(played.contains(_)))
						var successful = false

						val newHypo = fulfilledLinks.foldLeft(hypo) { (h, link) =>
							link.promise.fold(h) { id =>
								if (!h.state.isPlayable(id))
									Log.warn(s"tried to add linked ${h.state.logId(id)} ($order) onto hypo stacks, but they were at ${h.state.playStacks} $played")
									h
								else
									successful = true
									h.withState(_.withPlay(id))
							}
						}

						acc.copy(
							hypo = if (successful) newHypo else hypo,
							unknownPlays = unknownPlays + order,
							played = played + order,
							viable = viable - order
						)
					case Some(id) =>
						if (hypo.state.isPlayable(id))
							acc.copy(
								hypo = hypo.withState(_.withPlay(id)),
								unknownPlays = unknownPlays + order,
								played = played + order,
								viable = viable - order
							)
						else
							Log.warn(s"tried to add ${hypo.state.logId(id)} ($order) onto hypo stacks, but they were at ${hypo.state.playStacks} $played")
							acc.copy(viable = viable - order)
				}
			}

		val viable = game.state.hands.flatten
			.filter(game.state.deck(_).id().forall(!game.state.isBasicTrash(_)))
			.toSet
		val initialS = HypoStruct(game, Set(), Set(), viable)
		val HypoStruct(hypo, unknownPlays, played, _) = Iterator.iterate(initialS)(processRound)
			.sliding(2)
			.find { case Seq(prev, curr) => prev == curr }
			.map(_.last)
			.getOrElse(initialS)

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
			allInferred = allPossible
		)
