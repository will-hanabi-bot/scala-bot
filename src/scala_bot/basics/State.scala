package scala_bot.basics

import scala_bot.utils._
import scala.util.matching.Regex
import scala.util.hashing.MurmurHash3

case class State(
	turnCount: Int = 0,
	clueTokens: Int = 8,
	strikes: Int = 0,
	endgameTurns: Option[Int] = None,
	nextCardOrder: Int = 0,
	cardsLeft: Int,

	playStacks: Vector[Int],
	discardStacks: Vector[Vector[Vector[Int]]],
	maxRanks: Vector[Int],
	baseCount: Vector[Int],

	variant: Variant,
	allIds: IdentitySet,
	playableSet: IdentitySet,
	criticalSet: IdentitySet,
	trashSet: IdentitySet,
	cardCount: Vector[Int],

	numPlayers: Int,
	names: Vector[String],
	hands: Vector[Vector[Int]],
	deck: Vector[Card] = Vector(),

	ourPlayerIndex: Int,
	actionList: Vector[List[Action]] = Vector(),
	currentPlayerIndex: Int = 0
):
	def hash =
		val deckInts = deck.map(_.id().fold(0)(_.toOrd))
		MurmurHash3.productHash((hands, deckInts, clueTokens, endgameTurns))

	def withDiscard(id: Identity, order: Int) =
		val Identity(suitIndex, rank) = id
		val newStacks = discardStacks.updated(suitIndex,
			discardStacks(suitIndex).updated(rank - 1, order +: discardStacks(suitIndex)(rank - 1)))

		val newBase = baseCount.updated(id.toOrd, baseCount(id.toOrd) + 1)

		val (newMax, newCritical, newTrash, newPlayable) =
			if (criticalSet.contains(id))
				(maxRanks.updated(suitIndex, math.min(maxRanks(suitIndex), rank - 1)),
				criticalSet.difference(id),
				trashSet.union(id),
				playableSet.difference(id))
			else
				val critical = cardCount(id.toOrd) - newBase(id.toOrd) == 1 && !isBasicTrash(id)
				(maxRanks,
				if (critical) criticalSet.union(id) else criticalSet,
				trashSet,
				playableSet)

		copy(
			discardStacks = newStacks,
			baseCount = newBase,
			maxRanks = newMax,
			playableSet = newPlayable,
			criticalSet = newCritical,
			trashSet = newTrash
		)

	def withPlay(id: Identity) =
		val newPlayable = id.next match {
			case Some(next) => playableSet.difference(id).union(next)
			case None => playableSet.difference(id)
		}

		copy(
			playStacks = playStacks.updated(id.suitIndex, id.rank),
			baseCount = baseCount.updated(id.toOrd, baseCount(id.toOrd) + 1),
			playableSet = newPlayable,
			trashSet = trashSet.union(id)
		).when(_ => id.rank == 5)(_.regainClue)

	def tryPlay(id: Identity) =
		if (isPlayable(id)) withPlay(id) else this

	def regainClue =
		copy(clueTokens = 8.min(clueTokens + 1))

	def ended =
		strikes == 3 || score == maxScore || endgameTurns.contains(0)

	def score = playStacks.sum
	def maxScore = maxRanks.sum
	def remScore = maxScore - score

	def pace = score + cardsLeft + numPlayers - maxScore
	def inEndgame = pace < numPlayers || score >= maxScore - 5

	def lastPlayerIndex(playerIndex: Int) =
		(playerIndex + numPlayers - 1) % numPlayers

	def nextPlayerIndex(playerIndex: Int) =
		(playerIndex + 1) % numPlayers

	def isBasicTrash(id: Identity) =
		id.rank <= playStacks(id.suitIndex) || id.rank > maxRanks(id.suitIndex)

	def playableAway(id: Identity) =
		id.rank - (playStacks(id.suitIndex) + 1)

	def isPlayable(id: Identity) = playableAway(id) == 0

	def isCritical(id: Identity) =
		!isBasicTrash(id) && discardStacks(id.suitIndex)(id.rank - 1).length == cardCount(id.toOrd) - 1

	def ourHand = hands(ourPlayerIndex)

	def clueTouched(orders: Seq[Int], clue: ClueLike) =
		orders.filter(order => variant.cardTouched(deck(order), clue))

	def allColourClues(target: Int) =
		(0 until variant.colourableSuits.length).view.map(Clue(ClueKind.Colour, _, target))
			.filter(clue => !clueTouched(hands(target), clue.toBase).isEmpty)

	def allValidClues(target: Int) =
		val rankClues = (1 to 5).view.map(Clue(ClueKind.Rank, _, target))
		val colourClues = (0 until variant.colourableSuits.length).view.map(Clue(ClueKind.Colour, _, target))

		rankClues.concat(colourClues).filter(clue => !clueTouched(hands(target), clue.toBase).isEmpty)

	def hasConsistentInfs(thought: Thought) =
		thought.possible.length == 1 ||
		deck(thought.order).id().forall(thought.inferred.contains) ||
		thought.matches(Identity(1, 1), infer = true)

	def canClue = clueTokens > 0

	def includesVariant(regex: Regex) =
		variant.suits.exists(regex.matches)

	def multiplicity(ids: IdentitySet) =
		var count = 0
		ids.foreachFast { id =>
			count += cardCount(id.toOrd)
		}
		count

	def holderOf(order: Int): Int =
		var i = 0
		while (i < numPlayers) {
			val hand = hands(i)
			var j = 0

			while (j < hand.length) {
				if (hand(j) == order)
					return i
				j += 1
			}

			i += 1
		}
		throw new IllegalArgumentException(s"Tried to get holder of $order, hands were $hands!")

	def inStartingHand(order: Int)=
		order < numPlayers * HAND_SIZE(numPlayers)

	def expandShort(short: String) =
		if (short.length != 2)
			throw new IllegalArgumentException(s"Short should be exactly 2 characters! (eg. r5)")

		val suitIndex = variant.shortForms.indexOf(short.charAt(0))
		if (suitIndex == -1)
			throw new IllegalArgumentException(s"Colour $short doesn't exist in selected variant!")

		if (!short.charAt(1).isDigit)
			throw new IllegalArgumentException(s"Rank $short doesn't exist in selected variant!")

		Identity(suitIndex, short.charAt(1) - '0')

	def logId(id: Identity) =
		s"${variant.shortForms(id.suitIndex)}${id.rank}"

	def logId(id: Option[Identity]): String =
		id match {
			case Some(id) => logId(id)
			case None => "xx"
		}

	def logId(id: Identifiable): String =
		logId(id.id())

	def logId(order: Int): String =
		logId(deck(order).id())

	def logConn(conn: Connection) =
		val ids = conn.ids
		val idStr = if (ids.length == 1) logId(ids.head) else s"[${ids.map(logId).mkString(",")}]"
		val extra = conn match {
			case f: FinesseConn => if (f.certain) " (certain)" else if (f.hidden) " (hidden)" else ""
			case _ => ""
		}

		s"${conn.order} $idStr ${conn.kind} (${names(conn.reacting)})$extra"

	def logConns(conns: Seq[Connection]) =
		s"[${conns.map(logConn).mkString(" -> ")}]"

	def logConns(conns: Seq[Connection], nextId: Identity) =
		val terminator = if (nextId.rank > 5) "" else s" -> ${logId(nextId)}?"

		s"[${conns.map(logConn).mkString(" -> ")}$terminator]"

object State:
	def apply(
		names: Vector[String],
		ourPlayerIndex: Int,
		variant: Variant
	): State =
		State(
			cardsLeft = (0 until variant.suits.length).foldLeft(0)((acc, suitIndex) =>
				acc + (1 to 5).map(rank => variant.cardCount(Identity(suitIndex, rank))).sum),
			cardCount = (0 until variant.suits.length).flatMap(suitIndex =>
				(1 to 5).map(rank => variant.cardCount(Identity(suitIndex, rank)))).toVector,

			playStacks = Vector.fill(variant.suits.length)(0),
			discardStacks = Vector.fill(variant.suits.length)(Vector.fill(5)(Vector())),
			maxRanks = Vector.fill(variant.suits.length)(5),
			baseCount = Vector.fill(variant.suits.length * 5)(0),

			variant = variant,
			allIds = IdentitySet.from(variant.allIds),
			playableSet = IdentitySet.from((0 until variant.suits.length).map(Identity(_, 1))),
			criticalSet = IdentitySet.from((0 until variant.suits.length).map(Identity(_, 5))),
			trashSet = IdentitySet.empty,
			numPlayers = names.length,

			names = names,
			hands = Vector.fill(names.length)(Vector()),

			ourPlayerIndex = ourPlayerIndex)
