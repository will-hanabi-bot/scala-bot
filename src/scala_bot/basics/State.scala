package scala_bot.basics

import scala_bot.utils._
import scala.util.matching.Regex

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
	cardCount: Vector[Int],

	numPlayers: Int,
	names: Vector[String],
	hands: Vector[Vector[Int]],
	deck: Vector[Card] = Vector(),

	ourPlayerIndex: Int,
	actionList: Vector[List[Action]] = Vector(),
	currentPlayerIndex: Int = 0
):
	def withDiscard(id: Identity, order: Int) =
		val Identity(suitIndex, rank) = id
		val newStack = order +: discardStacks(suitIndex)(rank - 1)
		val newStacks = discardStacks.updated(suitIndex,
			discardStacks(suitIndex).updated(rank - 1, newStack))

		copy(
			discardStacks = newStacks,
			baseCount = baseCount.updated(id.toOrd, baseCount(id.toOrd) + 1),
			maxRanks =
				if (newStack.length == cardCount(Identity(suitIndex, rank).toOrd))
					maxRanks.updated(suitIndex, math.min(maxRanks(suitIndex), rank - 1))
				else
					maxRanks
		)

	def withPlay(id: Identity) =
		copy(
			playStacks = playStacks.updated(id.suitIndex, id.rank),
			baseCount = baseCount.updated(id.toOrd, baseCount(id.toOrd) + 1),
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

	def clueTouched(orders: Seq[Int], clue: BaseClue) =
		orders.filter(order => variant.cardTouched(deck(order), clue))

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

	def remainingMultiplicity(ids: IdentitySet) =
		var count = 0
		ids.foreachFast { id =>
			count += cardCount(id.toOrd) - baseCount(id.toOrd)
		}
		count

	def holderOf(order: Int) =
		val holder = hands.indexWhere(_.contains(order))
		if (holder == -1)
			throw new IllegalArgumentException(s"Tried to get holder of $order, hands were $hands!")
		holder

	def expandShort(short: String) =
		if (short.length != 2)
			throw new IllegalArgumentException(s"Short should be exactly 2 characters! (eg. r5)")

		val suitIndex = variant.shortForms.indexWhere(_ == short.charAt(0))
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
			numPlayers = names.length,

			names = names,
			hands = Vector.fill(names.length)(Vector()),

			ourPlayerIndex = ourPlayerIndex)
