package scala_bot.basics

/**
  * The trait inherited by anything with a suitIndex and rank, like [[Identity]], [[Card]], and [[Thought]].
  * Requires id() to be defined, and provides matches() based on it.
  */
trait Identifiable:
	def suitIndex: Int
	def rank: Int

	/** Returns the contained identity, or None if unknown.
	  *
	  * @param infer Whether to return Some(inf) if there is exactly 1 inference and is otherwise unknown.
	  * @param symmetric Whether to ignore info from "looking" at the card.
	  * @param partial Whether to return partial information about an unknown identity. May return -1 for suitIndex or rank.
	  */
	def id(infer: Boolean = false, symmetric: Boolean = false, partial: Boolean = false): Option[Identity]

	/** Returns whether this identity is the same as the other.
	  * If the other's identity is unknown, returns false.
	  *
	  * @param other
	  * @param infer Whether to identify each card if they have exactly 1 inference and are otherwise unknown.
	  * @param symmetric Whether to identify each card without info from "looking" at them.
	  * @param assume The value returned if this identity is unknown.
	  */
	def matches(other: Identifiable, infer: Boolean = false, symmetric: Boolean = false, assume: Boolean = false): Boolean =
		id(infer, symmetric).fold(assume): a =>
			other.id(infer, symmetric).exists: b =>
				a.suitIndex == b.suitIndex && a.rank == b.rank

/** The pairing of a suitIndex and rank.*/
case class Identity(suitIndex: Int, rank: Int) extends Identifiable:
	def id(infer: Boolean = false, symmetric: Boolean = false, partial: Boolean = false) =
		Some(this)

	/** Converts the identity to its ordinal. */
	inline def toOrd: Int =
		suitIndex * 5 + (rank - 1)

	/** Returns the previous identity in the suit, if it exists. */
	def prev: Option[Identity] =
		Option.when(rank > 1)(Identity(suitIndex, rank - 1))

	/** Returns the next identity in the suit, if it exists. */
	def next: Option[Identity] =
		Option.when(rank < 5)(Identity(suitIndex, rank + 1))

	/** Returns true if this is of the same suit and can be played before the given identity. */
	def playedBefore(id: Identity) =
		suitIndex == id.suitIndex && rank < id.rank

object Identity:
	/** Converts an ordinal to an identity.
	  * @throws IllegalArgumentException If the ordinal is out of range to be converted.
	 */
	inline def fromOrd(ord: Int): Identity =
		if ord >= 0 && ord < 30 then
			val suitIndex = ord / 5
			val rank = (ord % 5) + 1
			Identity(suitIndex, rank)
		else
			throw new IllegalArgumentException(s"Couldn't convert ordinal $ord to identity!")

/** The conventional status of the card (e.g. chop moved, called to discard). */
enum CardStatus:
	case None
	case ChopMoved
	case CalledToPlay
	case CalledToDiscard
	case PermissionToDiscard
	case Finessed
	case Sarcastic
	case GentlemansDiscard
	/** Bluffed or finessed, but definitely blind playing. */
	case FMaybeBluffed
	case MaybeBluffed
	case Bluffed

	override def toString(): String =
		this match
			case CardStatus.None => "none"
			case CardStatus.ChopMoved => "chop moved"
			case CardStatus.CalledToPlay => "called to play"
			case CardStatus.CalledToDiscard => "called to discard"
			case CardStatus.PermissionToDiscard => "permission to discard"
			case CardStatus.Finessed => "finessed"
			case CardStatus.Sarcastic => "sarcastic"
			case CardStatus.GentlemansDiscard => "gentleman's discard"
			case CardStatus.FMaybeBluffed => "finessed, maybe bluffed"
			case CardStatus.MaybeBluffed => "maybe bluffed"
			case CardStatus.Bluffed => "bluffed"

/** The physical card in the game. */
case class Card(
	suitIndex: Int,
	rank: Int,
	/** The position of the card in the deck (0 is the topmost card). */
	order: Int,
	turnDrawn: Int,
	/** Whether the card has been positively touched by a clue. */
	clued: Boolean = false,
	/** The clues that have touched this card. */
	clues: List[CardClue] = Nil
) extends Identifiable:
	def id(infer: Boolean = false, symmetric: Boolean = false, partial: Boolean = false) =
		Option.when(suitIndex != -1 && rank != -1)(Identity(suitIndex, rank))

/** What a player thinks about a card. */
case class Thought(
	suitIndex: Int,
	rank: Int,
	/** The position of the card in the deck (0 is the topmost card). */
	order: Int,
	/** The possible ids of this card based on clues and what this player knows (except "looking" at the card). */
	possible: IdentitySet,
	/** The possible ids of this card based on conventional information and what this player knows. Always a subset of [[possible]], may be empty. */
	inferred: IdentitySet,
	/** When possible, the previous inferences of this card. Only used when a finesse may not be real. */
	oldInferred: IdentitySetOpt = IdentitySetOpt.empty,
	/** When possible, the possible ids of this card before it was fully known as a result of being played or discarded. */
	oldPossible: IdentitySetOpt = IdentitySetOpt.empty,
	/** The promised ids of this card based on information lock. */
	infoLock: IdentitySetOpt = IdentitySetOpt.empty,
	/** Whether the identity of this card is known from a rewind. */
	rewinded: Boolean = false,
	/** Whether this card has previously lost all of its inferences (e.g. due to being bad touched and fixed). */
	reset: Boolean = false
) extends Identifiable:
	def id(infer: Boolean = false, symmetric: Boolean = false, partial: Boolean = false) =
		if possible.length == 1 then
			Some(possible.head)
		else if !symmetric && suitIndex != -1 then
			Some(Identity(suitIndex, rank))
		else if infer && inferred.length == 1 then
			Some(inferred.head)
		else if partial then
			val Identity(suitIndex, rank) = possible.head
			if possible.forall(_.suitIndex == suitIndex) then
				Some(Identity(suitIndex, -1))
			else if possible.forall(_.rank == rank) then
				Some(Identity(-1, rank))
			else
				None
		else
			None

	/** Returns the inferences of the card, or the possible ids if there are none. */
	inline def possibilities: IdentitySet =
		if inferred.isEmpty then possible else inferred

	/** Returns a copy of the card with the inferences reset based on the possible ids and info lock. */
	def resetInferences(): Thought =
		if reset then
			return this

		val newInfoLock =
			if !infoLock.isDefined then infoLock else
				val ids = infoLock.get.intersect(possible)
				if ids.isEmpty then IdentitySetOpt.empty else ids.toOpt

		this.copy(
			reset = true,
			inferred =
				if newInfoLock.isDefined then
					possible.intersect(newInfoLock.get)
				else
					possible,
			infoLock = newInfoLock
		)

object Thought:
	def apply(
		suitIndex: Int,
		rank: Int,
		order: Int,
		possible: IdentitySet
	): Thought =
		Thought(suitIndex, rank, order, possible, possible)

/** The conventional information on a card common to all players. */
case class ConvData(
	order: Int,
	focused: Boolean = false,
	urgent: Boolean = false,
	/** Whether this card is conventionally promised trash, even if it has non-trash possibilities. */
	trash: Boolean = false,
	status: CardStatus = CardStatus.None,
	/** Whether this card is unknown to the person playing (like in a *Hidden Finesse*). */
	hidden: Boolean = false,
	/** The list of turns where something new was discovered about the card. */
	reasoning: List[Int] = Nil,
	/** The turn when this card was signalled, if it exists. */
	signalTurn: Option[Int] = None,
	/** The index of the player who signalled this card, if they exist. */
	by: Option[Int] = None
):
	/** Whether this card has been chop moved. */
	inline def cm = status == CardStatus.ChopMoved

	inline def bluffed =
		status == CardStatus.Bluffed ||
		status == CardStatus.FMaybeBluffed ||
		status == CardStatus.MaybeBluffed

	/** Returns a copy with all flags returned to default.
	  * Does NOT remove a [cm] note.
	 */
	def cleared = copy(
		focused = false,
		urgent = false,
		trash = false,
		status = if status == CardStatus.ChopMoved then status else CardStatus.None,
		signalTurn = None,
		by = None
	)

	/** Adds an entry to the reasoning list.  */
	def reason(turnCount: Int) =
		if reasoning.lastOption.forall(_ != turnCount) then
			copy(reasoning = turnCount +: reasoning)
		else
			this

	/** Indicates the turn when this card was signalled.
	  * Does nothing if called on an already-signalled card.
	 */
	def signal(turnCount: Int) =
		copy(signalTurn = signalTurn.orElse(Some(turnCount)))
