package scala_bot.basics

trait Identifiable:
	def suitIndex: Int
	def rank: Int

	def id(infer: Boolean = false, symmetric: Boolean = false): Option[Identity]

	def matches(other: Identifiable, infer: Boolean = false, symmetric: Boolean = false, assume: Boolean = false): Boolean =
		id(infer, symmetric).fold(assume): a =>
			other.id(infer, symmetric).exists: b =>
				a.suitIndex == b.suitIndex && a.rank == b.rank

case class Identity(suitIndex: Int, rank: Int) extends Identifiable:
	def id(infer: Boolean = false, symmetric: Boolean = false) =
		Some(this)

	inline def toOrd: Int =
		suitIndex * 5 + (rank - 1)

	def prev: Option[Identity] =
		Option.when(rank > 1)(Identity(suitIndex, rank - 1))

	def next: Option[Identity] =
		Option.when(rank < 5)(Identity(suitIndex, rank + 1))

	def playedBefore(id: Identity) =
		suitIndex == id.suitIndex && rank < id.rank

object Identity:
	inline def fromOrd(ord: Int): Identity =
		if ord < 30 then
			val suitIndex = ord / 5
			val rank = (ord % 5) + 1
			Identity(suitIndex, rank)
		else
			throw new Error(s"Couldn't convert ordinal $ord to identity!")

enum CardStatus:
	case None,
		ChopMoved,
		CalledToPlay,
		CalledToDiscard,
		Finessed,
		Sarcastic,
		GentlemansDiscard,
		FMaybeBluffed,
		MaybeBluffed,
		Bluffed

	override def toString(): String =
		this match
			case CardStatus.None => "none"
			case CardStatus.ChopMoved => "chop moved"
			case CardStatus.CalledToPlay => "called to play"
			case CardStatus.CalledToDiscard => "called to discard"
			case CardStatus.Finessed => "finessed"
			case CardStatus.Sarcastic => "sarcastic"
			case CardStatus.GentlemansDiscard => "gentleman's discard"
			case CardStatus.FMaybeBluffed => "finessed, maybe bluffed"
			case CardStatus.MaybeBluffed => "maybe bluffed"
			case CardStatus.Bluffed => "bluffed"

case class Card(
	suitIndex: Int,
	rank: Int,
	order: Int,
	drawnIndex: Int,
	clued: Boolean = false,
	clues: List[CardClue] = Nil
) extends Identifiable:
	def id(infer: Boolean = false, symmetric: Boolean = false) =
		Option.when(suitIndex != -1 && rank != -1)(Identity(suitIndex, rank))

case class Thought(
	suitIndex: Int,
	rank: Int,
	order: Int,
	possible: IdentitySet,
	inferred: IdentitySet,
	oldInferred: IdentitySetOpt = IdentitySetOpt.empty,
	oldPossible: IdentitySetOpt = IdentitySetOpt.empty,
	infoLock: IdentitySetOpt = IdentitySetOpt.empty,
	rewinded: Boolean = false,
	reset: Boolean = false
) extends Identifiable:
	def id(infer: Boolean = false, symmetric: Boolean = false) =
		if possible.length == 1 then
			Some(possible.head)
		else if !symmetric && suitIndex != -1 then
			Some(Identity(suitIndex, rank))
		else
			Option.when(infer && inferred.length == 1)(inferred.head)

	inline def possibilities: IdentitySet =
		if inferred.isEmpty then possible else inferred

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

case class ConvData(
	order: Int,
	focused: Boolean = false,
	urgent: Boolean = false,
	trash: Boolean = false,
	status: CardStatus = CardStatus.None,
	hidden: Boolean = false,
	reasoning: List[Int] = Nil,
	signalTurn: Option[Int] = None,
	by: Option[Int] = None
):
	inline def cm = status == CardStatus.ChopMoved

	def cleared = copy(
		focused = false,
		urgent = false,
		trash = false,
		status = if status == CardStatus.ChopMoved then status else CardStatus.None,
		signalTurn = None,
		by = None
	)

	def reason(turnCount: Int) =
		if reasoning.lastOption.forall(_ != turnCount) then
			copy(reasoning = turnCount +: reasoning)
		else
			this

	def signal(turnCount: Int) =
		copy(signalTurn = signalTurn.orElse(Some(turnCount)))
