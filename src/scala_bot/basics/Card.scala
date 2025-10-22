package scala_bot.basics

trait Identifiable:
	def suitIndex: Int
	def rank: Int

	def id(infer: Boolean = false, symmetric: Boolean = false): Option[Identity]

	def matches(other: Identifiable, infer: Boolean = false, symmetric: Boolean = false, assume: Boolean = false): Boolean =
		id(infer, symmetric) match {
			case None => assume
			case Some(a) =>
				other.id(infer, symmetric).exists { b =>
					a.suitIndex == b.suitIndex && a.rank == b.rank
				}
		}

case class Identity(suitIndex: Int, rank: Int) extends Identifiable:
	def id(infer: Boolean = false, symmetric: Boolean = false) =
		Some(this)

	inline def toOrd: Int =
		suitIndex * 5 + (rank - 1)

	def prev: Identity =
		if (rank == 1)
			throw new Error(s"Tried to get prev of $this!")

		Identity(suitIndex, rank - 1)

	def next: Identity =
		if (rank == 5)
			throw new Error(s"Tried to get next of $this!")

		Identity(suitIndex, rank + 1)

object Identity {
	inline def fromOrd(ord: Int): Identity =
		if (ord < 30)
			val suitIndex = ord / 5
			val rank = (ord % 5) + 1
			Identity(suitIndex, rank)
		else
			throw new Error(s"Couldn't convert ordinal $ord to identity!")
}

enum CardStatus:
	case None, ChopMoved, CalledToPlay, CalledToDiscard, Finessed, Sarcastic, GentlemansDiscard, ZeroClueChop

	override def toString(): String =
		this match {
			case CardStatus.None => "none"
			case CardStatus.ChopMoved => "chop moved"
			case CardStatus.CalledToPlay => "called to play"
			case CardStatus.CalledToDiscard => "called to discard"
			case CardStatus.Finessed => "finessed"
			case CardStatus.Sarcastic => "sarcastic"
			case CardStatus.GentlemansDiscard => "gentleman's discard"
			case CardStatus.ZeroClueChop => "zero clue chop"
		}

case class Card(
	suitIndex: Int,
	rank: Int,
	order: Int,
	drawnIndex: Int,
	clued: Boolean = false,
	clues: List[CardClue] = List()
) extends Identifiable:
	def id(infer: Boolean = false, symmetric: Boolean = false) =
		Option.when(suitIndex != -1 && rank != -1)(Identity(suitIndex, rank))

case class Thought(
	suitIndex: Int,
	rank: Int,
	order: Int,
	possible: IdentitySet,
	inferred: IdentitySet,
	oldInferred: Option[IdentitySet] = None,
	infoLock: Option[IdentitySet] = None,
	rewinded: Boolean = false,
	reset: Boolean = false
) extends Identifiable:
	def id(infer: Boolean = false, symmetric: Boolean = false) =
		if (possible.length == 1) {
			Some(possible.iterator.next)
		}
		else if (!symmetric && suitIndex != -1) {
			Some(Identity(suitIndex, rank))
		}
		else {
			Option.when(infer && inferred.length == 1)(inferred.head)
		}

	inline def possibilities: IdentitySet =
		if (inferred.isEmpty) { possible } else { inferred }

	def resetInferences(): Thought =
		if (reset) {
			return this
		}
		this.copy(
			reset = true,
			inferred = infoLock match {
				case None => possible
				case Some(ids) => possible.intersect(ids)
			}
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
	reasoning: List[Int] = List(),
	by: Option[Int] = None
):
	inline def cm = status == CardStatus.ChopMoved

	def cleared = copy(
		focused = false,
		urgent = false,
		trash = false,
		status = if (status == CardStatus.ZeroClueChop) status else CardStatus.None,
		by = None
	)

	def reason(turnCount: Int) =
		if (reasoning.lastOption.forall(_ != turnCount))
			copy(reasoning = turnCount +: reasoning)
		else
			this
