package scala_bot.basics

import scala_bot.utils._

/** Information about a particular card in a sequence of plays.
  * @define reactingDesc The index of the player holding the card.
  * @define orderDesc The position of the card in the deck (0 is the topmost card).
  */
sealed trait Connection:
	/** $reactingDesc */
	def reacting: Int
	/** $orderDesc */
	def order: Int

	/** The possible ids the card could be. */
	def ids: List[Identity]

	/** A human-readable version of the type of connection. */
	def kind: String

	/** Whether the reacting player knows this card is part of the connection (e.g. in a *Hidden Finesse*). */
	def hidden: Boolean

	def isBluff = this.matchesP:
		case f: FinesseConn => f.bluff

	def isPossiblyBluff = this.matchesP:
		case f: FinesseConn => f.bluff || f.possiblyBluff

/** A [[Connection]] where the reacting player already knew about this card before the clue.
  * @param reacting $reactingDesc
  * @param order $orderDesc
  * @param id The promised identity of the card.
  */
case class KnownConn(
	reacting: Int,
	order: Int,
	id: Identity
) extends Connection:
	def ids = List(id)
	def kind = "known"
	def hidden = false

/** A [[Connection]] where the reacting player already knew this card was playable (but not its identity) before the clue.
  * @param reacting $reactingDesc
  * @param order $orderDesc
  * @param id The promised identity of the card.
  * @param linked The orders of cards that look identical to this one. Any of these cards could be the promised identity.
  * @param insertingInto The orders of connecting cards that this is inserting in front of (i.e. in a *Layered Finesse*), if any.
  */
case class PlayableConn(
	reacting: Int,
	order: Int,
	id: Identity,
	linked: Seq[Int] = Nil,
	hidden: Boolean = false,
	/** The orders of connecting cards that this is inserting in front of (i.e. in a *Layered Finesse*), if any. */
	insertingInto: Option[Seq[Int]] = None
) extends Connection:
	def ids = List(id)
	def kind = "playable"

/** A [[Connection]] where the reacting player is prompted for this identity from the clue.
  * @param reacting $reactingDesc
  * @param order $orderDesc
  * @param id The promised identity of the card.
  */
case class PromptConn(
	reacting: Int,
	order: Int,
	id: Identity,
	hidden: Boolean = false
) extends Connection:
	def ids = List(id)
	def kind = "prompt"

/** A [[Connection]] where the reacting player is finessed for this identity from the clue.
  * @param reacting $reactingDesc
  * @param order $orderDesc
  * @param id The promised identity of the card.
  */
case class FinesseConn(
	reacting: Int,
	order: Int,
	ids: List[Identity],
	hidden: Boolean = false,
	bluff: Boolean = false,
	possiblyBluff: Boolean = false,
	/** Whether this is part of a *Certain Finesse*. */
	certain: Boolean = false,
	ambiguousPassback: Boolean = false
) extends Connection:
	def kind =
		if possiblyBluff then "possiblyBluff"
		else if bluff then "bluff"
		else "finesse"

/** A [[Connection]] where the reacting player is clued to play a specific slot without being touched.
  * @param reacting $reactingDesc
  * @param order $orderDesc
  * @param id The promised identity of the card.
  */
case class PositionalConn(
	reacting: Int,
	order: Int,
	ids: List[Identity],
	/** If possible, the targeted order and playable possibilities if the reacting player should be us instead. */
	ambiguousOwn: Option[(Int, List[Identity])]
) extends Connection:
	def kind = "positional"
	def hidden = false
/**
  * @define symmetricDesc Whether this is something the clue receiver must entertain, but is actually false.
  * @define ambiguousDesc Whether this involves us when we see another possibility not involving us.
  */
case class FocusPossibility(
	id: Identity,
	connections: List[Connection],
	interp: Interp,
	/** $symmetricDesc */
	symmetric: Boolean = false,
	/** ambiguousDesc */
	ambiguous: Boolean = false,
	/** Whether this is a save clue. */
	save: Boolean = false,
):
	def isBluff =
		connections.headOption.existsM:
			case f: FinesseConn => f.bluff

/** A potential set of connections that needs more information before its truthiness can be determined.
  * @param connections   The involved connections if true.
  * @param giver         The index of the player who gave the clue.
  * @param target        The index of the player who received the clue.
  * @param turn          The turn the clue was given.
  * @param focus         The order of the card focused.
  * @param inference     The identity of the focused card if true.
  * @param ambiguousPassback Whether this involved an *Ambiguous Passback*.
  * @param selfPassback  Whether this involved a *Self-Passback* (where the receiver has a self-connection but hesitated for someone else).
  * @param symmetric     $symmetricDesc
  * @param ambiguousSelf $ambiguousDesc
  */
case class WaitingConnection(
	connections: List[Connection],
	giver: Int,
	target: Int,
	turn: Int,
	/** The order of the card focused. */
	focus: Int,
	/** The identity of the focused card if true. */
	inference: Identity,
	/** Whether this involved an *Ambiguous Passback*. */
	ambiguousPassback: Boolean = false,
	/** Whether this involved a *Self-Passback* (where the receiver has a self-connection but hesitated for someone else). */
	selfPassback: Boolean = false,
	/** $symmetricDesc */
	symmetric: Boolean = false,
	/** $ambiguousDesc */
	ambiguousSelf: Boolean = false
):
	/** Returns the index of the next connection that still exists. */
	def getNextIndex(state: State) =
		val index = connections.indexWhere: conn =>
			state.hands(conn.reacting).contains(conn.order)

		Option.when(index != -1)(index)

	/** The upcoming connection. */
	inline def currConn = connections.head
