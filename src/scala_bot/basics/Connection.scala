package scala_bot.basics

sealed trait Connection:
	def reacting: Int
	def order: Int
	def ids: List[Identity]

	def kind: String
	def hidden: Boolean

	def isBluff = this match {
		case f: FinesseConn => f.bluff
		case _ => false
	}

	def isPossiblyBluff = this match {
		case f: FinesseConn => f.bluff
		case _ => false
	}

case class KnownConn(
	reacting: Int,
	order: Int,
	id: Identity,
	asymmetric: Boolean = false
) extends Connection:
	def ids = List(id)
	def kind = "known"
	def hidden = false

case class PlayableConn(
	reacting: Int,
	order: Int,
	id: Identity,
	linked: List[Int] = Nil,
	layered: Boolean = false
) extends Connection:
	def ids = List(id)
	def kind = "playable"
	def hidden = false

case class PromptConn(
	reacting: Int,
	order: Int,
	id: Identity,
	hidden: Boolean = false
) extends Connection:
	def ids = List(id)
	def kind = "prompt"

case class FinesseConn(
	reacting: Int,
	order: Int,
	ids: List[Identity],
	hidden: Boolean = false,
	bluff: Boolean = false,
	possiblyBluff: Boolean = false,
	certain: Boolean = false,
	ambiguousPassback: Boolean = false
) extends Connection:
	def kind =
		if (possiblyBluff) "possiblyBluff"
		else if (bluff) "bluff"
		else "finesse"

case class PositionalConn(
	reacting: Int,
	order: Int,
	ids: List[Identity]
):
	def kind = "positional"
	def hidden = false

case class FocusPossibility(
	id: Identity,
	connections: List[Connection],
	interp: Interp,
	symmetric: Boolean = false,
	save: Boolean = false,
	illegal: Boolean = false
):
	def isBluff =
		connections.headOption.exists({
			case f: FinesseConn => f.bluff
			case _ => false
		})

case class WaitingConnection(
	connections: List[Connection],
	giver: Int,
	target: Int,
	turn: Int,
	focus: Int,
	inference: Identity,
	ambiguousPassback: Boolean = false,
	selfPassback: Boolean = false,
	symmetric: Boolean = false,
	rewinded: Boolean = false
):
	/** Returns the index of the next connection that still exists. */
	def getNextIndex(state: State) =
		val index = connections.indexWhere { conn =>
			state.hands(conn.reacting).contains(conn.order)
		}
		Option.when(index != -1)(index)

	inline def currConn = connections.head
