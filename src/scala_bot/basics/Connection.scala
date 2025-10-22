package scala_bot.basics

sealed trait Connection:
	def reacting: Int
	def order: Int
	def ids: List[Identity]

	def kind: String

case class KnownConn(
	reacting: Int,
	order: Int,
	ids: List[Identity],
	asymmetric: Boolean = false
) extends Connection:
	def kind = "known"

case class PlayableConn(
	reacting: Int,
	order: Int,
	ids: List[Identity],
	linked: List[Int] = List(),
	layered: Boolean = false
) extends Connection:
	def kind = "playable"

case class PromptConn(
	reacting: Int,
	order: Int,
	ids: List[Identity]
) extends Connection:
	def kind = "prompt"

case class FinesseConn(
	reacting: Int,
	order: Int,
	ids: List[Identity],
	self: Boolean = false,
	hidden: Boolean = false,
	bluff: Boolean = false,
	possiblyBluff: Boolean = false,
	certain: Boolean = false
) extends Connection:
	def kind =
		if (possiblyBluff) "possiblyBluff"
		else if (bluff) "bluff"
		else "finesse"

case class FocusPossibility(
	id: Identity,
	connections: List[Connection],
	interp: Interp,
	save: Boolean = false,
	illegal: Boolean = false
)

case class WaitingConnection(
	connections: List[Connection],
	giver: Int,
	target: Int,
	connIndex: Int,
	turn: Int,
	focus: Int,
	inference: Identity,
	ambiguousPassback: Boolean = false,
	selfPassback: Boolean = false,
	symmetric: Boolean = false,
	rewinded: Boolean = false
)
