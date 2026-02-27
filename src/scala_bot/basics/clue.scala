package scala_bot.basics

enum ClueKind:
	case Colour, Rank

/** Generic trait that all clues extend, containing a [[kind]] and a [[value]].
  * Provides [[ClueLike.base]] to convert to a BaseClue, [[ClueLike.fmt]] to pretty-print, and [[ClueLike.isEq]] for equality.
  */
trait ClueLike:
	def kind: ClueKind
	def value: Int

	def base: BaseClue = BaseClue(kind, value)

	def fmt(state: State, target: Int) =
		val clueValue = kind match
			case ClueKind.Colour => state.variant.suits(value).name.toLowerCase
			case ClueKind.Rank => value.toString
		s"($clueValue to ${state.names(target)})"

	def isEq(other: ClueLike): Boolean =
		kind == other.kind && value == other.value

/** The pairing of a clue kind and value.
  * @param kind  Whether the clue is rank or colour.
  * @param value The rank of a rank clue, or the suitIndex of a colour clue.
  */
case class BaseClue(kind: ClueKind, value: Int) extends ClueLike:
	def hash: Int =
		(if kind == ClueKind.Colour then 0 else 10) + value

	def toClue(target: Int) =
		Clue(kind, value, target)

object BaseClue:
	def fromJSON(json: ujson.Value) =
		val kind = json.obj("type").num match
			case 0 => ClueKind.Colour
			case 1 => ClueKind.Rank
			case _ => throw new IllegalArgumentException("Invalid clue kind")
		BaseClue(kind, json.obj("value").num.toInt)

/** A clue that has touched a card.
  * @param kind   Whether the clue is rank or colour.
  * @param value  The rank of a rank clue, or the suitIndex of a colour clue.
  * @param giver  The index of the player who gave the clue.
  * @param turn   The turn on which the clue was given.
  */
case class CardClue(
	kind: ClueKind,
	value: Int,
	giver: Int,
	turn: Int
) extends ClueLike

/** A clue that can be given.
  * @param kind   Whether the clue is rank or colour.
  * @param value  The rank of a rank clue, or the suitIndex of a colour clue.
  * @param target  The index of the player to receive this clue.
  */
case class Clue(
	kind: ClueKind,
	value: Int,
	target: Int
) extends ClueLike:
	def fmt(state: State) =
		base.fmt(state, target)
