package scala_bot.basics

enum ClueKind:
	case Colour, Rank

trait ClueLike:
	def kind: ClueKind
	def value: Int

	def base: BaseClue = BaseClue(kind, value)

	def fmt(state: State, target: Int) =
		val clueValue = kind match {
			case ClueKind.Colour => state.variant.suits(value).toLowerCase
			case ClueKind.Rank => value.toString
		}
		s"($clueValue to ${state.names(target)})"

	def isEq(other: ClueLike): Boolean =
		kind == other.kind && value == other.value

case class BaseClue(kind: ClueKind, value: Int) extends ClueLike:
	def hash: Int =
		(if kind == ClueKind.Colour then 0 else 10) + value

	def toClue(target: Int) =
		Clue(kind, value, target)

object BaseClue:
	def fromJSON(json: ujson.Value) =
		val kind = json.obj("type").num match {
			case 0 => ClueKind.Colour
			case 1 => ClueKind.Rank
			case _ => throw new IllegalArgumentException("Invalid clue kind")
		}
		BaseClue(kind, json.obj("value").num.toInt)

case class CardClue(
	kind: ClueKind,
	value: Int,
	giver: Int,
	turn: Int
) extends ClueLike

case class Clue(
	kind: ClueKind,
	value: Int,
	target: Int
) extends ClueLike:
	def toBase: BaseClue =
		BaseClue(kind, value)

	def fmt(state: State) =
		toBase.fmt(state, target)
