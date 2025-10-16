package scala_bot.basics

enum ClueKind:
	case Colour, Rank

case class BaseClue(kind: ClueKind, value: Int):
	def hash: Int =
		(if (kind == ClueKind.Colour) 0 else 10) + value

	def fmt(state: State, target: Int) =
		val clueValue = kind match {
			case ClueKind.Colour => state.variant.suits(value)
			case ClueKind.Rank => value.toString
		}
		s"($clueValue to ${state.names(target)})"

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
)

case class Clue(
	kind: ClueKind,
	value: Int,
	target: Int
):
	def toBase: BaseClue =
		BaseClue(kind, value)

	def fmt(state: State) =
		toBase.fmt(state, target)
