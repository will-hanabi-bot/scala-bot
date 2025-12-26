package scala_bot.basics

import scala_bot.logger.Log

sealed trait Action:
	def fmt(state: State): String

	def playerIndex: Int

	def requiresDraw: Boolean

case class StatusAction(
	clues: Int,
	score: Int,
	maxScore: Int
) extends Action:
	def fmt(state: State) =
		s"Status! Clues: $clues, Score: $score/$maxScore"

	def playerIndex = -1

	def requiresDraw = false

object StatusAction:
	def fromJSON(json: ujson.Value) =
		StatusAction(
			json.obj("clues").num.toInt,
			json.obj("score").num.toInt,
			json.obj("maxScore").num.toInt
		)

case class TurnAction(
	num: Int,
	currentPlayerIndex: Int
) extends Action:
	def fmt(state: State) =
		s"Turn $num (${state.names(currentPlayerIndex)})"

	def playerIndex = currentPlayerIndex

	def requiresDraw = false

object TurnAction:
	def fromJSON(json: ujson.Value) =
		TurnAction(
			json.obj("num").num.toInt,
			json.obj("currentPlayerIndex").num.toInt
		)

case class ClueAction(
	giver: Int,
	target: Int,
	list: Seq[Int],
	clue: BaseClue
) extends Action:
	def fmt(state: State) =
		val value = clue.kind match
			case ClueKind.Colour => state.variant.colourableSuits(clue.value).toLowerCase()
			case ClueKind.Rank => clue.value.toString
		s"${state.names(giver)} clues $value to ${state.names(target)}"

	def playerIndex = giver

	def requiresDraw = false

object ClueAction:
	def fromJSON(json: ujson.Value) =
		ClueAction(
			json.obj("giver").num.toInt,
			json.obj("target").num.toInt,
			json.obj("list").arr.toSeq.map(_.num.toInt),
			BaseClue.fromJSON(json.obj("clue"))
		)

def logId(state: State, suitIndex: Int, rank: Int) =
	if suitIndex == -1 || rank == -1 then
		"xx"
	else
		state.logId(Identity(suitIndex, rank))

case class DrawAction(
	playerIndex: Int,
	order: Int,
	suitIndex: Int,
	rank: Int
) extends Action:
	def fmt(state: State) =
		s"${state.names(playerIndex)} draws ${logId(state, suitIndex, rank)} ($order)"

	def requiresDraw = false

object DrawAction:
	def fromJSON(json: ujson.Value) =
		DrawAction(
			json.obj("playerIndex").num.toInt,
			json.obj("order").num.toInt,
			json.obj("suitIndex").num.toInt,
			json.obj("rank").num.toInt
		)

case class PlayAction(
	playerIndex: Int,
	order: Int,
	suitIndex: Int,
	rank: Int
) extends Action:
	def fmt(state: State) =
		s"${state.names(playerIndex)} plays ${logId(state, suitIndex, rank)} ($order)"

	def requiresDraw = true

object PlayAction:
	def fromJSON(json: ujson.Value) =
		PlayAction(
			json.obj("playerIndex").num.toInt,
			json.obj("order").num.toInt,
			json.obj("suitIndex").num.toInt,
			json.obj("rank").num.toInt
		)

	def apply(playerIndex: Int, order: Int, id: Option[Identity]): PlayAction =
		id match
			case Some(Identity(suitIndex, rank)) =>
				PlayAction(playerIndex, order, suitIndex, rank)
			case None =>
				PlayAction(playerIndex, order, -1, -1)

case class DiscardAction(
	playerIndex: Int,
	order: Int,
	suitIndex: Int,
	rank: Int,
	failed: Boolean = false
) extends Action:
	def fmt(state: State) =
		val verb = if failed then "bombs" else "discards"
		s"${state.names(playerIndex)} $verb ${logId(state, suitIndex, rank)} ($order)"

	def requiresDraw = true

object DiscardAction:
	def fromJSON(json: ujson.Value) =
		DiscardAction(
			json.obj("playerIndex").num.toInt,
			json.obj("order").num.toInt,
			json.obj("suitIndex").num.toInt,
			json.obj("rank").num.toInt,
			json.obj("failed").bool
		)

	def apply(playerIndex: Int, order: Int, id: Option[Identity]): DiscardAction =
		id match
			case Some(Identity(suitIndex, rank)) =>
				DiscardAction(playerIndex, order, suitIndex, rank)
			case None =>
				DiscardAction(playerIndex, order, -1, -1)

case class StrikeAction(
	num: Int,
	turn: Int,
	order: Int
) extends Action:
	def fmt(state: State) = ""

	def playerIndex = -1

	def requiresDraw = false

object StrikeAction:
	def fromJSON(json: ujson.Value) =
		StrikeAction(
			json.obj("num").num.toInt,
			json.obj("turn").num.toInt,
			json.obj("order").num.toInt
		)

case class GameOverAction(
	endCondition: Int,
	playerIndex: Int
) extends Action:
	def fmt(state: State) = "Game over!"

	def requiresDraw = false

object GameOverAction:
	def fromJSON(json: ujson.Value) =
		GameOverAction(
			json.obj("endCondition").num.toInt,
			json.obj("playerIndex").num.toInt
		)

case class InterpAction(interp: ClueInterp) extends Action:
	def fmt(state: State) = ""

	def playerIndex = -1

	def requiresDraw = false

object InterpAction:
	def fromJSON(json: ujson.Value) =
		throw new Exception("can't parse InterpAction from json!")

object Action:
	def fromJSON(json: ujson.Value) =
		json.obj("type").str match
			case "clue"    => Some(ClueAction.fromJSON(json))
			case "discard" => Some(DiscardAction.fromJSON(json))
			case "play"    => Some(PlayAction.fromJSON(json))
			case "draw"    => Some(DrawAction.fromJSON(json))
			case "status"  => Some(StatusAction.fromJSON(json))
			case "turn"    => Some(TurnAction.fromJSON(json))
			case "strike"  => Some(StrikeAction.fromJSON(json))
			case "gameOver" => Some(GameOverAction.fromJSON(json))
			case _     => None

def addAction(actionList: Vector[List[Action]], action: Action, turn: Int) =
	if turn < actionList.size then
		if actionList(turn).contains(action) then
			Log.error(s"Action $action already exists in turn $turn")
			actionList
		else
			actionList.updated(turn, actionList(turn) :+ action)
	else if turn == actionList.size then
		actionList :+ List(action)
	else
		throw new IndexOutOfBoundsException(s"Attempted to add action to turn $turn, but action list had size ${actionList.size}")

enum PerformAction:
	case Play(target: Int)
	case Discard(target: Int)
	case Colour(target: Int, value: Int)
	case Rank(target: Int, value: Int)
	case Terminate(target: Int, value: Int)

	def isClue: Boolean = this match
		case Colour(_, _) | Rank(_, _) => true
		case _ => false

	def fmt(game: Game, accordingTo: Option[Player] = None) =
		val state = game.state
		val player = accordingTo.getOrElse(game.common)

		this match
			case PerformAction.Play(target) =>
				val slot = state.ourHand.indexOf(target) + 1
				s"Play slot $slot, inferences ${player.strInfs(state, target)}"

			case PerformAction.Discard(target) =>
				val slot = state.ourHand.indexOf(target) + 1
				s"Discard slot $slot, inferences ${player.strInfs(state, target)}"

			case PerformAction.Colour(target, value) =>
				Clue(ClueKind.Colour, value, target).fmt(state)

			case PerformAction.Rank(target, value) =>
				Clue(ClueKind.Rank, value, target).fmt(state)

			case PerformAction.Terminate(target, value) =>
				s"Game ended: $target $value"

	def fmtObj(game: Game, playerIndex: Int) =
		val (state, deckIds) = (game.state, game.deckIds)

		val actionType = this match
			case PerformAction.Play(target) =>
				s"play ${state.logId(deckIds(target))}, order $target"

			case PerformAction.Discard(target) =>
				s"discard ${state.logId(deckIds(target))}, order $target"

			case _ => fmt(game)
		s"$actionType (${state.names(playerIndex)})"

	def json(tableID: Int) =
		this match
			case PerformAction.Play(target) =>
				ujson.Obj("tableID" -> tableID, "type" -> 0, "target" -> target)
			case PerformAction.Discard(target) =>
				ujson.Obj("tableID" -> tableID, "type" -> 1, "target" -> target)
			case PerformAction.Colour(target, value) =>
				ujson.Obj("tableID" -> tableID, "type" -> 2, "target" -> target, "value" -> value)
			case PerformAction.Rank(target, value) =>
				ujson.Obj("tableID" -> tableID, "type" -> 3, "target" -> target, "value" -> value)
			case PerformAction.Terminate(target, value) =>
				ujson.Obj("tableID" -> tableID, "type" -> 4, "target" -> target, "value" -> value)

object PerformAction:
	def fromJSON(json: ujson.Value): PerformAction =
		val actionType = json("type").num.toInt
		lazy val target = json("target").num.toInt
		lazy val value = json("value").num.toInt

		actionType match
			case 0 => PerformAction.Play(target)
			case 1 => PerformAction.Discard(target)
			case 2 => PerformAction.Colour(target, value)
			case 3 => PerformAction.Rank(target, value)
			case 4 => PerformAction.Terminate(target, value)
