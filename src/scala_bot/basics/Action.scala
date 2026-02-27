package scala_bot.basics

import scala_bot.logger.Log

/** A game action (e.g. ClueAction, TurnAction) sent from hanab.live as part of the game state.
  * This is different from a [[PerformAction]], which describes an action taken by a player to send to hanab.live.
  */
sealed trait Action:
	def fmt(state: State): String

	def playerIndex: Int

	def requiresDraw: Boolean

/** @param clues The number of clues in the bank.
  * @param score The current score.
  * @param maxScore The maximum score possible, given the current state.
  */
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

/** @param num The turn number (1-indexed).
  * @param currentPlayerIndex The index of the player whose turn it is.
  */
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

/** @param giver The index of the player who gave this clue.
  * @param target The index of the player who received this clue.
  * @param list The orders touched by the clue.
  * @param clue The clue kind and value.
  */
case class ClueAction(
	giver: Int,
	target: Int,
	list: Seq[Int],
	clue: BaseClue
) extends Action:
	def fmt(state: State) =
		val value = clue.kind match
			case ClueKind.Colour => state.variant.colourableSuits(clue.value).name.toLowerCase()
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

/** Returns a human-readable string of an identity.
  * Accepts -1 for [[suitIndex]] and [[rank]].
  */
def logId(state: State, suitIndex: Int, rank: Int) =
	if suitIndex == -1 || rank == -1 then
		"xx"
	else
		state.logId(Identity(suitIndex, rank))

/** @param playerIndex The index of the player who drew the card.
  * @param order The position of the card in the deck (0 is the topmost card).
  * @param suitIndex The index of the suit of the card.
  * @param rank The rank of the played card.
  */
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

/** @param playerIndex The index of the player who played the card.
  * @param order The position of the card in the deck (0 is the topmost card).
  * @param suitIndex The index of the suit of the played card.
  * @param rank The rank of the played card.
  */
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

/** @param playerIndex The index of the player who discarded the card.
  * @param order The position of the card in the deck (0 is the topmost card).
  * @param suitIndex The index of the suit of the discarded card.
  * @param rank The rank of the discarded card.
  * @param failed Whether the discard occurred due to a strike.
  */
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

/** @param num The current number of strikes.
  * @param turn The current turn number.
  * @param order The order of the card that caused the strike.
  */
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

enum EndCondition:
	case InProgress, Normal, Strikeout, Timeout, Terminated,
		SpeedrunFail, IdleTimeout, CharacterSoftlock, AllOrNothingFail, AllOrNothingSoftlock,
		TerminatedByVote

/** @param endCondition The reason for the game over (see [[EndCondition]]).
  * @param playerIndex The index of the player who caused the game over.
  */
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

/** A custom action used to signal that the next clue should be interpreted a particular way.
  * Typically inserted after rewinding when something has been proven.
  * @param interp The intended interpretation of the upcoming clue.
  */
case class InterpAction(interp: ClueInterp) extends Action:
	def fmt(state: State) = ""

	def playerIndex = -1

	def requiresDraw = false

object InterpAction:
	def fromJSON(json: ujson.Value) =
		throw new Exception("can't parse InterpAction from json!")

object Action:
	def fromJSON(json: ujson.Value): Option[Action] =
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

/** Returns an updated action list after trying to insert the action on the specified turn.
  * Does nothing if that turn already contains the action.
  *
  * @param actionList The existing list of actions.
  * @param action The action to insert.
  * @param turn The turn to insert the action on.
  * @throws IndexOutOfBoundsException If the action list is too short for the turn provided.
  */
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

/** A game action to be taken by a user and sent to hanab.live.
  * This is different from an [[Action]], which is a game action sent by hanab.live to describe the game state.
  *
  * Variants: Play, Discard, Colour, Rank, Terminate
  */
enum PerformAction:
	/** @param target The order of the card to play. */
	case Play(target: Int)
	/** @param target The order of the card to discard. */
	case Discard(target: Int)
	/** @param target The index of the player to clue.
	  * @param value The index of the colour to clue (leftmost is 0, skipping colourless suits).
	  */
	case Colour(target: Int, value: Int)
	/** @param target The index of the player to clue.
	  * @param value The rank to clue.
	  */
	case Rank(target: Int, value: Int)
	/** @param target ??
	  * @param value ??
	  */
	case Terminate(target: Int, value: Int)

	def hash: Int =
		this match
			case Play(target) =>
				target
			case Discard(target) =>
				10 + target
			case Colour(target, value) =>
				20 + target + value * 100
			case Rank(target, value) =>
				30 + target + value * 100
			case Terminate(target, value) =>
				-1

	def isClue: Boolean = this match
		case Colour(_, _) | Rank(_, _) => true
		case _ => false

	/** Returns a human-readable string of the action (must be from our hand).
	  * For plays and discards, a player can be provided whose inferences will be used.
	  * Otherwise, the common perspective will be used.
	  */
	def fmt(game: Game, accordingTo: Option[Player] = None) =
		val state = game.state
		val player = accordingTo.getOrElse(game.common)

		this match
			case Play(target) =>
				val slot = state.ourHand.indexOf(target) + 1
				s"Play slot $slot, inferences ${player.strInfs(state, target)}"

			case Discard(target) =>
				val slot = state.ourHand.indexOf(target) + 1
				s"Discard slot $slot, inferences ${player.strInfs(state, target)}"

			case Colour(target, value) =>
				Clue(ClueKind.Colour, value, target).fmt(state)

			case Rank(target, value) =>
				Clue(ClueKind.Rank, value, target).fmt(state)

			case Terminate(target, value) =>
				s"Game ended: $target $value"

	/** Returns a human-readable string of the action performed by the player whose index is provided.
	  * Unlike [[PerformAction.fmt]], this can be from any player, not only us.
	  */
	def fmtObj(game: Game, playerIndex: Int) =
		val (state, deckIds) = (game.state, game.deckIds)

		val actionType = this match
			case Play(target) =>
				val id = deckIds(target)
				s"${if id.exists(!state.isPlayable(_)) then "bomb" else "play"} ${state.logId(id)}, order $target"

			case Discard(target) =>
				s"discard ${state.logId(deckIds(target))}, order $target"

			case _ => fmt(game)
		s"$actionType (${state.names(playerIndex)})"

	def json(tableID: Int) =
		this match
			case Play(target) =>
				ujson.Obj("tableID" -> tableID, "type" -> 0, "target" -> target)
			case Discard(target) =>
				ujson.Obj("tableID" -> tableID, "type" -> 1, "target" -> target)
			case Colour(target, value) =>
				ujson.Obj("tableID" -> tableID, "type" -> 2, "target" -> target, "value" -> value)
			case Rank(target, value) =>
				ujson.Obj("tableID" -> tableID, "type" -> 3, "target" -> target, "value" -> value)
			case Terminate(target, value) =>
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
