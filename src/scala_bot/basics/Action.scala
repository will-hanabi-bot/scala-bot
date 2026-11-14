package scala_bot.basics

import scala_bot.logger.Log

/** A game action (e.g. ClueAction, TurnAction) sent from hanab.live as part of the game state.
  * This is different from a [[PerformAction]], which describes an action taken by a player to send to hanab.live.
  */
sealed trait Action:
	def fmt(state: State): String
	def playerIndex: Int
	def requiresDraw: Boolean
	def isPlayerAction: Boolean

/** @param turn The turn number (1-indexed).
  * @param currentPlayerIndex The index of the player whose turn it is.
  */
case class TurnAction(
	turn: Int,
	currentPlayerIndex: Int
) extends Action:
	def fmt(state: State) =
		s"Turn $turn (${state.names(currentPlayerIndex)})"

	def playerIndex = currentPlayerIndex
	def requiresDraw = false
	def isPlayerAction = false

object TurnAction:
	def fromJSON(json: ujson.Value) =
		TurnAction(
			json.obj("turn").num.toInt,
			json.obj.get("currentPlayerIndex").fold(-1)(_.num.toInt)
		)

/** @param giver The index of the player who gave this clue.
  * @param receiver The index of the player who received this clue.
  * @param list The orders touched by the clue.
  * @param clue The clue kind and value.
  */
case class ClueAction(
	giver: Int,
	receiver: Int,
	orders: Seq[Int],
	clue: BaseClue
) extends Action:
	def fmt(state: State) =
		val value = clue.kind match
			case ClueKind.Colour => state.variant.colourableSuits(clue.value).name.toLowerCase()
			case ClueKind.Rank => clue.value.toString
		s"${state.names(giver)} clues $value to ${state.names(receiver)}"

	def playerIndex = giver
	def requiresDraw = false
	def isPlayerAction = true

object ClueAction:
	def fromJSON(json: ujson.Value) =
		ClueAction(
			json.obj("playerIndex").num.toInt,
			json.obj("receiverIndex").num.toInt,
			json.obj("orders").arr.toSeq.map(_.num.toInt),
			BaseClue.fromJSON(json.obj("clue"))
		)

def parseId(json: ujson.Value) =
	json.obj.get("identity").map: idObj =>
		Identity(idObj("suitIndex").num.toInt, idObj("rank").num.toInt)

/** @param playerIndex The index of the player who drew the card.
  * @param order The position of the card in the deck (0 is the topmost card).
  * @param suitIndex The index of the suit of the card.
  * @param rank The rank of the played card.
  */
case class DrawAction(
	playerIndex: Int,
	order: Int,
	identity: Option[Identity] = None
) extends Action:
	def fmt(state: State) =
		s"${state.names(playerIndex)} draws ${state.logId(identity)} ($order)"

	def requiresDraw = false
	def isPlayerAction = false

object DrawAction:
	def fromJSON(json: ujson.Value) =
		DrawAction(
			json.obj("playerIndex").num.toInt,
			json.obj("order").num.toInt,
			parseId(json)
		)

/** @param playerIndex The index of the player who played the card.
  * @param order The position of the card in the deck (0 is the topmost card).
  * @param suitIndex The index of the suit of the played card.
  * @param rank The rank of the played card.
  */
case class PlayAction(
	playerIndex: Int,
	order: Int,
	identity: Option[Identity] = None
) extends Action:
	def fmt(state: State) =
		s"${state.names(playerIndex)} plays ${state.logId(identity)} ($order)"

	def requiresDraw = true
	def isPlayerAction = true

object PlayAction:
	def fromJSON(json: ujson.Value) =
		PlayAction(
			json.obj("playerIndex").num.toInt,
			json.obj("order").num.toInt,
			parseId(json)
		)

/** @param playerIndex The index of the player who discarded the card.
  * @param order The position of the card in the deck (0 is the topmost card).
  * @param suitIndex The index of the suit of the discarded card.
  * @param rank The rank of the discarded card.
  * @param failed Whether the discard occurred due to a strike.
  */
case class DiscardAction(
	playerIndex: Int,
	order: Int,
	identity: Option[Identity] = None,
	failed: Boolean = false
) extends Action:
	def fmt(state: State) =
		val verb = if failed then "bombs" else "discards"
		s"${state.names(playerIndex)} $verb ${state.logId(identity)} ($order)"

	def requiresDraw = true
	def isPlayerAction = true

object DiscardAction:
	def fromJSON(json: ujson.Value, failed: Boolean = false) =
		DiscardAction(
			json.obj("playerIndex").num.toInt,
			json.obj("order").num.toInt,
			parseId(json),
			json.obj.get("failed").map(_.bool).getOrElse(failed)
		)

/** @param endCondition The reason for the game over (see [[EndCondition]]).
  * @param playerIndex The index of the player who caused the game over.
  */
case class GameOverAction() extends Action:
	def fmt(state: State) = "Game over!"
	def playerIndex = -1
	def requiresDraw = false
	def isPlayerAction = false

/** A custom action used to signal that the next clue should be interpreted a particular way.
  * Typically inserted after rewinding when something has been proven.
  * @param interp The intended interpretation of the upcoming clue.
  */
case class InterpAction(interp: ClueInterp) extends Action:
	def fmt(state: State) = ""
	def playerIndex = -1
	def requiresDraw = false
	def isPlayerAction = false

object InterpAction:
	def fromJSON(json: ujson.Value) =
		throw new Exception("can't parse InterpAction from json!")

object Action:
	def fromJSON(json: ujson.Value): Option[Action] =
		json.obj("type").str match
			case "clue"    => Some(ClueAction.fromJSON(json))
			case "discard" => Some(DiscardAction.fromJSON(json))
			case "misplay" => Some(DiscardAction.fromJSON(json, failed = true))
			case "play"    => Some(PlayAction.fromJSON(json))
			case "draw"    => Some(DrawAction.fromJSON(json))
			case "turn"    => Some(TurnAction.fromJSON(json))
			case "gameOver" => Some(GameOverAction())
			case _     => None

	def fromClue(state: State, clue: Clue, giver: Int): ClueAction =
		val orders = state.clueTouched(state.hands(clue.receiver), clue)
		ClueAction(giver, clue.receiver, orders, clue.base)

	def gerund(action: Action) =
		action match
			case _: PlayAction => "playing"
			case d: DiscardAction => if d.failed then "bombing" else "discarding"
			case _:	ClueAction => "cluing"
			case _ => throw new Error(s"trying to call gerund() on $action!")

	/**
	 * Returns the result of dragging this card to the discard pile.
	 * For inverted suits, this will return a PlayAction.
	 */
	def dragDiscard(state: State, playerIndex: Int, order: Int, id: Option[Identity] = None) =
		id.orElse(state.deck(order).id()) match
			case Some(i) =>
				if state.isInverted(i) then
					if state.isPlayable(i) then
						PlayAction(playerIndex, order, Some(i))
					else
						DiscardAction(playerIndex, order, Some(i), failed = true)
				else
					DiscardAction(playerIndex, order, Some(i))
			case _ =>
				DiscardAction(playerIndex, order, None)

	/**
	 * Returns the result of dragging this card to the discard pile.
	 * For inverted suits, this will return a PlayAction.
	 */
	def dragPlay(state: State, playerIndex: Int, order: Int, id: Option[Identity] = None) =
		id.orElse(state.deck(order).id()) match
			case Some(i) =>
				if state.isInverted(i) then
					DiscardAction(playerIndex, order, Some(i))
				else if state.isPlayable(i) then
					PlayAction(playerIndex, order, Some(i))
				else
					DiscardAction(playerIndex, order, Some(i), failed = true)
			case _ =>
				PlayAction(playerIndex, order, None)

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
	/** @param order The order of the card to play. */
	case Play(order: Int)
	/** @param order The order of the card to discard. */
	case Discard(order: Int)
	/** @param receiver The index of the player to clue.
	  * @param value The index of the colour to clue (leftmost is 0, skipping colourless suits).
	  */
	case Colour(receiver: Int, value: Int)
	/** @param receiver The index of the player to clue.
	  * @param value The rank to clue.
	  */
	case Rank(receiver: Int, value: Int)
	/** @param target ??
	  * @param value ??
	  */
	case Terminate(target: Int, value: Int)

	def hash: Int =
		this match
			case Play(order) =>
				order
			case Discard(order) =>
				10 + order
			case Colour(receiver, value) =>
				20 + receiver + value * 100
			case Rank(receiver, value) =>
				30 + receiver + value * 100
			case Terminate(target, value) =>
				-1

	def isClue: Boolean = this match
		case _: Colour | _: Rank => true
		case _ => false

	def getOrder: Int = this match
		case Play(order) => order
		case Discard(order) => order
		case _ => throw new Error(s"tried to get order of $this!")

	/** Returns a human-readable string of the action (must be from our hand).
	  * For plays and discards, a player can be provided whose inferences will be used.
	  * Otherwise, the common perspective will be used.
	  */
	def fmt(game: Game, accordingTo: Option[Player] = None) =
		val state = game.state
		val player = accordingTo.getOrElse(game.common)

		this match
			case Play(order) =>
				val slot = state.ourHand.indexOf(order) + 1
				s"Play slot $slot, inferences ${player.strInfs(state, order)}"

			case Discard(order) =>
				val slot = state.ourHand.indexOf(order) + 1
				s"Discard slot $slot, inferences ${player.strInfs(state, order)}"

			case Colour(receiver, value) =>
				// A PerformAction can only clue the colourable suits, but logging uses the index out of all suits.
				// e.g. In Rainbow & Pink (6 Suits), a value of 5 is a pink clue, not a "rainbow" clue.
				val adjustedValue = state.variant.suits.indexOf(state.variant.colourableSuits(value))
				Clue(ClueKind.Colour, adjustedValue, receiver).fmt(state)

			case Rank(receiver, value) =>
				Clue(ClueKind.Rank, value, receiver).fmt(state)

			case Terminate(target, value) =>
				s"Game ended: $target $value"

	/** Returns a human-readable string of the action performed by the player whose index is provided.
	  * Unlike [[PerformAction.fmt]], this can be from any player, not only us.
	  */
	def fmtObj(game: Game, playerIndex: Int) =
		val (state, deckIds) = (game.state, game.deckIds)

		val actionType = this match
			case Play(order) =>
				val id = deckIds(order)
				s"${if id.exists(!state.isPlayable(_)) then "bomb" else "play"} ${state.logId(id)}, order $order"

			case Discard(order) =>
				s"discard ${state.logId(deckIds(order))}, order $order"

			case _ => fmt(game)
		s"$actionType (${state.names(playerIndex)})"

	def json(tableID: Int) =
		this match
			case Play(order) =>
				ujson.Obj("tableID" -> tableID, "type" -> "play", "order" -> order)
			case Discard(order) =>
				ujson.Obj("tableID" -> tableID, "type" -> "discard", "order" -> order)
			case Colour(receiver, value) =>
				val clue = ujson.Obj("type" -> "color", "color" -> value)
				ujson.Obj("tableID" -> tableID, "type" -> "clue", "receiverIndex" -> receiver, "clue" -> clue)
			case Rank(receiver, value) =>
				val clue = ujson.Obj("type" -> "rank", "rank" -> value)
				ujson.Obj("tableID" -> tableID, "type" -> "clue", "receiverIndex" -> receiver, "clue" -> clue)
			case Terminate(target, value) =>
				ujson.Obj("tableID" -> tableID, "type" -> "gameOver", "endCondition" -> "terminatedByPlayer", "playerIndex" -> target)

	def toAction(state: State, playerIndex: Int, deck: Option[IndexedSeq[Identity]] = None, retain: Boolean = false) =
		val deckId = (order: Int) =>
			state.deck.lift(order).map(_.id()).flatten.orElse(deck.flatMap(d => d(order).id()))

		val clueTouched = (orders: Seq[Int], clue: BaseClue) =>
			orders.filter(deckId(_).map(state.variant.cardTouched(_, clue)).getOrElse(false))

		this match
			case PerformAction.Play(order) =>
				deckId(order) match
					case Some(id) =>
						if !retain && state.isInverted(id) then
							DiscardAction(playerIndex, order, Some(id))
						else if state.isPlayable(id) then
							PlayAction(playerIndex, order, Some(id))
						else
							DiscardAction(playerIndex, order, Some(id), true)
					case None =>
						DiscardAction(playerIndex, order, None, true)

			case PerformAction.Discard(order) =>
				deckId(order) match
					case Some(id) =>
						if !retain && state.isInverted(id) then
							if state.isPlayable(id) then
								PlayAction(playerIndex, order, Some(id))
							else
								DiscardAction(playerIndex, order, Some(id), true)
						else
							DiscardAction(playerIndex, order, Some(id), false)
					case None =>
						DiscardAction(playerIndex, order, None, false)

			case PerformAction.Colour(receiver, value) =>
				val clue = BaseClue(ClueKind.Colour, value)
				val orders = clueTouched(state.hands(receiver), clue)
				ClueAction(playerIndex, receiver, orders, clue)

			case PerformAction.Rank(receiver, value) =>
				val clue = BaseClue(ClueKind.Rank, value)
				val orders = clueTouched(state.hands(receiver), clue)
				ClueAction(playerIndex, receiver, orders, clue)

			case PerformAction.Terminate(target, value) =>
				GameOverAction()

object PerformAction:
	def fromJSON(json: ujson.Value): PerformAction =
		val actionType = json("type").str

		actionType match
			case "play" =>
				val order = json("order").num.toInt
				PerformAction.Play(order)
			case "discard" =>
				val order = json("order").num.toInt
				PerformAction.Discard(order)
			case "clue" =>
				val receiver = json("receiverIndex").num.toInt
				val clue = BaseClue.fromJSON(json("clue")).toClue(receiver)
				PerformAction.fromClue(clue)
			case "gameOver" =>
				PerformAction.Terminate(0, 0)
			case x =>
				throw new IllegalArgumentException(s"Unknown PerformAction type: $x")

	def fromAction(action: Action): PerformAction =
		action match
			case ClueAction(_, receiver, _, clue) =>
				if clue.kind == ClueKind.Colour then
					PerformAction.Colour(receiver, clue.value)
				else
					PerformAction.Rank(receiver, clue.value)

			case DiscardAction(_, order, _, failed) =>
				if failed then
					PerformAction.Play(order)
				else
					PerformAction.Discard(order)

			case PlayAction(_, order, _) =>
				PerformAction.Play(order)

			case _ =>
				throw new Error(s"can't convert non-performed action $action to a PerformAction!")

	def fromClue(clue: Clue): PerformAction =
		val Clue(kind, value, receiver) = clue
		kind match
			case ClueKind.Colour => PerformAction.Colour(receiver, value)
			case ClueKind.Rank   => PerformAction.Rank(receiver, value)

	def tryPlay(game: Game, order: Int) =
		if game.meta(order).status == CardStatus.CalledToDiscard || (game.assumeInverted(game.me, order) && game.me.thoughts(order).possibilities.difference(game.state.playableSet).isEmpty) then
			PerformAction.Discard(order)
		else
			PerformAction.Play(order)

	def tryDiscard(game: Game, order: Int) =
		if game.assumeInverted(game.me, order) && game.meta(order).status != CardStatus.CalledToDiscard then
			PerformAction.Play(order)
		else
			PerformAction.Discard(order)
