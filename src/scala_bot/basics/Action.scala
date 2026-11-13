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
	def isPlayerAction = false

object TurnAction:
	def fromJSON(json: ujson.Value) =
		TurnAction(
			json.obj("turn").num.toInt,
			json.obj.get("currentPlayerIndex").fold(-1)(_.num.toInt)
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
	def isPlayerAction = true

object ClueAction:
	def fromJSON(json: ujson.Value) =
		ClueAction(
			json.obj("playerIndex").num.toInt,
			json.obj("receiverIndex").num.toInt,
			json.obj("orders").arr.toSeq.map(_.num.toInt),
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
	def isPlayerAction = false

object DrawAction:
	def fromJSON(json: ujson.Value) =
		val identity = json.obj.get("identity")
		DrawAction(
			json.obj("playerIndex").num.toInt,
			json.obj("order").num.toInt,
			identity.fold(-1)(_("suitIndex").num.toInt),
			identity.fold(-1)(_("rank").num.toInt)
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
	def isPlayerAction = true

object PlayAction:
	def fromJSON(json: ujson.Value) =
		val identity = json.obj.get("identity")
		PlayAction(
			json.obj("playerIndex").num.toInt,
			json.obj("order").num.toInt,
			identity.fold(-1)(_("suitIndex").num.toInt),
			identity.fold(-1)(_("rank").num.toInt)
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
	def isPlayerAction = true

object DiscardAction:
	def fromJSON(json: ujson.Value, failed: Boolean = false) =
		val identity = json.obj.get("identity")
		DiscardAction(
			json.obj("playerIndex").num.toInt,
			json.obj("order").num.toInt,
			identity.fold(-1)(_("suitIndex").num.toInt),
			identity.fold(-1)(_("rank").num.toInt),
			failed
		)

	def apply(playerIndex: Int, order: Int, id: Option[Identity]): DiscardAction =
		id match
			case Some(Identity(suitIndex, rank)) =>
				DiscardAction(playerIndex, order, suitIndex, rank)
			case None =>
				DiscardAction(playerIndex, order, -1, -1)

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
			case "clue"     => Some(ClueAction.fromJSON(json))
			case "discard"  => Some(DiscardAction.fromJSON(json))
			case "misplay"  => Some(DiscardAction.fromJSON(json, failed = true))
			case "play"     => Some(PlayAction.fromJSON(json))
			case "draw"     => Some(DrawAction.fromJSON(json))
			case "turn"     => Some(TurnAction.fromJSON(json))
			case "gameOver" => Some(GameOverAction())
			case _          => None

	def fromClue(state: State, clue: Clue, giver: Int): ClueAction =
		val list = state.clueTouched(state.hands(clue.target), clue)
		ClueAction(giver, clue.target, list, clue.base)

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
			case Some(id) =>
				if state.isInverted(id) then
					if state.isPlayable(id) then
						PlayAction(playerIndex, order, id.suitIndex, id.rank)
					else
						DiscardAction(playerIndex, order, id.suitIndex, id.rank, failed = true)
				else
					DiscardAction(playerIndex, order, id.suitIndex, id.rank)
			case _ =>
				DiscardAction(playerIndex, order, -1, -1)

	/**
	 * Returns the result of dragging this card to the discard pile.
	 * For inverted suits, this will return a PlayAction.
	 */
	def dragPlay(state: State, playerIndex: Int, order: Int, id: Option[Identity] = None) =
		id.orElse(state.deck(order).id()) match
			case Some(id) =>
				if state.isInverted(id) then
					DiscardAction(playerIndex, order, id.suitIndex, id.rank)
				else if state.isPlayable(id) then
					PlayAction(playerIndex, order, id.suitIndex, id.rank)
				else
					DiscardAction(playerIndex, order, id.suitIndex, id.rank, failed = true)
			case _ =>
				PlayAction(playerIndex, order, -1, -1)

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
				// A PerformAction can only clue the colourable suits, but logging uses the index out of all suits.
				// e.g. In Rainbow & Pink (6 Suits), a value of 5 is a pink clue, not a "rainbow" clue.
				val adjustedValue = state.variant.suits.indexOf(state.variant.colourableSuits(value))
				Clue(ClueKind.Colour, adjustedValue, target).fmt(state)

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

	def actionJson =
		this match
			case Play(target) =>
				ujson.Obj("type" -> "play", "order" -> target)
			case Discard(target) =>
				ujson.Obj("type" -> "discard", "order" -> target)
			case Colour(target, value) =>
				ujson.Obj("type" -> "clue", "receiverIndex" -> target, "clue" -> ujson.Obj("type" -> "color", "color" -> value))
			case Rank(target, value) =>
				ujson.Obj("type" -> "clue", "receiverIndex" -> target, "clue" -> ujson.Obj("type" -> "rank", "rank" -> value))
			case Terminate(target, _) =>
				ujson.Obj("type" -> "gameOver", "endCondition" -> "terminatedByPlayer", "playerIndex" -> target)

	def json(tableId: Int) =
		ujson.Obj("tableId" -> tableId, "action" -> actionJson)

	def toAction(state: State, playerIndex: Int, deck: Option[IndexedSeq[Identity]] = None, retain: Boolean = false) =
		val deckId = (order: Int) =>
			state.deck.lift(order).map(_.id()).flatten.orElse(deck.flatMap(d => d(order).id()))

		val clueTouched = (orders: Seq[Int], clue: BaseClue) =>
			orders.filter(deckId(_).map(state.variant.cardTouched(_, clue)).getOrElse(false))

		this match
			case PerformAction.Play(target) =>
				deckId(target) match
					case Some(id) =>
						if !retain && state.isInverted(id) then
							DiscardAction(playerIndex, target, id.suitIndex, id.rank)
						else if state.isPlayable(id) then
							PlayAction(playerIndex, target, id.suitIndex, id.rank)
						else
							DiscardAction(playerIndex, target, id.suitIndex, id.rank, true)
					case None =>
						DiscardAction(playerIndex, target, -1, -1, true)

			case PerformAction.Discard(target) =>
				deckId(target) match
					case Some(id) =>
						if !retain && state.isInverted(id) then
							if state.isPlayable(id) then
								PlayAction(playerIndex, target, id.suitIndex, id.rank)
							else
								DiscardAction(playerIndex, target, id.suitIndex, id.rank, true)
						else
							DiscardAction(playerIndex, target, id.suitIndex, id.rank, false)
					case None =>
						DiscardAction(playerIndex, target, -1, -1, false)

			case PerformAction.Colour(target, value) =>
				val clue = BaseClue(ClueKind.Colour, value)
				val list = clueTouched(state.hands(target), clue)
				ClueAction(playerIndex, target, list, clue)

			case PerformAction.Rank(target, value) =>
				val clue = BaseClue(ClueKind.Rank, value)
				val list = clueTouched(state.hands(target), clue)
				ClueAction(playerIndex, target, list, clue)

			case PerformAction.Terminate(_, _) =>
				GameOverAction()

object PerformAction:
	def fromJSON(json: ujson.Value): PerformAction =
		json("type").str match
			case "play"     => PerformAction.Play(json("order").num.toInt)
			case "discard"  => PerformAction.Discard(json("order").num.toInt)
			case "clue"     => PerformAction.fromClue(BaseClue.fromJSON(json("clue")).toClue(json("receiverIndex").num.toInt))
			case "gameOver" => PerformAction.Terminate(0, 0)
			case other      => throw new IllegalArgumentException(s"Invalid PerformAction type: $other")

	def fromAction(action: Action): PerformAction =
		action match
			case ClueAction(_, target, _, clue) =>
				if clue.kind == ClueKind.Colour then
					PerformAction.Colour(target, clue.value)
				else
					PerformAction.Rank(target, clue.value)

			case DiscardAction(_, order, _, _, failed) =>
				if failed then
					PerformAction.Play(order)
				else
					PerformAction.Discard(order)

			case PlayAction(_, order, _, _) =>
				PerformAction.Play(order)

			case _ =>
				throw new Error(s"can't convert non-performed action $action to a PerformAction!")

	def fromClue(clue: Clue): PerformAction =
		val Clue(kind, value, target) = clue
		kind match
			case ClueKind.Colour => PerformAction.Colour(target, value)
			case ClueKind.Rank   => PerformAction.Rank(target, value)

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
