package scala_bot.basics

import cats.effect.IO

import scala_bot.utils._
import scala_bot.logger._
import scala.util.matching.Regex
import cats.effect.unsafe.IORuntime

val HAND_SIZE = Vector(0, 0, 5, 5, 4, 4, 3)

/** Manages the note written on a card.
  * @param turn The most recent turn the note was updated.
  * @param last The most recent specific note on the card (ignoring notes from earlier turns).
  * @param full The full note on the card (including notes from earlier turns).
  */
case class Note(
	/** The most recent turn the note was updated. */
	turn: Int,
	/** The most recent specific note on the card (ignoring notes from earlier turns). */
	last: String,
	/** The full note on the card (including notes from earlier turns). */
	full: String
)

/** An interpretation of a game action.
  * @see [[ClueInterp]], [[PlayInterp]], [[DiscardInterp]]
 */
sealed trait Interp

enum ClueInterp extends Interp:
	case Mistake, Reactive, Play, Save, Discard, Lock, Reveal, Fix, Stall, Distribution, Useless

enum PlayInterp extends Interp:
	case None, Mistake, OrderCM

enum DiscardInterp extends Interp:
	case None, Mistake, Sarcastic, GentlemansDiscard, Emergency, Positional, Sacrifice

case class GameUpdates(
	tableID: Option[Int] = None,
	state: Option[State] = None,
	players: Option[Vector[Player]] = None,
	common: Option[Player] = None,
	base: Option[(State, Vector[ConvData], Vector[Player], Player)] = None,
	meta: Option[Vector[ConvData]] = None,

	deckIds: Option[Vector[Option[Identity]]] = None,
	catchup: Option[Boolean] = None,
	notes: Option[Map[Int, Note]] = None,
	lastActions: Option[Vector[Option[Action]]] = None,
	lastMove: Option[Option[Interp]] = None,
	queuedCmds: Option[List[(String, String)]] = None,
	nextInterp: Option[Option[Interp]] = None,
	rewindDepth: Option[Int] = None,
	inProgress: Option[Boolean] = None,

	noRecurse: Option[Boolean] = None
)

/** A helper function to generate the players and common perspective, given a state. */
def genPlayers(state: State) =
	val allPossible = state.allIds
	val hypoStacks = Vector.fill(state.variant.suits.length)(0)

	val players = (0 until state.numPlayers).map(i => Player(i, state.names(i), allPossible, hypoStacks)).toVector
	val common = Player(-1, "common", allPossible, hypoStacks)
	(players, common)

trait Game:
	def tableID: Int
	def state: State
	def players: Vector[Player]
	/** The "common knowledge" perspective. Contains what all players know (that all players know...). */
	def common: Player
	/** The original state, meta-knowledge, players, and common perspective. Used when reinterpreting from the beginning of the game. */
	def base: (State, Vector[ConvData], Vector[Player], Player)
	/** The conventional knowledge on cards shared by all players. Indexed by card order. */
	def meta: Vector[ConvData]

	/** The identities of every card in the deck, if known. Potentially contains future info, unlike [[State.deck]]. */
	def deckIds: Vector[Option[Identity]]
	/** The possible ids of every card in the deck, if needed for retroactive notes (e.g. revealing a *Layered Finesse*). */
	def future: Vector[IdentitySet]
	/** Whether the game is "catching up" on game actions (i.e. if true, it shouldn't send websocket messages). */
	def catchup: Boolean
	def notes: Map[Int, Note]
	/** The last game action taken by each player, if they exist. */
	def lastActions: Vector[Option[Action]]
	/** The interpretation of the most recent move, if it exists. */
	def lastMove: Option[Interp]
	/** Commands that are queued to be sent to hanab.live (e.g. to play a specific card). */
	def queuedCmds: List[(String, String)]
	/** What the next clue should be interpreted as. Used when rewinding after a particular interpretation is demonstrated. */
	def nextInterp: Option[Interp]
	/** Whether to disallow recursing into hypothetical states, like seeing if a stall was available. */
	def noRecurse: Boolean
	def rewindDepth: Int
	/** Whether the game is live or a replay. */
	def inProgress: Boolean

	/** Whether the convention set observes Good Touch Principle. */
	def goodTouch: Boolean

	/** An extra filter function before returning from [[Player.thinksPlayables]]
	  * (e.g. 1's to be played in a conventionally-specific order).
	  */
	def filterPlayables(@annotation.unused player: Player, @annotation.unused playerIndex: Int, orders: Seq[Int], @annotation.unused assume: Boolean = true) =
		orders

	/** Returns whether assigning the given identity to the card with the given order is valid.
	  * For example, a convention with good touch may disallow assigning a trash identity to a clued card.
	  */
	def validArr(@annotation.unused id: Identity, @annotation.unused order: Int) =
		true

trait GameOps[G <: Game]:
	/** Returns a copy of the game state, but restarted from the beginning.
	  * @param game     The current game state.
	  * @param keepDeck Whether to keep the same deck (relevant when simulating endgames).
	  */
	def blank(game: G, keepDeck: Boolean): G
	def copyWith(game: G, updates: GameUpdates): G

	/** Returns the game state after interpreting the given clue.*/
	def interpretClue(prev: G, game: G, action: ClueAction): G
	/** Returns the game state after interpreting the given discard.*/
	def interpretDiscard(prev: G, game: G, action: DiscardAction): G
	/** Returns the game state after interpreting the given play.*/
	def interpretPlay(prev: G, game: G, action: PlayAction): G
	/** Returns the best action to take.*/
	def takeAction(game: G): IO[PerformAction]
	/** Called between every play, clue and discard. */
	def updateTurn(game: G, action: TurnAction): G

	/** A hook that returns the updated game state after a play in particular. */
	def refreshAfterPlay(@annotation.unused prev: G, game: G, @annotation.unused action: PlayAction): G =
		game

	/** Returns all valid clues the giver can give (used in endgame solving). */
	def findAllClues(game: G, giver: Int): Seq[PerformAction]

	/** Returns all valid discards the giver can perform (used in endgame solving). */
	def findAllDiscards(game: G, playerIndex: Int): Seq[PerformAction]

extension[G <: Game](game: G)
	def withThought(order: Int)(f: Thought => Thought)(using ops: GameOps[G]) =
		ops.copyWith(game, GameUpdates(
			common = Some(game.common.withThought(order)(f))
		))

	def withMeta(order: Int)(f: ConvData => ConvData)(using ops: GameOps[G]) =
		val meta = game.meta
		ops.copyWith(game, GameUpdates(meta = Some(meta.updated(order, f(meta(order))))))

	def withState(f: State => State)(using ops: GameOps[G]): G =
		ops.copyWith(game, GameUpdates(state = Some(f(game.state))))

	def withCard(order: Int)(f: Card => Card)(using ops: GameOps[G]): G =
		val deck = game.state.deck
		withState(_.copy(deck = deck.updated(order, f(deck(order)))))

	/** Writes the identity on the given order in [[State.deck]] and [[Game.deckIds]]. */
	def withId(order: Int, id: Identity)(using ops: GameOps[G]): G =
		withCard(order)(c => c.copy(suitIndex = id.suitIndex, rank = id.rank))
			.pipe(g => ops.copyWith(g, GameUpdates(deckIds = Some(g.deckIds.updated(order, Some(id))))))

	def withCatchup(f: G => G)(using ops: GameOps[G]) =
		ops.copyWith(game, GameUpdates(catchup = Some(true)))
			.pipe(f)
			.pipe(ops.copyWith(_, GameUpdates(catchup = Some(false))))

	/** Returns an updated copy of the game after interpreting the given action. */
	def handleAction(action: Action)(using ops: GameOps[G]): G =
		val state = game.state

		if state.actionList.length < state.turnCount then
			throw new IllegalStateException(s"Turn count ${state.turnCount}, actionList ${state.actionList.length} ${state.actionList}")

		val newGame = withState(_.copy(actionList = addAction(state.actionList, action, state.turnCount)))

		action match
			case clue: ClueAction =>
				Log.highlight(Console.YELLOW, s"Turn ${state.turnCount}: ${action.fmt(state)}")
				newGame.handleClue(game, clue).pipe: g =>
					ops.copyWith(g, GameUpdates(lastActions = Some(g.lastActions.updated(clue.playerIndex, Some(clue)))))

			case discard: DiscardAction =>
				Log.highlight(Console.YELLOW, s"Turn ${state.turnCount}: ${action.fmt(state)}")
				ops.interpretDiscard(game, newGame.onDiscard(discard), discard).pipe: g =>
					ops.copyWith(g, GameUpdates(lastActions = Some(g.lastActions.updated(discard.playerIndex, Some(discard)))))

			case play: PlayAction =>
				Log.highlight(Console.YELLOW, s"Turn ${state.turnCount}: ${action.fmt(state)}")
				ops.interpretPlay(game, newGame.onPlay(play), play).pipe: g =>
					ops.copyWith(g, GameUpdates(lastActions = Some(g.lastActions.updated(play.playerIndex, Some(play)))))

			case draw @ DrawAction(playerIndex, order, suitIndex, rank) =>
				newGame.onDraw(draw)
					.when(g => g.state.turnCount == 0 && g.state.hands.forall(_.length == HAND_SIZE(state.numPlayers)))
						(_.withState(_.copy(turnCount = 1)))

			case _: GameOverAction =>
				Log.highlight(Console.YELLOW, "Game over!")
				ops.copyWith(newGame, GameUpdates(inProgress = Some(false)))

			case turn @ TurnAction(num, currentPlayerIndex) =>
				if currentPlayerIndex == -1 then	// game ended
					newGame
				else
					newGame.withState(_.copy(
						currentPlayerIndex = currentPlayerIndex,
						turnCount = num + 1
					))
					.pipe(ops.updateTurn(_, turn).updateNotes())

			case InterpAction(interp) =>
				ops.copyWith(newGame, GameUpdates(nextInterp = Some(Some(interp))))

			case _ => game

	inline def me = game.players(game.state.ourPlayerIndex)

	/** Returns whether a card is "gotten" (clued, finessed, gd, etc.) */
	def isTouched(order: Int) =
		val status = game.meta(order).status

		game.state.deck(order).clued ||
		status == CardStatus.CalledToPlay ||
		status == CardStatus.GentlemansDiscard ||
		status == CardStatus.Finessed

	/** Returns whether a card is playing without being clued (excludes gd). */
	def isBlindPlaying(order: Int) =
		!game.state.deck(order).clued && (
			game.meta(order).status == CardStatus.CalledToPlay ||
			game.meta(order).status == CardStatus.Finessed ||
			game.meta(order).bluffed
		)

	/** Returns whether a card is "gotten" or chop moved. */
	def isSaved(order: Int) =
		isTouched(order) || game.meta(order).cm

	/** Tries all ways to see if the order matches the given identity. */
	def orderMatches(order: Int, id: Identity, infer: Boolean = false) =
		game.state.deck(order).id()
		.orElse(game.deckIds(order))
		.orElse(game.me.thoughts(order).id(infer = infer))
		.contains(id)

	/** Returns whether the order is known to be a particular special suit from empathy. */
	def knownAs(order: Int, regex: Regex) =
		game.common.thoughts(order).possible.forall: i =>
			regex.matches(game.state.variant.suits(i.suitIndex).name)

	def handleClue(prev: G, clue: ClueAction)(using ops: GameOps[G]) =
		ops.interpretClue(prev, game.onClue(clue), clue)

	def takeAction(using ops: GameOps[G]) =
		ops.takeAction(game)

	def simulateClue(action: ClueAction, free: Boolean = false, log: Boolean = false)(using ops: GameOps[G]) =
		val level = Logger.level

		if !log then
			Logger.setLevel(LogLevel.Off)

		// Log.info(s"----- SIMULATING CLUE ${action.fmt(game.state)} -----")

		val hypoGame = ops.copyWith(game, GameUpdates(catchup = Some(true)))
			.withState(s => s.copy(
				actionList = addAction(s.actionList, action, s.turnCount),
				clueTokens = s.clueTokens + (if free then 1 else 0)
			))
			.handleClue(game, action)
			.pipe(ops.copyWith(_, GameUpdates(catchup = Some(false))))
			.withState(s => s.copy(turnCount = s.turnCount + 1))

		// Log.info(s"----- END SIMULATING CLUE: ${hypoGame.lastMove} -----")

		Logger.setLevel(level)
		hypoGame

	def simulateAction(action: Action, draw: Option[Identity] = None, log: Boolean = false)(using ops: GameOps[G]): G =
		val level = Logger.level

		if !log then
			Logger.setLevel(LogLevel.Off)

		val playerIndex = action.playerIndex

		val hypoGame = withCatchup:
			_.handleAction(action)
			.when(_ => action.requiresDraw): g =>
				g.handleAction(TurnAction(g.state.turnCount, playerIndex))
				.when(_.state.cardsLeft > 0): g2 =>
					val order = g2.state.nextCardOrder

					val action = g2.deckIds.lift(order).flatten.orElse(draw) match
						case Some(id) => DrawAction(playerIndex, order, id.suitIndex, id.rank)
						case None     => DrawAction(playerIndex, order, -1, -1)

					g2.handleAction(action)

		Logger.setLevel(level)
		hypoGame

	def simulate(action: Action)(using ops: GameOps[G]): G =
		action match
			case clue: ClueAction => game.simulateAction(clue, log = true)
			case _ => game.simulateAction(action)

	def rewind(turn: Int, action: Action)(using ops: GameOps[G]): Either[String, G] =
		val state = game.state

		if turn < 1 || turn > state.actionList.length + 1 then
			return Left(s"attempted to rewind to invalid turn $turn")

		Log.info(s"Rewinding to insert $action on turn $turn!")

		if state.actionList(turn).contains(action) then
			return Left("action was already rewinded")

		if game.rewindDepth > 4 then
			return Left("rewind depth went too deep")

		Log.highlight(Console.GREEN, "------- STARTING REWIND -------")

		val level = Logger.level
		Logger.setLevel(LogLevel.Off)

		val newGame = ops.blank(game, keepDeck = true)
			.pipe:
				ops.copyWith(_, GameUpdates(
					catchup = Some(true),
					rewindDepth = Some(game.rewindDepth + 1),
				))
			.pipe:
				state.actionList.take(turn).flatten.foldLeft(_): (acc, action) =>
					action match
						case DrawAction(playerIndex, order, _, _)
							if acc.state.hands(playerIndex).contains(order) => acc
						case _ =>
							acc.handleAction(action)
			.pipe:
				Logger.setLevel(level)
				_.handleAction(action)
			.pipe:
				state.actionList.drop(turn).flatten.foldLeft(_)(_.handleAction(_))

		Log.highlight(Console.GREEN, s"------- REWIND COMPLETE -------")

		Right(ops.copyWith(newGame, GameUpdates(
			catchup = Some(game.catchup),
			notes = Some(game.notes),
			state = Some(newGame.state.copy(
				deck = newGame.state.deck.map: c =>
					if c.id().nonEmpty then c else
						game.deckIds(c.order).fold(c): id =>
							c.copy(suitIndex = id.suitIndex, rank = id.rank))),
			rewindDepth = Some(game.rewindDepth))))

	def replay(using ops: GameOps[G]): Either[String, G] =
		val state = game.state

		if game.rewindDepth > 4 then
			return Left("rewind depth went too deep")

		Log.highlight(Console.GREEN, "------- STARTING REPLAY -------")

		// val level = Logger.level
		// Logger.setLevel(LogLevel.Off)

		val newGame = ops.blank(game, keepDeck = true)
			.pipe: g =>
				ops.copyWith(g, GameUpdates(
					catchup = Some(true),
					rewindDepth = Some(game.rewindDepth + 1),
				))
			.pipe:
				state.actionList.flatten.foldLeft(_): (acc, action) =>
					action match
						case DrawAction(playerIndex, order, _, _)
							if acc.state.hands(playerIndex).contains(order) => acc
						case _ =>
							acc.handleAction(action)

		Log.highlight(Console.GREEN, s"------- REPLAY COMPLETE -------")

		Right(ops.copyWith(newGame, GameUpdates(
			catchup = Some(game.catchup),
			notes = Some(game.notes),
			state = Some(newGame.state.copy(
				deck = newGame.state.deck.map: c =>
					if c.id().nonEmpty then c else
						game.deckIds(c.order).fold(c): id =>
							c.copy(suitIndex = id.suitIndex, rank = id.rank))),
			rewindDepth = Some(game.rewindDepth))))

	def navigate(turn: Int)(using ops: GameOps[G], runtime: IORuntime) =
		Log.highlight(Console.GREEN, s"------- NAVIGATING (turn $turn) -------")

		val actions = game.state.actionList

		ops.blank(game, keepDeck = false)
			.cond(_ => turn == 1 && game.state.ourPlayerIndex == 0)
				(actions.flatten.takeWhile(_.isInstanceOf[DrawAction]).foldLeft(_)(_.handleAction(_)))
				{
					val level = Logger.level
					Logger.setLevel(LogLevel.Off)

					actions.flatten.foldLeftOpt(_): (g, action) =>
						if g.state.turnCount == turn - 1 then
							Logger.setLevel(level)

						if g.state.turnCount == turn || actions.isEmpty then
							Left(g)
						else if action.isInstanceOf[InterpAction] then
							Right(g)
						else
							Right(g.handleAction(action))
				}
			.pipe(ops.copyWith(_, GameUpdates(catchup = Some(game.catchup))))
			.when(g => !g.catchup && g.state.currentPlayerIndex == g.state.ourPlayerIndex): g =>
				Log.highlight(Console.BLUE, s"Suggested action: ${g.takeAction.unsafeRunSync().fmt(g, accordingTo = Some(g.me))}")
				g
			.withState(_.copy(actionList = actions))

	def getNote(order: Int): String =
		if game.meta(order).trash then
			return "kt"

		val thought = game.common.thoughts(order)

		val note = if thought.inferred.isEmpty then
			"??"
		else if thought.inferred == thought.possible then
			""
		else if thought.inferred.length <= 6 then
			game.common.strInfs(game.state, order)
		else
			"..."

		game.meta(order).status match
			case CardStatus.CalledToPlay => s"[f]${if note.isEmpty then "" else s" [$note]"}"
			case CardStatus.Finessed => s"[f]${if note.isEmpty then "" else s" [$note]"}"
			case CardStatus.ChopMoved => s"[cm]${if note.isEmpty then "" else s" [$note]"}"
			case CardStatus.CalledToDiscard => "dc"
			case CardStatus.PermissionToDiscard => "ptd"
			case _ => note

	def updateNotes()(using ops: GameOps[G]): G =
		val (state, notes) = (game.state, game.notes)
		val (cmds, newNotes) = game.state.hands.flatten.foldLeft((List.empty[(String, String)], notes)): (acc, order) =>
			val (cmds, newNotes) = acc
			val card = state.deck(order)
			lazy val note = getNote(order)

			if (!card.clued && game.meta(order).status == CardStatus.None) || note.isEmpty then
				acc
			else
				val linkNote = game.common.links.foldLeft(Nil) { (a, link) =>
					if link.getOrders.contains(order) && link.promise.isDefined then
						a :+ state.logId(link.promise.get)
					else a
				}.mkString("? ")

				val finalNote =
					if !linkNote.isEmpty then
						if note.contains("]") then s"$note?" else s"[$note] $linkNote?"
					else
						note

				val write = notes.get(order).forall(prev => finalNote != prev.last && state.turnCount > prev.turn)

				if write then
					val full = notes.get(order).map(n => s"${n.full} | ").getOrElse("").appendedAll(s"t${state.turnCount}: $finalNote")

					val newCmds =
						if !game.catchup && game.inProgress then
							("note", ujson.Obj("tableID" -> game.tableID, "order" -> order, "note" -> full).toString) +: cmds
						else
							cmds

					(newCmds, newNotes + (order -> Note(state.turnCount, finalNote, full)))
				else
					acc

		ops.copyWith(game, GameUpdates(notes = Some(newNotes), queuedCmds = Some(game.queuedCmds ++ cmds)))
