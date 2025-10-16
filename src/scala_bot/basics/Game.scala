package scala_bot.basics

import scala_bot.utils._
import scala_bot.logger._

import scala.util.chaining.scalaUtilChainingOps

val HAND_SIZE = Vector(0, 0, 5, 5, 4, 4, 3)

case class Note(turn: Int, last: String, full: String)

sealed trait Interp

enum ClueInterp extends Interp:
	case Mistake, Reactive, RefPlay, RefDiscard, Lock, Reveal, Fix, Stall

enum PlayInterp extends Interp:
	case None

enum DiscardInterp extends Interp:
	case None, Mistake, Sarcastic, GentlemansDiscard

trait Convention:
	def interpretClue(prev: Game, game: Game, action: ClueAction): Game
	def interpretDiscard(prev: Game, game: Game, action: DiscardAction): Game
	def interpretPlay(prev: Game, game: Game, action: PlayAction): Game
	def takeAction(game: Game): PerformAction
	def updateTurn(prev: Game, game: Game, action: TurnAction): Game

	def findAllClues(game: Game, playerIndex: Int): List[PerformAction]
	def findAllDiscards(game: Game, playerIndex: Int): List[PerformAction]

case class Game(
	tableID: Int,
	state: State,
	players: Vector[Player],
	common: Player,
	base: (State, Vector[ConvData], Vector[Player], Player),
	inProgress: Boolean,
	convention: Convention,

	meta: Vector[ConvData] = Vector(),
	deckIds: Vector[Option[Identity]] = Vector(),
	catchup: Boolean = false,
	notes: Map[Int, Note] = Map(),
	lastMove: Option[Interp] = None,
	queuedCmds: List[(String, String)] = List(),
	nextInterp: Option[Interp] = None,
	noRecurse: Boolean = false,
	rewindDepth: Int = 0
):
	def blank(keepDeck: Boolean) =
		Game(tableID, base._1, convention, inProgress)
			.copy(
				deckIds = if (keepDeck) deckIds else Vector(),
				meta = base._2,
				players = base._3,
				common = base._4,
				base = base
			)

	def withCard(order: Int)(f: Card => Card): Game =
		copy(state = state.copy(deck = state.deck.updated(order, f(state.deck(order)))))

	def withThought(order: Int)(f: Thought => Thought): Game =
		copy(common = common.copy(thoughts = common.thoughts.updated(order, f(common.thoughts(order)))))

	def withMeta(order: Int)(f: ConvData => ConvData): Game =
		copy(meta = meta.updated(order, f(meta(order))))

	def withState(f: State => State): Game = copy(state = f(state))

	def withId(order: Int, id: Identity): Game =
		withCard(order)(c => c.copy(suitIndex = id.suitIndex, rank = id.rank))
		.copy(deckIds = deckIds.updated(order, Some(id)))

	def withCatchup(f: Game => Game) =
		f(copy(catchup = true)).copy(catchup = false)

	def me = players(state.ourPlayerIndex)

	def isTouched(order: Int) =
		state.deck(order).clued || meta(order).status == CardStatus.CalledToPlay

	def isBlindPlaying(order: Int) =
		!state.deck(order).clued && meta(order).status == CardStatus.CalledToPlay

	def handleAction(action: Action) =
		if (state.actionList.length < state.turnCount)
			println(state.actionList)
			throw new IllegalStateException(s"Turn count ${state.turnCount}, actionList length ${state.actionList.length}")
		val newGame = withState(_.copy(
			actionList = addAction(state.actionList, action, state.turnCount)
		))

		action match {
			case clue @ ClueAction(_, _, _, _) =>
				Log.highlight(Console.YELLOW, s"Turn ${state.turnCount}: ${action.fmt(state)}")
				newGame.handleClue(this, clue)

			case discard @ DiscardAction(_, _, _, _, _) =>
				Log.highlight(Console.YELLOW, s"Turn ${state.turnCount}: ${action.fmt(state)}")
				convention.interpretDiscard(this, newGame.onDiscard(discard), discard)

			case play @ PlayAction(_, _, _, _) =>
				Log.highlight(Console.YELLOW, s"Turn ${state.turnCount}: ${action.fmt(state)}")
				convention.interpretPlay(this, newGame.onPlay(play), play)

			case draw @ DrawAction(playerIndex, order, suitIndex, rank) =>
				newGame.onDraw(draw)
					.when(g => g.state.turnCount == 0 && g.state.hands.forall(_.length == HAND_SIZE(state.numPlayers)))
						(_.withState(_.copy(turnCount = 1)))

			case GameOverAction(_, _) =>
				Log.highlight(Console.YELLOW, "Game over!")
				copy(inProgress = false)

			case turn @ TurnAction(num, currentPlayerIndex) =>
				val newGame2 = newGame.withState(_.copy(
					currentPlayerIndex = currentPlayerIndex,
					turnCount = num + 1
				))
				convention.updateTurn(this, newGame2, turn).updateNotes()

			case InterpAction(interp) =>
				newGame.copy(nextInterp = Some(interp))

			case _ =>
				this
		}

	def handleClue(prev: Game, clue: ClueAction) =
		val newGame = this.onClue(clue).elim(goodTouch = true)
		convention.interpretClue(prev, newGame, clue)

	def takeAction =
		convention.takeAction(this)

	def simulateClue(action: ClueAction, free: Boolean = false, log: Boolean = false, noRecurse: Boolean = false) =
		val level = Logger.level

		if (!log)
			Logger.setLevel(LogLevel.Off)

		val hypoGame = copy(catchup = true, noRecurse = noRecurse)
			.withState(s => s.copy(
				actionList = addAction(s.actionList, action, s.turnCount),
				clueTokens = s.clueTokens + (if (free) 1 else 0)
			))
			.handleClue(this, action)
			.copy(catchup = false)
			.withState(s => s.copy(turnCount = state.turnCount + 1))

		Logger.setLevel(level)
		hypoGame

	def simulateAction(action: Action, draw: Option[Identity] = None, log: Boolean = false) =
		val level = Logger.level

		if (!log)
			Logger.setLevel(LogLevel.Off)

		val playerIndex = action.playerIndex

		val hypoGame = withCatchup(
			_.handleAction(action)
			.when(_ => action.requiresDraw) { g =>
				g.handleAction(TurnAction(g.state.turnCount, playerIndex))
				.when(_.state.cardsLeft > 0) { g2 =>
					val order = g2.state.nextCardOrder

					val action = deckIds.lift(order).flatten.orElse(draw) match {
						case Some(id) => DrawAction(playerIndex, order, id.suitIndex, id.rank)
						case None => DrawAction(playerIndex, order, -1, -1)
					}

					g2.handleAction(action)
				}
			}
		)

		Logger.setLevel(level)
		hypoGame

	def rewind(turn: Int, action: Action): Either[String, Game] =
		if (turn < 1 || turn > state.actionList.length + 1)
			return Left(s"attempted to rewind to invalid turn $turn")

		Log.info(s"Rewinding to insert $action on turn $turn!")

		if (state.actionList(turn).contains(action))
			return Left("action was already rewinded")

		if (rewindDepth > 2)
			return Left("rewind depth went too deep")

		Log.highlight(Console.GREEN, "------- STARTING REWIND -------")

		val level = Logger.level
		Logger.setLevel(LogLevel.Off)

		val newGame = blank(true)
			.copy(
				catchup = true,
				rewindDepth = rewindDepth + 1,
			)
			.pipe {
				state.actionList.take(turn).flatten.foldLeft(_){ (acc, action) => action match {
					case DrawAction(playerIndex, order, _, _)
						if acc.state.hands(playerIndex).contains(order) => acc
					case _ =>
						acc.handleAction(action)
				}}
			}
			.pipe { g =>
				Logger.setLevel(level)
				g.handleAction(action)
			}
			.pipe {
				state.actionList.drop(turn).flatten.foldLeft(_)(_.handleAction(_))
			}

		Log.highlight(Console.GREEN, s"------- REWIND COMPLETE -------")

		Right(newGame.copy(
			catchup = catchup,
			notes = notes,
			state = newGame.state.copy(
				deck = newGame.state.deck.map { c =>
					if (c.id().isEmpty)
						deckIds(c.order).map { id =>
							c.copy(suitIndex = id.suitIndex, rank = id.rank)
						}.getOrElse(c)
					else
						c
				}
			)
		))

	def navigate(turn: Int) =
		Log.highlight(Console.GREEN, s"------- NAVIGATING (turn $turn) -------")

		var newGame = blank(false)
		val actions = state.actionList

		newGame = if (turn == 1 && state.ourPlayerIndex == 0)
			actions.flatten.takeWhile {
				case DrawAction(_, _, _, _) => true
				case _ => false
			}.foldLeft(newGame)(_.handleAction(_))
		else
			val level = Logger.level
			Logger.setLevel(LogLevel.Off)

			def loop(game: Game, actions: Iterator[Action]): Game =
				if (game.state.turnCount == turn - 1)
					Logger.setLevel(level)

				if (game.state.turnCount == turn || actions.isEmpty)
					game
				else
					val action = actions.next()
					if (action.isInstanceOf[InterpAction])
						loop(game, actions)
					else
						loop(game.handleAction(action), actions)

			loop(newGame, actions.flatten.iterator)
		.copy(catchup = catchup)

		if (!newGame.catchup && newGame.state.currentPlayerIndex == state.ourPlayerIndex)
			val perform = newGame.takeAction
			Log.highlight(Console.BLUE, s"Suggested action: ${perform.fmt(newGame)}")

		newGame.withState(_.copy(actionList = actions))

	def getNote(order: Int): String =
		if (meta(order).trash)
			return "kt"

		val thought = common.thoughts(order)

		val note = if (thought.inferred.isEmpty)
			"??"
		else if (thought.inferred == thought.possible)
			""
		else if (thought.inferred.length <= 6)
			common.strInfs(state, order)
		else
			"..."

		meta(order).status match {
			case CardStatus.CalledToPlay => s"[f]${if (note.isEmpty) "" else s" [$note]"}"
			case CardStatus.ChopMoved => s"[cm]${if (note.isEmpty) "" else s" [$note]"}"
			case CardStatus.CalledToDiscard => "dc"
			case _ => note
		}

	def updateNotes(): Game =
		val (cmds, newNotes) = state.hands.flatten.foldLeft((List[(String, String)](), notes)) { (acc, order) =>
			val (cmds, newNotes) = acc
			val card = state.deck(order)
			lazy val note = getNote(order)

			if ((!card.clued && meta(order).status == CardStatus.None) || note.isEmpty)
				acc
			else
				val linkNote = common.links.foldLeft(List()) { (a, link) =>
					if (link.getOrders.contains(order) && link.promise.isDefined)
						a :+ link.promise.get
					else a
				}.mkString("? ")

				val finalNote = if (!linkNote.isEmpty)
					if (note.contains("]")) s"$note?" else s"[$note] $linkNote?"
				else
					note

				val write = notes.get(order).forall(prev => note != prev.last && state.turnCount > prev.turn)

				if (write)
					val full = notes.get(order).map(n => s"${n.full} | ").getOrElse("").appendedAll(s"t${state.turnCount}: $finalNote")

					val newCmds = if (!catchup && inProgress)
						("note", ujson.Obj("tableID" -> tableID, "order" -> order, "note" -> full).toString) +: cmds
					else
						cmds

					(newCmds, newNotes + (order -> Note(state.turnCount, finalNote, full)))
				else
					acc
		}

		copy(notes = newNotes, queuedCmds = queuedCmds ++ cmds)

object Game {
	private def genPlayers(state: State) =
		val numPlayers = state.numPlayers
		val allPossible = IdentitySet.from(state.variant.allIds)
		val hypoStacks = Vector.fill(state.variant.suits.length)(0)

		val players = (0 until numPlayers).map(i => Player(i, state.names(i), allPossible, hypoStacks)).toVector
		val common = Player(-1, "common", allPossible, hypoStacks)
		(players, common)

	private def init(
		tableID: Int,
		state: State,
		inProgress: Boolean,
		convention: Convention,
		t: (players: Vector[Player], common: Player)
	): Game =
		Game(
			tableID = tableID,
			state = state,
			players = t.players,
			common = t.common,
			base = (state, Vector(), t.players, t.common),
			inProgress = inProgress,
			convention = convention,
		)

	def apply(
		tableID: Int,
		state: State,
		convention: Convention,
		inProgress: Boolean
	) =
		init(tableID, state, inProgress, convention, Game.genPlayers(state))
}
