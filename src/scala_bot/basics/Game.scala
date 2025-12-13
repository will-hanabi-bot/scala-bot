package scala_bot.basics

import scala_bot.utils._
import scala_bot.logger._

import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex

val HAND_SIZE = Vector(0, 0, 5, 5, 4, 4, 3)

case class Note(turn: Int, last: String, full: String)

sealed trait Interp

enum ClueInterp extends Interp:
	case Mistake, Reactive, Play, Save, Discard, Lock, Reveal, Fix, Stall, Distribution

enum PlayInterp extends Interp:
	case None, Mistake, OrderCM

enum DiscardInterp extends Interp:
	case None, Mistake, Sarcastic, GentlemansDiscard

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
	lastMove: Option[Option[Interp]] = None,
	queuedCmds: Option[List[(String, String)]] = None,
	nextInterp: Option[Option[Interp]] = None,
	rewindDepth: Option[Int] = None,
	inProgress: Option[Boolean] = None
)

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
	def common: Player
	def base: (State, Vector[ConvData], Vector[Player], Player)
	def meta: Vector[ConvData]

	def deckIds: Vector[Option[Identity]]
	def future: Vector[IdentitySet]
	def catchup: Boolean
	def notes: Map[Int, Note]
	def lastMove: Option[Interp]
	def queuedCmds: List[(String, String)]
	def nextInterp: Option[Interp]
	def noRecurse: Boolean
	def rewindDepth: Int
	def inProgress: Boolean

	def goodTouch: Boolean

	def filterPlayables(_player: Player, _playerIndex: Int, orders: Seq[Int]) =
		orders

trait GameOps[G <: Game]:
	def blank(game: G, keepDeck: Boolean): G
	def copyWith(game: G, updates: GameUpdates): G

	def interpretClue(prev: G, game: G, action: ClueAction): G
	def interpretDiscard(prev: G, game: G, action: DiscardAction): G
	def interpretPlay(prev: G, game: G, action: PlayAction): G
	def takeAction(game: G): PerformAction
	def updateTurn(game: G, action: TurnAction): G

	def findAllClues(game: G, giver: Int): Seq[PerformAction]
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

	def withId(order: Int, id: Identity)(using ops: GameOps[G]): G =
		withCard(order)(c => c.copy(suitIndex = id.suitIndex, rank = id.rank))
			.pipe(g => ops.copyWith(g, GameUpdates(deckIds = Some(g.deckIds.updated(order, Some(id))))))

	def withCatchup(f: G => G)(using ops: GameOps[G]) =
		ops.copyWith(game, GameUpdates(catchup = Some(true)))
			.pipe(f)
			.pipe(ops.copyWith(_, GameUpdates(catchup = Some(false))))

	def handleAction(action: Action)(using ops: GameOps[G]): G =
		val state = game.state

		if (state.actionList.length < state.turnCount)
			throw new IllegalStateException(s"Turn count ${state.turnCount}, actionList ${state.actionList}")

		val newGame = withState(_.copy(actionList = addAction(state.actionList, action, state.turnCount)))

		action match {
			case clue: ClueAction =>
				Log.highlight(Console.YELLOW, s"Turn ${state.turnCount}: ${action.fmt(state)}")
				newGame.handleClue(game, clue)

			case discard: DiscardAction =>
				Log.highlight(Console.YELLOW, s"Turn ${state.turnCount}: ${action.fmt(state)}")
				ops.interpretDiscard(game, newGame.onDiscard(discard), discard)

			case play: PlayAction =>
				Log.highlight(Console.YELLOW, s"Turn ${state.turnCount}: ${action.fmt(state)}")
				ops.interpretPlay(game, newGame.onPlay(play), play)

			case draw @ DrawAction(playerIndex, order, suitIndex, rank) =>
				newGame.onDraw(draw)
					.when(g => g.state.turnCount == 0 && g.state.hands.forall(_.length == HAND_SIZE(state.numPlayers)))
						(_.withState(_.copy(turnCount = 1)))

			case _: GameOverAction =>
				Log.highlight(Console.YELLOW, "Game over!")
				ops.copyWith(newGame, GameUpdates(inProgress = Some(false)))

			case turn @ TurnAction(num, currentPlayerIndex) =>
				if (currentPlayerIndex == -1)	// game ended
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
		}

	inline def me = game.players(game.state.ourPlayerIndex)

	def isTouched(order: Int) =
		game.state.deck(order).clued ||
		game.meta(order).status == CardStatus.CalledToPlay ||
		game.meta(order).status == CardStatus.Finessed

	def isBlindPlaying(order: Int) =
		!game.state.deck(order).clued && (
			game.meta(order).status == CardStatus.CalledToPlay ||
			game.meta(order).status == CardStatus.Finessed
		)

	/** Tries all ways to see if the order matches. */
	def orderMatches(order: Int, id: Identity, infer: Boolean = false) =
		game.state.deck(order).id()
		.orElse(game.deckIds(order))
		.orElse(game.me.thoughts(order).id(infer = infer))
		.contains(id)

	/** Returns whether the order is known to be a particular special suit from empathy. */
	def knownAs(order: Int, regex: Regex) =
		game.common.thoughts(order).possible.forall { i =>
			regex.matches(game.state.variant.suits(i.suitIndex))
		}

	def handleClue(prev: G, clue: ClueAction)(using ops: GameOps[G]) =
		ops.interpretClue(prev, game.onClue(clue).elim(goodTouch = true), clue)
			.elim(goodTouch = true)

	def takeAction(using ops: GameOps[G]) =
		ops.takeAction(game)

	def simulateClue(action: ClueAction, free: Boolean = false, log: Boolean = false)(using ops: GameOps[G]) =
		val level = Logger.level

		if (!log)
			Logger.setLevel(LogLevel.Off)

		// Log.info(s"----- SIMULATING CLUE ${action.fmt(game.state)} -----")

		val hypoGame = ops.copyWith(game, GameUpdates(catchup = Some(true)))
			.withState(s => s.copy(
				actionList = addAction(s.actionList, action, s.turnCount),
				clueTokens = s.clueTokens + (if (free) 1 else 0)
			))
			.handleClue(game, action)
			.pipe(ops.copyWith(_, GameUpdates(catchup = Some(false))))
			.withState(s => s.copy(turnCount = s.turnCount + 1))

		// Log.info(s"----- END SIMULATING CLUE: ${hypoGame.lastMove} -----")

		Logger.setLevel(level)
		hypoGame

	def simulateAction(action: Action, draw: Option[Identity] = None, log: Boolean = false)(using ops: GameOps[G]): G =
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

					val action = g2.deckIds.lift(order).flatten.orElse(draw) match {
						case Some(id) => DrawAction(playerIndex, order, id.suitIndex, id.rank)
						case None => DrawAction(playerIndex, order, -1, -1)
					}

					g2.handleAction(action)
				}
			}
		)

		Logger.setLevel(level)
		hypoGame

	def rewind(turn: Int, action: Action)(using ops: GameOps[G]): Either[String, G] =
		val state = game.state

		if (turn < 1 || turn > state.actionList.length + 1)
			return Left(s"attempted to rewind to invalid turn $turn")

		Log.info(s"Rewinding to insert $action on turn $turn!")

		if (state.actionList(turn).contains(action))
			return Left("action was already rewinded")

		if (game.rewindDepth > 4)
			return Left("rewind depth went too deep")

		Log.highlight(Console.GREEN, "------- STARTING REWIND -------")

		val level = Logger.level
		Logger.setLevel(LogLevel.Off)

		val newGame = ops.blank(game, keepDeck = true)
			.pipe { g =>
				ops.copyWith(g, GameUpdates(
					catchup = Some(true),
					rewindDepth = Some(game.rewindDepth + 1),
				))
			}
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

		Right(ops.copyWith(newGame, GameUpdates(
			catchup = Some(game.catchup),
			notes = Some(game.notes),
			state = Some(newGame.state.copy(
				deck = newGame.state.deck.map { c =>
					if (c.id().nonEmpty) c else
						game.deckIds(c.order).fold(c) { id =>
							c.copy(suitIndex = id.suitIndex, rank = id.rank)
						}
				}
			))
		)))

	def replay(turn: Int)(using ops: GameOps[G]): Either[String, G] =
		val state = game.state

		if (game.rewindDepth > 4)
			return Left("rewind depth went too deep")

		Log.highlight(Console.GREEN, "------- STARTING REPLAY -------")

		// val level = Logger.level
		// Logger.setLevel(LogLevel.Off)

		val newGame = ops.blank(game, keepDeck = true)
			.pipe { g =>
				ops.copyWith(g, GameUpdates(
					catchup = Some(true),
					rewindDepth = Some(game.rewindDepth + 1),
				))
			}
			.pipe {
				state.actionList.flatten.foldLeft(_){ (acc, action) => action match {
					case DrawAction(playerIndex, order, _, _)
						if acc.state.hands(playerIndex).contains(order) => acc
					case _ =>
						acc.handleAction(action)
				}}
			}

		Log.highlight(Console.GREEN, s"------- REPLAY COMPLETE -------")

		Right(ops.copyWith(newGame, GameUpdates(
			catchup = Some(game.catchup),
			notes = Some(game.notes),
			state = Some(newGame.state.copy(
				deck = newGame.state.deck.map { c =>
					if (c.id().nonEmpty) c else
						game.deckIds(c.order).fold(c) { id =>
							c.copy(suitIndex = id.suitIndex, rank = id.rank)
						}
				}
			))
		)))

	def navigate(turn: Int)(using ops: GameOps[G]) =
		Log.highlight(Console.GREEN, s"------- NAVIGATING (turn $turn) -------")

		val actions = game.state.actionList

		ops.blank(game, keepDeck = false)
			.cond(_ => turn == 1 && game.state.ourPlayerIndex == 0)
				(actions.flatten.takeWhile(_.isInstanceOf[DrawAction]).foldLeft(_)(_.handleAction(_)))
				{
					val level = Logger.level
					Logger.setLevel(LogLevel.Off)

					def loop(g: G, actions: Iterator[Action]): G =
						if (g.state.turnCount == turn - 1)
							Logger.setLevel(level)

						if (g.state.turnCount == turn || actions.isEmpty)
							g
						else
							val action = actions.next()
							if (action.isInstanceOf[InterpAction])
								loop(g, actions)
							else
								loop(g.handleAction(action), actions)

					loop(_, actions.flatten.iterator)
				}
			.pipe(ops.copyWith(_, GameUpdates(catchup = Some(game.catchup))))
			.when(g => !g.catchup && g.state.currentPlayerIndex == g.state.ourPlayerIndex) { g =>
				val perform = g.takeAction
				Log.highlight(Console.BLUE, s"Suggested action: ${perform.fmt(g, accordingTo = Some(g.me))}")
				g
			}
			.withState(_.copy(actionList = actions))

	def getNote(order: Int): String =
		if (game.meta(order).trash)
			return "kt"

		val thought = game.common.thoughts(order)

		val note = if (thought.inferred.isEmpty)
			"??"
		else if (thought.inferred == thought.possible)
			""
		else if (thought.inferred.length <= 6)
			game.common.strInfs(game.state, order)
		else
			"..."

		game.meta(order).status match {
			case CardStatus.CalledToPlay => s"[f]${if (note.isEmpty) "" else s" [$note]"}"
			case CardStatus.Finessed => s"[f]${if (note.isEmpty) "" else s" [$note]"}"
			case CardStatus.ChopMoved => s"[cm]${if (note.isEmpty) "" else s" [$note]"}"
			case CardStatus.CalledToDiscard => "dc"
			case _ => note
		}

	def updateNotes()(using ops: GameOps[G]): G =
		val (state, notes) = (game.state, game.notes)
		val (cmds, newNotes) = game.state.hands.flatten.foldLeft((List[(String, String)](), notes)) { (acc, order) =>
			val (cmds, newNotes) = acc
			val card = state.deck(order)
			lazy val note = getNote(order)

			if ((!card.clued && game.meta(order).status == CardStatus.None) || note.isEmpty)
				acc
			else
				val linkNote = game.common.links.foldLeft(List()) { (a, link) =>
					if (link.getOrders.contains(order) && link.promise.isDefined)
						a :+ state.logId(link.promise.get)
					else a
				}.mkString("? ")

				val finalNote = if (!linkNote.isEmpty)
					if (note.contains("]")) s"$note?" else s"[$note] $linkNote?"
				else
					note

				val write = notes.get(order).forall(prev => finalNote != prev.last && state.turnCount > prev.turn)

				if (write)
					val full = notes.get(order).map(n => s"${n.full} | ").getOrElse("").appendedAll(s"t${state.turnCount}: $finalNote")

					val newCmds = if (!game.catchup && game.inProgress)
						("note", ujson.Obj("tableID" -> game.tableID, "order" -> order, "note" -> full).toString) +: cmds
					else
						cmds

					(newCmds, newNotes + (order -> Note(state.turnCount, finalNote, full)))
				else
					acc
		}

		ops.copyWith(game, GameUpdates(notes = Some(newNotes), queuedCmds = Some(game.queuedCmds ++ cmds)))
