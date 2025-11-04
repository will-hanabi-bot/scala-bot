package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.basics.given_Conversion_IdentitySet_Iterable
// import scala_bot.endgame.EndgameSolver
import scala_bot.utils._
import scala_bot.logger.{Log, Logger, LogLevel}

import scala.util.chaining.scalaUtilChainingOps

case class FocusResult(
	focus: Int,
	chop: Boolean = false,
	positional: Boolean = false
)

enum StallInterp:
	case Stall5, Tempo, Locked, FillIn, Clues8, Burn

enum DcStatus:
	case None, Scream, Shout, Generation

object Level {
	val Fix = 3
	val Sarcastic = 3
	val BasicCM = 4
	val IntermediateFinesses = 5
	val TempoClues = 6
	val LastResorts = 7
	val Endgame = 8
	val Stalling = 9
	val SpecialDiscards = 10
	val Bluffs = 11
	val Context = 12
}

case class XConvData(
	idUncertain: Boolean = false,
	maybeFinessed: Boolean = false,
	turnFinessed: Option[Int] = None,
	finesseIds: Option[IdentitySet] = None
)

case class HGroup(
	tableID: Int,
	state: State,
	players: Vector[Player],
	common: Player,
	base: (State, Vector[ConvData], Vector[Player], Player),

	meta: Vector[ConvData] = Vector(),
	deckIds: Vector[Option[Identity]] = Vector(),
	catchup: Boolean = false,
	notes: Map[Int, Note] = Map(),
	lastMove: Option[Interp] = None,
	queuedCmds: List[(String, String)] = Nil,
	nextInterp: Option[Interp] = None,
	noRecurse: Boolean = false,
	rewindDepth: Int = 0,
	inProgress: Boolean = false,

	goodTouch: Boolean = true,
	level: Int = 1,
	waiting: List[WaitingConnection] = Nil,
	stalled5: Boolean = false,
	cluedOnChop: Set[Int] = Set(),
	dcStatus: DcStatus = DcStatus.None,
	dda: Option[Identity] = None,
	inEarlyGame: Boolean = true,
	stallInterp: Option[StallInterp] = None,
	lastActions: Vector[Option[Action]] = Vector(),
	xmeta: Vector[XConvData] = Vector()
) extends Game:
	def withXMeta(order: Int)(f: XConvData => XConvData) =
		copy(xmeta = xmeta.updated(order, f(xmeta(order))))

	def chop(playerIndex: Int) =
		state.hands(playerIndex).findLast { o =>
			!state.deck(o).clued && meta(o).status == CardStatus.None
		}

	/** Returns how far a card is from chop. A card on chop is 0-away. */
	def chopDistance(playerIndex: Int, order: Int) =
		chop(playerIndex) match {
			case None =>
				throw new IllegalArgumentException(s"${state.names(playerIndex)} has no chop!")
			case Some(c) =>
				if (order < c)
					throw new IllegalArgumentException(s"order $order is right of chop $c!")

				state.hands(playerIndex).count { o =>
					o < order && o > c &&
					!state.deck(o).clued &&
					meta(o).status == CardStatus.None
				}
		}

	def findFinesse(playerIndex: Int, connected: List[Int] = Nil, ignore: Set[Int] = Set()) =
		val order = state.hands(playerIndex).find { o =>
			(!this.isTouched(o) || this.xmeta(o).maybeFinessed) && !connected.contains(o)
		}

		order.filter(!ignore.contains(_))

	def unknown1(order: Int) =
		val clues = state.deck(order).clues

		clues.nonEmpty && clues.forall(_.eq(ClueKind.Rank, 1))

	def order1s(orders: Seq[Int], noFilter: Boolean = false) =
		val unknown1s = if (noFilter) orders else
			orders.filter { o =>
				unknown1(o) && common.thoughts(o).possible.forall(_.rank == 1)
			}

		unknown1s.sortBy { o =>
			if (state.inStartingHand(o) && meta(o).status != CardStatus.ChopMoved)
				o
			else if (cluedOnChop.contains(o))
				-100 - o
			else
				-o
		}

	def priority(orders: List[Int]) =
		val initial = (0 to 5).map(_ => Vector.empty[Int])
		orders.foldLeft(initial) { (acc, o) =>
			val thought = this.me.thoughts(o)

			val inFinesse = this.isBlindPlaying(o)	// TODO: play link?
			lazy val unknownCM = meta(o).status == CardStatus.ChopMoved &&
				!state.deck(o).clued &&
				thought.possible.exists(!state.isPlayable(_))

			def connecting(playerIndex: Int, id: Identity) =
				state.hands(playerIndex).exists(this.me.thoughts(_).matches(id.next, infer = true))

			lazy val connectsTo = (0 until state.numPlayers).filter { playerIndex =>
				thought.possibilities.exists(i => connecting(playerIndex, i))
			}

			val priorityIndex =
				if (inFinesse)
					0
				else if (unknownCM)
					// Don't blind play CM cards at 2 strikes
					if (state.strikes == 2) -1 else 5
				else if (connectsTo.exists(_ != state.ourPlayerIndex))
					1
				else if (connectsTo.nonEmpty)
					2
				else if (thought.possibilities.forall(_.rank == 5))
					3
				else if (thought.possibilities.length > 1)
					4
				else
					5

			if (priorityIndex == -1)
				acc
			else
				(acc.updated(priorityIndex, acc(priorityIndex) :+ o))
		}
		.pipe { ps =>
			// Speed-up clues first, then oldest finesse to newest
			ps.updated(0, ps(0).sortBy { o =>
				if (this.isBlindPlaying(o))
					-200 - o
				else if (state.deck(o).clued)
					-100 - o
				else if (meta(o).hidden)
					-o
				else
					o
			})
			// Lowest rank, then leftmost
			.updated(5, ps(5).sortBy { o =>
				this.me.thoughts(o).possibilities.map(_.rank).min * 100 - o
			})
		}

	def determineFocus(prev: HGroup, action: ClueAction): FocusResult =
		val ClueAction(giver, target, list, clue) = action
		val hand = state.hands(target)
		val chop = prev.chop(target)
		val reclue = list.forall(prev.state.deck(_).clued)

		lazy val pinkChoiceTempo = clue.kind == ClueKind.Rank &&
			state.includesVariant(PINKISH) &&
			reclue &&
			clue.value <= hand.length &&
			list.contains(hand(clue.value - 1)) &&
			List(list.max, hand(clue.value - 1)).forall(this.knownAs(_, PINKISH))

		lazy val brownTempo = clue.kind == ClueKind.Colour &&
			state.variant.colourableSuits(clue.value).contains("Brown") &&
			reclue

		lazy val ordered1s = order1s(list.filter(unknown1), noFilter = true)

		lazy val muddySuitIndex = state.variant.suits.indexWhere(MUDDY.matches)
		lazy val muddyCards = list.filter(this.knownAs(_, MUDDY))
		lazy val mudClue = clue.kind == ClueKind.Colour &&
			state.includesVariant(MUDDY) &&
			muddyCards.nonEmpty &&
			reclue &&
			// Mud clues should only work if the leftmost card is muddy.
			common.thoughts(list.max).possible.exists(_.suitIndex == muddySuitIndex)

		lazy val pinkStall5 = clue.eq(BaseClue(ClueKind.Rank, 5)) &&
			state.includesVariant(PINKISH) &&
			stallSeverity(prev, prev.common, giver) > 0

		if (chop.exists(list.contains))
			FocusResult(chop.get, chop = true)

		else if (pinkChoiceTempo)
			FocusResult(hand(clue.value - 1), positional = true)

		else if (brownTempo)
			FocusResult(list.min, positional = true)

		else if (clue.eq(BaseClue(ClueKind.Rank, 1)) && ordered1s.nonEmpty)
			FocusResult(ordered1s.head)

		else if (mudClue)
			val coloursAvailable = state.variant.colourableSuits.length
			val focusIndex = (clue.value - coloursAvailable + 6*muddyCards.length) % muddyCards.length
			FocusResult(muddyCards(focusIndex), positional = true)

		else if (pinkStall5)
			FocusResult(list.filter(!prev.state.deck(_).clued).min)

		else
			val sortedList = list.sortBy(o => -o)
			val focus =
				sortedList.find(o => !prev.state.deck(o).clued && prev.meta(o).status == CardStatus.None)
				.orElse(sortedList.find(o => prev.meta(o).status == CardStatus.ChopMoved))
				.orElse(sortedList.headOption)

			focus match {
				case Some(order) => FocusResult(order)
				case None => throw new Error("No focus found!")
			}

	def importantAction(playerIndex: Int): Boolean = ???

object HGroup:
	private def init(
		tableID: Int,
		state: State,
		inProgress: Boolean,
		t: (players: Vector[Player], common: Player)
	): HGroup =
	HGroup(
		tableID = tableID,
		state = state,
		players = t.players,
		common = t.common,
		base = (state, Vector(), t.players, t.common),
		inProgress = inProgress,
		lastActions = Vector.fill(state.numPlayers)(None)
	)

	def apply(tableID: Int, state: State, inProgress: Boolean) =
		init(tableID, state, inProgress, genPlayers(state))

	private def revert(g: HGroup, order: Int, ids: List[Identity], warn: Boolean = true) =
		// println(s"reverting conn ${g.state.logConn(conn)}")
		val newInferred = g.common.thoughts(order).inferred.difference(ids)

		if (newInferred.isEmpty)
			g.withThought(order) { t =>
				t.copy(
					inferred = t.oldInferred.map(_.intersect(t.possible)).getOrElse {
						if (warn)
							Log.error(s"no old inferences on ${order}!")
						t.possible
					},
					oldInferred = None
				)
			}
			.withMeta(order)(_.copy(status = CardStatus.None))
		else
			g.withThought(order)(_.copy(inferred = newInferred))

	given GameOps[HGroup] with
		def copyWith(game: HGroup, updates: GameUpdates) =
			val meta = updates.meta.getOrElse(game.meta)

			game.copy(
				tableID = updates.tableID.getOrElse(game.tableID),
				state = updates.state.getOrElse(game.state),
				players = updates.players.getOrElse(game.players),
				common = updates.common.getOrElse(game.common),
				base = updates.base.getOrElse(game.base),
				meta = meta,
				deckIds = updates.deckIds.getOrElse(game.deckIds),
				catchup = updates.catchup.getOrElse(game.catchup),
				notes = updates.notes.getOrElse(game.notes),
				lastMove = updates.lastMove.getOrElse(game.lastMove),
				queuedCmds = updates.queuedCmds.getOrElse(game.queuedCmds),
				nextInterp = updates.nextInterp.getOrElse(game.nextInterp),
				noRecurse = updates.noRecurse.getOrElse(game.noRecurse),
				rewindDepth = updates.rewindDepth.getOrElse(game.rewindDepth),
				inProgress = updates.inProgress.getOrElse(game.inProgress),

				goodTouch = game.goodTouch,
				level = game.level,
				stalled5 = game.stalled5,
				cluedOnChop = game.cluedOnChop,
				dcStatus = game.dcStatus,
				dda = game.dda,
				inEarlyGame = game.inEarlyGame,
				stallInterp = game.stallInterp,
				lastActions = game.lastActions,
				xmeta = game.xmeta.padTo(meta.length, XConvData())
			)

		def blank(game: HGroup, keepDeck: Boolean) =
			game.copy(
				tableID = game.tableID,
				state = game.base._1,
				inProgress = game.inProgress,
				deckIds = if (keepDeck) game.deckIds else Vector(),
				meta = game.base._2,
				players = game.base._3,
				common = game.base._4,
				base = game.base,

				stalled5 = false,
				dcStatus = DcStatus.None,
				dda = None,
				inEarlyGame = true,
				stallInterp = None,
				lastActions = Vector.fill(game.state.numPlayers)(None),
				xmeta = Vector.fill(game.base._2.length)(XConvData())
			)

		def interpretClue(prev: HGroup, game: HGroup, action: ClueAction): HGroup =
			interpClue(prev, game, action)
				.pipe { g =>
					g.copy(lastActions = g.lastActions.updated(action.playerIndex, Some(action)))
				}

		def interpretDiscard(prev: HGroup, game: HGroup, action: DiscardAction): HGroup =
			game.pipe { g =>
				g.copy(lastActions = g.lastActions.updated(action.playerIndex, Some(action)))
			}

		def interpretPlay(prev: HGroup, game: HGroup, action: PlayAction): HGroup =
			game.pipe { g =>
				g.copy(lastActions = g.lastActions.updated(action.playerIndex, Some(action)))
			}

		def takeAction(game: HGroup): PerformAction =
			???

		def updateTurn(game: HGroup, action: TurnAction): HGroup =
			val lastPlayerIndex = game.state.lastPlayerIndex(action.currentPlayerIndex)

			val initial = (
				game.copy(waiting = Nil),
				List[WaitingConnection](),
				Map[Int, IdentitySet]()
			)
			game.waiting.foldRight(initial) { case (wc, (newGame, newWCs, demos)) =>
				updateWc(game, wc, lastPlayerIndex) match {
					case UpdateResult.Keep =>
						(newGame, wc +: newWCs, demos)

					case UpdateResult.Advance(nextIndex, skipped) => (
						if (skipped)
							(0 until nextIndex).map(wc.connections)
								.foldLeft(newGame)((a, c) => revert(a, c.order, c.ids))
						else
							newGame,
						wc.copy(connections = wc.connections.drop(nextIndex)) +: newWCs,
						demos
					)
					case UpdateResult.AmbiguousPassback => (
						newGame,
						wc.copy(ambiguousPassback = true) +: newWCs,
						demos
					)
					case UpdateResult.Demonstrated(order, id, nextIndex, skipped) => (
						if (skipped)
							(0 until nextIndex.getOrElse(wc.connections.length))
								.map(wc.connections).foldLeft(newGame)((a, c) => revert(a, c.order, c.ids))
						else
							newGame,
						nextIndex match {
							case Some(i) => wc.copy(connections = wc.connections.drop(i)) +: newWCs
							case None => newWCs
						},
						demos + (order -> (demos.getOrElse(order, IdentitySet.empty).union(id)))
					)
					case UpdateResult.Remove => (
						wc.connections.foldLeft(newGame)((a, c) => revert(a, c.order, c.ids))
							.pipe(revert(_, wc.focus, List(wc.inference), warn = false)),
						newWCs,
						demos
					)
					case UpdateResult.Complete => (newGame, newWCs, demos)
				}
			}
			.pipe { (newGame, newWCs, demos) => (
				demos.foldLeft(newGame) { case (acc, (order, ids)) =>
					acc.withThought(order) { t =>
						val newInferred = t.inferred.intersect(ids)
						t.copy(
							inferred = newInferred,
							infoLock = Some(newInferred)
						)
					}
					.withXMeta(order)(_.copy(maybeFinessed = false))
				},
				newWCs.filterNot { wc =>
					demos.get(wc.focus).exists(d => d.contains(wc.inference))
				})
			}
			.pipe((newGame, newWCs) => newGame.copy(waiting = newWCs))
			.elim(goodTouch = true)

		def findAllClues(game: HGroup, giver: Int): List[PerformAction] =
			val state = game.state

			val level = Logger.level
			Logger.setLevel(LogLevel.Off)

			def validClue(clue: Clue, target: Int) =
				val list = state.clueTouched(state.hands(target), clue)

				// Do not simulate clues that touch only previously-clued trash
				!(list.forall(o => state.deck(o).clued && state.isBasicTrash(state.deck(o).id().get)))

			val allClues =
				(for
					target <- (0 until state.numPlayers).view if target != giver
					clue   <- state.allValidClues(target) if validClue(clue, target)
				yield
					clueToPerform(clue)).toList

			Logger.setLevel(level)
			allClues

		def findAllDiscards(game: HGroup, playerIndex: Int): List[PerformAction] =
			val trash = game.common.discardable(game, playerIndex)
			val target = trash.headOption
				.orElse(game.chop(playerIndex))
				.getOrElse(game.players(playerIndex).lockedDiscard(game.state, playerIndex))

			List(PerformAction.Discard(target))
