package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.basics.given_Conversion_IdentitySet_Iterable
import scala_bot.endgame.EndgameSolver
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
	xmeta: Vector[XConvData] = Vector(),

	allowFindOwn: Boolean = true
) extends Game:
	override def filterPlayables(player: Player, _playerIndex: Int, orders: Vector[Int]): Vector[Int] =
		orders.filter(o => player.orderKp(this, o) || !xmeta(o).maybeFinessed)

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

	def mustClue(playerIndex: Int) =
		val bob = state.nextPlayerIndex(playerIndex)

		state.canClue &&
		state.numPlayers > 2 &&
		!common.thinksLoaded(this, bob) &&
		chop(bob).flatMap(state.deck(_).id()).exists(id => state.isCritical(id))

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
				state.hands(playerIndex).exists { o =>
					id.next.exists { i =>
						this.me.thoughts(o).matches(i, infer = true)
					}
				}

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
		t: (players: Vector[Player], common: Player),
		level: Int
	): HGroup =
	HGroup(
		tableID = tableID,
		state = state,
		players = t.players,
		common = t.common,
		base = (state, Vector(), t.players, t.common),
		inProgress = inProgress,
		lastActions = Vector.fill(state.numPlayers)(None),
		level = level
	)

	def apply(tableID: Int, state: State, inProgress: Boolean, level: Int) =
		init(tableID, state, inProgress, genPlayers(state), level)

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
				waiting = game.waiting,
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
			HGroup(
				tableID = game.tableID,
				state = game.base._1,
				meta = game.base._2,
				players = game.base._3,
				common = game.base._4,
				base = game.base,

				level = game.level,
				deckIds = if (keepDeck) game.deckIds else Vector(),
				lastActions = Vector.fill(game.state.numPlayers)(None),
				xmeta = Vector.fill(game.base._2.length)(XConvData())
			)

		def interpretClue(prev: HGroup, game: HGroup, action: ClueAction): HGroup =
			refreshWCs(prev, game, action)
				.pipe(g => interpClue(ClueContext(prev, g, action)))
				.pipe { g =>
					g.copy(lastActions = g.lastActions.updated(action.playerIndex, Some(action)))
				}

		def interpretDiscard(prev: HGroup, game: HGroup, action: DiscardAction): HGroup =
			refreshWCs(prev, game, action)
				.pipe { g =>
					val DiscardAction(playerIndex, order, _, _, failed) = action
					val endEarlyGame = g.inEarlyGame &&
						!failed &&
						!g.state.deck(order).clued &&
						g.meta(order).status == CardStatus.None

					g.when(_ => endEarlyGame)
						(_.copy(inEarlyGame = false))
					.copy(lastActions = g.lastActions.updated(playerIndex, Some(action)))
				}

		def interpretPlay(prev: HGroup, game: HGroup, action: PlayAction): HGroup =
			refreshWCs(prev, game, action)
				.pipe { g =>
					g.copy(lastActions = g.lastActions.updated(action.playerIndex, Some(action)))
				}

		def takeAction(game: HGroup): PerformAction =
			val (state, me) = (game.state, game.me)

			if (state.inEndgame && state.remScore <= state.variant.suits.length + 1)
				Log.highlight(Console.MAGENTA, "trying to solve endgame...")

				EndgameSolver(monteCarlo = true).solve(game) match {
					case Left(err) => Log.info(s"couldn't solve endgame: $err")
					case Right((perform, _)) =>
						Log.info(s"endgame solved!")
						return perform
				}

			val discardOrders = me.thinksTrash(game, state.ourPlayerIndex)
			val playableOrders = me.thinksPlayables(game, state.ourPlayerIndex)

			Log.info(s"playables $playableOrders")
			Log.info(s"discardable $discardOrders")

			val allClues =
				for
					target <- (0 until state.numPlayers) if state.canClue && target != state.ourPlayerIndex
					clue <- state.allValidClues(target)
				yield
					val perform = clueToPerform(clue)
					val action = performToAction(state, perform, state.ourPlayerIndex)
					(perform, action)

			val clueEvals = allClues.view.map((_, action) => (action, evalAction(game, action)))

			val earlyGameClue = game.inEarlyGame &&
				clueEvals.exists { case (action, (hypo, _)) => hypo.lastMove match {
					case Some(ClueInterp.Save) =>
						true
					case Some(ClueInterp.Stall) =>
						hypo.stallInterp == Some(StallInterp.Stall5) &&
						!game.stalled5
					case Some(ClueInterp.Play) =>
						val (badTouch, _, _) = badTouchResult(game, hypo, action.asInstanceOf[ClueAction])
						badTouch.isEmpty

					case _ => false
				}}

			if (earlyGameClue)
				Log.highlight(Console.YELLOW,"must give clue in early game!")

			val allPlays = playableOrders.map { o =>
				val action = PlayAction(state.ourPlayerIndex, o, me.thoughts(o).id(infer = true))
				(PerformAction.Play(o), action)
			}

			val cantDiscard = state.clueTokens == 8 ||
				(state.pace == 0 && (allClues.nonEmpty || allPlays.nonEmpty)) ||
				earlyGameClue
			Log.info(s"can discard: ${!cantDiscard} ${state.clueTokens}")

			val allDiscards = if (cantDiscard) List() else
				discardOrders.map { o =>
					val action = DiscardAction(state.ourPlayerIndex, o, me.thoughts(o).id(infer = true))
					(PerformAction.Discard(o), action)
				}

			val allActions = {
				val as = allClues.concat(allPlays).concat(allDiscards)

				game.chop(state.ourPlayerIndex) match {
					case Some(chop) if !cantDiscard && (!state.canClue || allPlays.isEmpty) && allDiscards.isEmpty && !me.thinksLocked(game, state.ourPlayerIndex) =>
						as :+ (PerformAction.Discard(chop), DiscardAction(state.ourPlayerIndex, chop, -1, -1, false))
					case _ => as
				}
			}

			if (allActions.isEmpty)
				if (state.clueTokens == 8)
					Log.error("No actions available at 8 clues! Playing slot 1")
					return PerformAction.Play(state.ourHand.head)
				else
					return PerformAction.Discard(me.lockedDiscard(state, state.ourPlayerIndex))

			allActions.maxBy((_, action) => evalAction(game, action)._2)._1

		def updateTurn(game: HGroup, action: TurnAction) =
			game

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
			val trash = game.common.thinksTrash(game, playerIndex)
			val target = trash.headOption
				.orElse(game.chop(playerIndex))
				.getOrElse(game.players(playerIndex).lockedDiscard(game.state, playerIndex))

			List(PerformAction.Discard(target))

	def atLevel(level: Int) =
		(tableID: Int, state: State, inProgress: Boolean) =>
			HGroup(tableID, state, inProgress, level)
