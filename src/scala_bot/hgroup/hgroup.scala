package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.basics.given_Conversion_IdentitySet_Iterable
import scala_bot.endgame.EndgameSolver
import scala_bot.utils._
import scala_bot.logger.{Log, Logger, LogLevel}

import scala.util.chaining.scalaUtilChainingOps
import scala.util.matching.Regex

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
	queuedCmds: List[(String, String)] = List(),
	nextInterp: Option[Interp] = None,
	noRecurse: Boolean = false,
	rewindDepth: Int = 0,
	inProgress: Boolean = false,

	goodTouch: Boolean = true,
	level: Int = 1,
	stalled5: Boolean = false,
	cluedOnChop: Set[Int] = Set(),
	dcStatus: DcStatus = DcStatus.None,
	dda: Option[Identity] = None,
	inEarlyGame: Boolean = false,
	stallInterp: Option[StallInterp] = None
) extends Game

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
			inProgress = inProgress
		)

	def apply(
		tableID: Int,
		state: State,
		inProgress: Boolean
	) =
		init(tableID, state, inProgress, genPlayers(state))

	def chop(game: HGroup, playerIndex: Int) =
		game.state.hands(playerIndex).findLast { o =>
			!game.state.deck(o).clued && game.meta(o).status == CardStatus.None
		}

	/** Returns how far a card is from chop. A card on chop is 0-away. */
	def chopDistance(game: HGroup, playerIndex: Int, order: Int) =
		chop(game, playerIndex) match {
			case None =>
				throw new IllegalArgumentException(s"${game.state.names(playerIndex)} has no chop!")
			case Some(c) =>
				if (order < c)
					throw new IllegalArgumentException(s"order $order is right of chop $c!")

				game.state.hands(playerIndex).count { o =>
					o < order && o > c &&
					!game.state.deck(o).clued &&
					game.meta(o).status == CardStatus.None
				}
		}

	def findFinesse(game: HGroup, playerIndex: Int, connected: Set[Int] = Set(), ignore: Set[Int] = Set()) =
		val order = game.state.hands(playerIndex).find { o =>
			!game.isTouched(o) && !connected.contains(o)
		}

		order.filter(!ignore.contains(_))

	def unknown1(game: HGroup, order: Int) =
		val clues = game.state.deck(order).clues

		clues.nonEmpty && clues.forall(_.eq(ClueKind.Rank, 1))

	def order1s(game: HGroup, orders: Seq[Int], noFilter: Boolean = false) =
		val unknown1s = if (noFilter) orders else
			orders.filter { o =>
				unknown1(game, o) &&
				game.common.thoughts(o).possible.forall(_.rank == 1)
			}

		unknown1s.sortBy { o =>
			if (game.state.inStartingHand(o) && game.meta(o).status != CardStatus.ChopMoved)
				o
			else if (game.cluedOnChop.contains(o))
				-100 - o
			else
				-o
		}

	def priority(game: HGroup, orders: List[Int]) =
		val state = game.state

		val initial = (0 to 5).map(_ => Vector.empty[Int])
		orders.foldLeft(initial) { (acc, o) =>
			val thought = game.me.thoughts(o)

			val inFinesse = game.isBlindPlaying(o)	// TODO: play link?
			lazy val unknownCM = game.meta(o).status == CardStatus.ChopMoved &&
				!game.state.deck(o).clued &&
				thought.possible.exists(!state.isPlayable(_))

			def connecting(playerIndex: Int, id: Identity) =
				state.hands(playerIndex).exists(game.me.thoughts(_).matches(id.next, infer = true))

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
				if (game.isBlindPlaying(o))
					-200 - o
				else if (game.state.deck(o).clued)
					-100 - o
				else if (game.meta(o).hidden)
					-o
				else
					o
			})
			// Lowest rank, then leftmost
			.updated(5, ps(5).sortBy { o =>
				game.me.thoughts(o).possibilities.map(_.rank).min * 100 - o
			})
		}

	def determineFocus(prev: HGroup, game: HGroup, action: ClueAction): FocusResult =
		val state = game.state
		val ClueAction(giver, target, list, clue) = action
		val hand = state.hands(target)
		val chop = HGroup.chop(prev, target)
		val reclue = list.forall(prev.state.deck(_).clued)

		lazy val pinkChoiceTempo = clue.kind == ClueKind.Rank &&
			state.includesVariant(PINKISH) &&
			reclue &&
			clue.value <= hand.length &&
			list.contains(hand(clue.value - 1)) &&
			List(list.max, hand(clue.value - 1)).forall(game.knownAs(_, PINKISH))

		lazy val brownTempo = clue.kind == ClueKind.Colour &&
			Regex("Brown").matches(state.variant.colourableSuits(clue.value)) &&
			reclue

		lazy val ordered1s = order1s(game, list.filter(unknown1(game, _)), noFilter = true)

		lazy val muddySuitIndex = state.variant.suits.indexWhere(MUDDY.matches)
		lazy val muddyCards = list.filter(game.knownAs(_, MUDDY))
		lazy val mudClue = clue.kind == ClueKind.Colour &&
			state.includesVariant(MUDDY) &&
			muddyCards.nonEmpty &&
			reclue &&
			// Mud clues should only work if the leftmost card is muddy.
			game.common.thoughts(list.max).possible.exists(_.suitIndex == muddySuitIndex)

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

	given GameOps[HGroup] with
		def copyWith(game: HGroup, updates: GameUpdates) =
			game.copy(
				tableID = updates.tableID.getOrElse(game.tableID),
				state = updates.state.getOrElse(game.state),
				players = updates.players.getOrElse(game.players),
				common = updates.common.getOrElse(game.common),
				base = updates.base.getOrElse(game.base),
				meta = updates.meta.getOrElse(game.meta),
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
				inEarlyGame = game.inEarlyGame
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
				base = game.base
			)

		def interpretClue(prev: HGroup, game: HGroup, action: ClueAction): HGroup =
			val state = game.state
			val ClueAction(giver, target, list, clue) = action

			val focusResult @ FocusResult(focus, chop, positional) = determineFocus(prev, game, action)
			val (cluedResets, duplicateReveals) = checkFix(prev, game, action)

			if (cluedResets.nonEmpty || duplicateReveals.nonEmpty)
				Log.info(s"fix clue! not inferring anything else")

				lazy val oldOrdered1s = order1s(prev, list.filter(unknown1(prev, _)), noFilter = true)
				val pinkFix1s = state.includesVariant(PINKISH) &&
					clue.kind == ClueKind.Rank && clue.value != 1 &&
					oldOrdered1s.nonEmpty

				return game.when (_ => pinkFix1s) { g =>
					val fixedOrder = oldOrdered1s.head

					if (chop && (clue.value == 2 || clue.value == 5))
						Log.info(s"pink fix!")
						g.withThought(fixedOrder)(t => t.copy(
							inferred = t.possible.retain(!state.isPlayable(_))
						))
					else
						Log.info(s"pink fix promise!")
						g.withThought(fixedOrder)(t => t.copy(
							inferred = t.inferred.retain(i => i.rank == clue.value && !state.isPlayable(i))
						))
				}
				.withMeta(focus) {
					// Focus doesn't matter for a fix clue
					_.copy(focused = prev.meta(focus).focused)
				}
				.copy(lastMove = Some(ClueInterp.Fix))

			val stall = stallingSituation(prev, game, action, focusResult)

			if (stall.isDefined)
				val (interp, thinksStall) = stall.get

				if (thinksStall.size > 0 && thinksStall.size < state.numPlayers)
					Log.warn(s"asymmetric move! interpreting mistake")
					return game.copy(lastMove = Some(ClueInterp.Mistake))

				if (thinksStall.size == state.numPlayers)
					Log.info(s"stalling situation $interp")

					return game
						.when(g => interp == StallInterp.Stall5 && g.inEarlyGame) {
							_.copy(stalled5 = true)
						}
						// Pink promise on stalls
						.when(g => g.state.includesVariant(PINKISH) && clue.kind == ClueKind.Rank) {
							_.withThought(focus)(t => t.copy(inferred = t.inferred.retain(_.rank == clue.value)))
						}
						.copy(
							lastMove = Some(ClueInterp.Stall),
							stallInterp = Some(interp)
						)

			val distributionIds = distributionClue(prev, game, action, focus)

			if (distributionIds.isDefined)
				Log.info(s"distribution clue!")

				return game
					.withThought(focus) { t => t.copy(
						inferred = t.possible.intersect(distributionIds.get),
						infoLock = Some(t.possible.intersect(distributionIds.get)),
						reset = false
					)}
					.copy(
						lastMove = Some(ClueInterp.Distribution)
					)

			if (game.level >= Level.BasicCM && !state.inEndgame)
				val tcm = interpretTcm(prev, game, action, focus)

				if (tcm.isDefined)
					// All newly cards are trash
					return list.foldLeft(game) { (acc, order) =>
						if (prev.state.deck(order).clued)
							acc
						else
							acc.withThought(order) { t =>
								val newInferred = t.possible.retain(state.isBasicTrash)
								t.copy(
									inferred = newInferred,
									infoLock = Some(newInferred)
								)
							}
							.withMeta(order)(_.copy(trash = true))
					}
					.pipe(performCM(_, tcm.get))
					.copy(lastMove = Some(ClueInterp.Discard))

				val cm5 = interpret5cm(prev, game, action, focus)

				if (cm5.isDefined)
					return performCM(game, cm5.get)
						.copy(lastMove = Some(ClueInterp.Discard))

			val pinkTrashFix = state.includesVariant(PINKISH) &&
				!positional && clue.kind == ClueKind.Rank &&
				list.forall(o => prev.state.deck(o).clued && game.knownAs(o, PINKISH)) &&
				state.variant.suits.zipWithIndex.forall { (suit, suitIndex) =>
					!PINKISH.matches(suit) ||
					game.common.isTrash(game, Identity(suitIndex, clue.value), focus)
				}

			if (pinkTrashFix)
				Log.info(s"pink trash fix!")
				return game
					.withThought(focus) { t =>
						val newInferred = t.possible.retain(game.common.isTrash(game, _, focus))
						t.copy(
							inferred = t.possible.retain(game.common.isTrash(game, _, focus)),
							infoLock = Some(newInferred)
						)
					}
					.withMeta(focus){ m => m.copy(
						trash = m.trash || state.variant.suits.zipWithIndex.forall { (suit, suitIndex) =>
							!PINKISH.matches(suit) ||
							game.state.isBasicTrash(Identity(suitIndex, clue.value))
						}
					)}
					.copy(lastMove = Some(ClueInterp.Fix))



			game

		def interpretDiscard(prev: HGroup, game: HGroup, action: DiscardAction): HGroup =
			???

		def interpretPlay(prev: HGroup, game: HGroup, action: PlayAction): HGroup =
			game

		def takeAction(game: HGroup): PerformAction =
			???

		def updateTurn(prev: HGroup, game: HGroup, action: TurnAction): HGroup =
			val currentPlayerIndex = action.currentPlayerIndex
			val state = game.state

			val (newCommon, newMeta) = state.hands(currentPlayerIndex).foldLeft((game.common, game.meta)) { case ((c, m), order) =>
				if (m(order).status == CardStatus.CalledToPlay)
					val newInferred = c.thoughts(order).inferred.retain(state.isPlayable)

					if (newInferred.isEmpty)
						val newCommon = c.withThought(order)(_.resetInferences())
						val newMeta = m.updated(order, m(order).copy(
							status = CardStatus.None,
							by = None,
							trash = true
						))
						(newCommon, newMeta)
					else
						(c.withThought(order)(_.copy(inferred = newInferred)), m)
				else
					(c, m)
			}

			game.copy(common = newCommon, meta = newMeta)
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
				.orElse(chop(game, playerIndex))
				.getOrElse(game.players(playerIndex).lockedDiscard(game.state, playerIndex))

			List(PerformAction.Discard(target))
