package scala_bot.reactor

import scala_bot.basics._
import scala_bot.endgame.EndgameSolver
import scala_bot.logger._
import scala_bot.utils._
import scala.util.chaining.scalaUtilChainingOps

case class ReactorWC(
	giver: Int,
	reacter: Int,
	receiver: Int,
	receiverHand: Vector[Int],
	clue: BaseClue,
	focusSlot: Int,
	inverted: Boolean,
	turn: Int
)

case class Reactor(
	tableID: Int,
	state: State,
	players: Vector[Player],
	common: Player,
	base: (State, Vector[ConvData], Vector[Player], Player),

	waiting: Option[ReactorWC] = None,

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

	goodTouch: Boolean = false
) extends Game

object Reactor:
	private def init(
		tableID: Int,
		state: State,
		inProgress: Boolean,
		t: (players: Vector[Player], common: Player)
	): Reactor =
		Reactor(
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

	private def checkMissed(game: Reactor, playerIndex: Int, actionOrder: Int) =
		game.state.hands(playerIndex)
			.find(o => game.meta(o).urgent && o != actionOrder)
			.fold(game) { urgent =>
				val newCommon = game.common.withThought(urgent)(t => t.copy(
					inferred = t.oldInferred.getOrElse(throw new Exception(s"No old inferred on $urgent!")),
					oldInferred = None
				))
				val newMeta = game.meta.updated(urgent,
					game.meta(urgent).cleared.reason(game.state.turnCount))

				game.copy(common = newCommon, meta = newMeta)
		}

	private def resetZcs(game: Reactor) =
		game.copy(
			meta = game.state.hands.foldLeft(game.meta) { (m, hand) =>
				hand.find(m(_).status == CardStatus.ZeroClueChop).fold(m) { zcs =>
					Log.info(s"resetting zcs on $zcs")
					m.updated(zcs, m(zcs).copy(status = CardStatus.None))
				}
			}
		)

	def chop(game: Reactor, playerIndex: Int) =
		game.state.hands(playerIndex).find { order =>
			val status = game.meta(order).status
			status == CardStatus.ZeroClueChop || status == CardStatus.CalledToDiscard
		}
		.orElse {
			game.state.hands(playerIndex).find { order =>
				!game.state.deck(order).clued && game.meta(order).status == CardStatus.None
			}
		}

	given GameOps[Reactor] with
		def copyWith(game: Reactor, updates: GameUpdates) =
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
				inProgress = updates.inProgress.getOrElse(game.inProgress)
			)

		def blank(game: Reactor, keepDeck: Boolean) =
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

		def interpretClue(prev: Reactor, game: Reactor, action: ClueAction): Reactor =
			val state = game.state
			val ClueAction(giver, target, _, _) = action

			val interpretedGame = checkMissed(game, giver, 99)
				.pipe { g =>
					g.waiting match {
						case Some(wc) if wc.reacter == giver => g.copy(waiting = None)
						case _ => g
					}
				}
				.pipe { g =>
					val (interp, interpGame) = g.nextInterp match {
						case Some(interp) =>
							Log.info(s"forcing rewinded interp $interp!")
							if (interp == ClueInterp.Reactive)
								val reacter = state.nextPlayerIndex(giver)
								interpretReactive(prev, g, action, reacter, inverted = false)
							else
								interpretStable(prev, g, action, stall = false)

						case None if prev.common.obviousLocked(prev, giver) || state.inEndgame || prev.state.clueTokens == 8 =>
							interpretStable(prev, g, action, stall = true)

						case None =>
							val reacter = (1 until state.numPlayers).view.map { i =>
								val playerIndex = (giver + i) % state.numPlayers

								// The clue may reveal a new playable, or the clue may fix a bad-touched card that looked playable previously
								val oldPlayables = prev.common.obviousPlayables(prev, playerIndex)
								val newPlayables = g.common.obviousPlayables(g, playerIndex)
								val playables = oldPlayables.filter(newPlayables.contains)

								if (playables.isEmpty)
									Log.info(s"reacter is ${state.names(playerIndex)}")
									Some(playerIndex)
								else
									Log.info(s"${state.names(playerIndex)} has playables $playables, not reacter")
									None
							}.find(_.isDefined).flatten

							val fixed = checkFix(prev, g, action) match {
								case FixResult.Normal(cluedResets, duplicateReveals) =>
									cluedResets ++ duplicateReveals
								case _ => Nil
							}
							val allowableFix = target == state.nextPlayerIndex(giver) && fixed.nonEmpty

							reacter match {
								case None => (Option.when(allowableFix)(ClueInterp.Fix), g)

								case Some(reacter) if reacter == target =>
									interpretStable(prev, g, action, stall = false)

								case Some(reacter) =>
									val prevPlayables = prev.players(target).obviousPlayables(prev, target)

									// Urgent fix on previous playable
									if (allowableFix && fixed.exists(prevPlayables.contains))
										(Some(ClueInterp.Fix), g)
									else
										interpretReactive(prev, g, action, reacter, inverted = false)
							}
						}

					if (interp.isEmpty)
						Log.warn("interpreted mistake!")

					interpGame.copy(lastMove = Some(interp.getOrElse(ClueInterp.Mistake)))
				}

			val signalledPlays = interpretedGame.state.hands.flatten.filter { o =>
				prev.meta(o).status != CardStatus.CalledToPlay && interpretedGame.meta(o).status == CardStatus.CalledToPlay
			}

			val eliminatedGame = interpretedGame.elim(goodTouch = false)
			val playsAfterElim = eliminatedGame.state.hands.flatten.filter(eliminatedGame.meta(_).status == CardStatus.CalledToPlay)

			eliminatedGame
				.when(_ => playsAfterElim.length < signalledPlays.length) { g =>
					Log.warn(s"lost play signal on ${signalledPlays.filterNot(playsAfterElim.contains)} after elim!")
					g.copy(lastMove = Some(ClueInterp.Mistake))
				}
				.when(_ => prev.state.canClue)(resetZcs)
				.when(!_.state.canClue) { g =>
					val zcsMeta = (0 until state.numPlayers).foldLeft(g.meta) { case (meta, i) =>
						chop(g, i) match {
							case Some(chop) if meta(chop).status == CardStatus.None =>
								Log.info(s"writing zcs on $chop")
								meta.updated(chop, meta(chop).copy(status = CardStatus.ZeroClueChop))
							case _ => meta
						}
					}
					g.copy(meta = zcsMeta)
				}
				.copy(nextInterp = None)

		def interpretDiscard(prev: Reactor, game: Reactor, action: DiscardAction): Reactor =
			val state = game.state
			val DiscardAction(playerIndex, order, suitIndex, rank, failed) = action
			val id = Identity(suitIndex, rank)

			val afterMissed = checkMissed(game, playerIndex, order)

			if (failed)
				Log.warn("bombed! clearing all information")

				val initial = (afterMissed.common, afterMissed.meta)
				val (clearedC, clearedM) = state.hands.flatten.foldLeft(initial) { case ((c, m), order) =>
					val newC = c.withThought(order)(t => t.copy(
						inferred = t.possible,
						oldInferred = None,
						infoLock = None,
					))
					val newM = m.updated(order, m(order).cleared)
					(newC, newM)
				}
				game.copy(
					waiting = None,
					common = clearedC,
					meta = clearedM
				)
			else
				(afterMissed.waiting match {
					case Some(wc) =>
						reactDiscard(prev, afterMissed, playerIndex, order, wc)
					case None if !failed && prev.state.deck(order).clued && suitIndex != -1 && rank != -1 && !state.isBasicTrash(id) =>
						val (interp, dcGame) = interpretUsefulDc(afterMissed, action)
						dcGame.copy(lastMove = Some(interp))
					case None =>
						afterMissed
				})
				.pipe(_.elim(goodTouch = false))
				.when(_ => prev.state.canClue)(resetZcs)

		def interpretPlay(prev: Reactor, game: Reactor, action: PlayAction): Reactor =
			val PlayAction(playerIndex, order, _, _) = action

			checkMissed(game, playerIndex, order)
				.pipe { g =>
					g.waiting match {
						case Some(wc) => reactPlay(prev, g, playerIndex, order, wc)
						case None => g
					}
				}
				.pipe(_.elim(goodTouch = false))
				.when(_ => prev.state.canClue)(resetZcs)

		def updateTurn(game: Reactor, action: TurnAction): Reactor =
			val currentPlayerIndex = action.currentPlayerIndex
			val state = game.state

			if (currentPlayerIndex == -1)
				game
			else
				val waitedGame = game.waiting match {
					case Some(wc) if wc.reacter == state.lastPlayerIndex(currentPlayerIndex) =>
						game.copy(waiting = None)
					case _ => game
				}

				val (newCommon, newMeta) = state.hands(currentPlayerIndex).foldLeft((game.common, game.meta)) { case ((c, m), order) =>
					if (m(order).status == CardStatus.CalledToPlay)
						val newInferred = c.thoughts(order).inferred.intersect(state.playableSet)

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

				waitedGame.copy(common = newCommon, meta = newMeta)
					.elim(goodTouch = true)

		def takeAction(game: Reactor): PerformAction =
			val (state, me) = (game.state, game.me)

			state.ourHand.find(game.meta(_).urgent) match {
				case Some(urgent) => game.meta(urgent).status match {
					case CardStatus.CalledToPlay if !me.thoughts(urgent).possible.forall(state.isBasicTrash) =>
						return PerformAction.Play(urgent)
					case CardStatus.CalledToDiscard =>
						return PerformAction.Discard(urgent)
					case _ =>
						Log.warn(s"Unexpected urgent card status ${game.meta(urgent).status}")
					}
				case _ => ()
			}

			if (state.inEndgame && state.remScore <= state.variant.suits.length + 1)
				Log.highlight(Console.MAGENTA, "trying to solve endgame...")

				EndgameSolver(monteCarlo = true).solve(game) match {
					case Left(err) => Log.info(s"couldn't solve endgame: $err")
					case Right((perform, _)) =>
						Log.info(s"endgame solved!")
						return perform
				}

			val discardOrders = me.discardable(game, state.ourPlayerIndex)
			val playableOrders = {
				val p = me.thinksPlayables(game, state.ourPlayerIndex)
				val knownP = p.filter(me.orderKp(game, _))

				if (knownP.nonEmpty) knownP else p
			}

			Log.info(s"playables $playableOrders")
			Log.info(s"discardable $discardOrders")

			val canClue = state.canClue && !game.waiting.exists(_.receiver == state.ourPlayerIndex)

			val allClues =
				for
					target <- (0 until state.numPlayers) if canClue && target != state.ourPlayerIndex
					clue <- state.allValidClues(target)
				yield
					val perform = clueToPerform(clue)
					val action = performToAction(state, perform, state.ourPlayerIndex)
					(perform, action)

			val allPlays = playableOrders.map { o =>
				val action = PlayAction(state.ourPlayerIndex, o, me.thoughts(o).id(infer = true))
				(PerformAction.Play(o), action)
			}

			val potentialReacter = state.nextPlayerIndex(state.ourPlayerIndex)

			// We have a play and the reacter might play on top of us
			lazy val potentialForcedPlay = allPlays.nonEmpty &&
				game.waiting.exists(_.reacter == potentialReacter) &&
				playableOrders.exists {
					game.me.thoughts(_).inferred.exists { id =>
						state.hands(potentialReacter).exists { o =>
							id.next.contains(state.deck(o).id().get)
						}
					}
				}

			val cantDiscard = state.clueTokens == 8 ||
				(state.pace == 0 && (allClues.nonEmpty || allPlays.nonEmpty)) ||
				potentialForcedPlay
			Log.info(s"can discard: ${!cantDiscard} ${state.clueTokens}")

			val allDiscards = if (cantDiscard) List() else
				discardOrders.map { o =>
					val action = DiscardAction(state.ourPlayerIndex, o, me.thoughts(o).id(infer = true))
					(PerformAction.Discard(o), action)
				}

			val allActions = {
				val as = allClues.concat(allPlays).concat(allDiscards)

				chop(game, state.ourPlayerIndex) match {
					case Some(chop) if !cantDiscard && (!state.canClue || allPlays.isEmpty) && allDiscards.isEmpty && !me.obviousLocked(game, state.ourPlayerIndex) =>
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

			allActions.maxBy((_, action) => evalAction(game, action))._1

		def findAllClues(game: Reactor, giver: Int): List[PerformAction] =
			val state = game.state

			val level = Logger.level
			Logger.setLevel(LogLevel.Off)

			def validClue(clue: Clue, target: Int) =
				val list = state.clueTouched(state.hands(target), clue)
				val action = ClueAction(giver, target, list, clue.toBase)

				// Do not simulate clues that touch only previously-clued trash
				if (list.forall(o => state.deck(o).clued && state.isBasicTrash(state.deck(o).id().get)))
					false
				else
					Log.highlight(Console.GREEN, s"===== Predicting value for ${clue.fmt(state)} =====")
					val hypoGame = game.simulateClue(action, log = true)
					getResult(game, hypoGame, action) > -1

			val allClues =
				(for
					target <- (0 until state.numPlayers).view if target != giver
					clue   <- state.allValidClues(target) if validClue(clue, target)
				yield
					clueToPerform(clue)).toList

			Logger.setLevel(level)
			allClues

		def findAllDiscards(game: Reactor, playerIndex: Int): List[PerformAction] =
			val trash = game.common.discardable(game, playerIndex)
			val target = trash.headOption
				.orElse(chop(game, playerIndex))
				.getOrElse(game.players(playerIndex).lockedDiscard(game.state, playerIndex))

			List(PerformAction.Discard(target))
