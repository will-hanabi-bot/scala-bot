package scala_bot.refSieve

import scala_bot.basics._
import scala_bot.endgame.EndgameSolver
import scala_bot.utils._
import scala_bot.logger.{Log, Logger, LogLevel}

case class RefSieve(
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

	goodTouch: Boolean = true
) extends Game

object RefSieve:
	private def init(
		tableID: Int,
		state: State,
		inProgress: Boolean,
		t: (players: Vector[Player], common: Player)
	): RefSieve =
	RefSieve(
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

	def chop(game: RefSieve, playerIndex: Int) =
		game.state.hands(playerIndex).find { order =>
			val status = game.meta(order).status
			status == CardStatus.ZeroClueChop || status == CardStatus.CalledToDiscard
		}
		.orElse {
			game.state.hands(playerIndex).find { order =>
				!game.state.deck(order).clued && game.meta(order).status == CardStatus.None
			}
		}

	def hasPtd(game: RefSieve, playerIndex: Int) =
		!game.common.thinksLocked(game, playerIndex) &&
		!game.common.thinksLoaded(game, playerIndex) &&
		!mustClue(game, playerIndex)

	def mustClue(game: RefSieve, playerIndex: Int) =
		val state = game.state

		state.canClue &&
		state.numPlayers > 2 && {
			val bobChop = RefSieve.chop(game, state.nextPlayerIndex(playerIndex))
			bobChop.flatMap(state.deck(_).id()).exists(id => state.isCritical(id) || state.isPlayable(id))
		}

	def findFinesse(game: RefSieve, playerIndex: Int, connected: Set[Int] = Set(), ignore: Set[Int] = Set()) =
		val order = game.state.hands(playerIndex).find { o =>
			!game.isTouched(o) && !connected.contains(o)
		}

		order.filter(!ignore.contains(_))

	given GameOps[RefSieve] with
		def copyWith(game: RefSieve, updates: GameUpdates) =
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

		def blank(game: RefSieve, keepDeck: Boolean) =
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

		def interpretClue(prev: RefSieve, game: RefSieve, action: ClueAction): RefSieve =
			val ClueAction(giver, target, list, clue) = action
			val newlyTouched = list.filter(!prev.state.deck(_).clued)
			val (clueResets, duplicateReveals) = checkFix(prev, game, action)

			// val resetOrder = clueResets.find(o => {
			// 	val thought = game.common.thoughts(o)
			// 	!thought.rewinded &&
			// 	thought.possible.length == 1 &&
			// 	game.common.dependentConns(o).nonEmpty
			// })

			// // There is a waiting connection that depends on this card
			// resetOrder.foreach { order =>
			// 	val action = IdentifyAction
			// 	return game.rewind()
			// }

			val fix = clueResets.nonEmpty || duplicateReveals.nonEmpty
			val trashPush = !fix && newlyTouched.forall(game.common.orderKt(game, _))

			val intent = determineFocus(prev, game, action, push = false)
			val distributionIds = distributionClue(prev, game, action, intent)

			if (!fix && !trashPush && distributionIds.isDefined)
				return game.withThought(intent) { t => t.copy(
					inferred = t.inferred.intersect(distributionIds.get),
					reset = false
				)}

			val prevPlayables = prev.common.thinksPlayables(prev, target)
			val prevLoaded = prev.common.thinksLoaded(prev, target)
			val stalling = prev.common.thinksLocked(prev, giver) || prev.state.clueTokens == 8

			lazy val focus = determineFocus(prev, game, action, push = trashPush || clue.kind == ClueKind.Colour)
			lazy val cluedGame = game.withMeta(focus)(_.copy(focused = true))

			lazy val newPlayables = cluedGame.common.thinksPlayables(cluedGame, target).filter(!prevPlayables.contains(_))
			lazy val loaded = cluedGame.common.thinksLoaded(cluedGame, target)

			val (interp, newGame) =
				if (!fix && prevLoaded)
					if (newlyTouched.nonEmpty)		// Loaded ref play
						if (clue.kind == ClueKind.Rank && stalling)
							Log.info("rank stall!")
							(Some(ClueInterp.Stall), game)
						else
							refPlay(prev, game, action, right = clue.kind == ClueKind.Rank && !trashPush)

					else if (stalling)
						Log.info("fill-in stall!")
						(Some(ClueInterp.Stall), game)

					else	// Loaded reclue
						targetPlay(prev, game, action, list.max)

				else if (newlyTouched.isEmpty)
					if (loaded)
						Log.info(s"revealed a safe action, not continuing")
						(Some(ClueInterp.Reveal), game)

					else if (stalling)
						Log.info("fill-in stall!")
						(Some(ClueInterp.Stall), game)

					else	// Reclue
						targetPlay(game, game, action, list.max)

				else if (trashPush)
					Log.info(s"trash push")
					refPlay(prev, game, action)

				else if (fix || (loaded && !(clue.kind == ClueKind.Colour && newPlayables.forall(newlyTouched.contains))))
					Log.info(s"revealed a safe action${if (fix) " (fix)" else ""}, not continuing")

					val newGame = cluedGame
						.when(_ => !fix && clue.kind == ClueKind.Rank && newPlayables.forall(newlyTouched.contains)) { g =>
							// Playable rank clue
							val focus = newlyTouched.max
							g.withMeta(focus)(_.copy(focused = true))
						}
					(Some(ClueInterp.Reveal), newGame)

				else if (clue.kind == ClueKind.Colour)
					refPlay(prev, cluedGame, action)

				else
					refDiscard(prev, cluedGame, action)

			if (interp.isEmpty)
				Log.warn("interpreted mistake!")

			newGame.copy(lastMove = Some(interp.getOrElse(ClueInterp.Mistake)))

		def interpretDiscard(prev: RefSieve, game: RefSieve, action: DiscardAction): RefSieve =
			val state = game.state
			val DiscardAction(playerIndex, order, suitIndex, rank, failed) = action
			val id = Identity(suitIndex, rank)

			if (!failed && prev.state.deck(order).clued && suitIndex != -1 && rank != -1 && !state.isBasicTrash(id))
				val (interp, dcGame) = interpretUsefulDc(game, action)
				dcGame.copy(lastMove = Some(interp))
			else
				game

		def interpretPlay(prev: RefSieve, game: RefSieve, action: PlayAction): RefSieve =
			game

		def takeAction(game: RefSieve): PerformAction =
			val (state, me) = (game.state, game.me)

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
				val playables = me.thinksPlayables(game, state.ourPlayerIndex)

				// Exclude cards if there is a duplicate that is fully known.
				playables.filterNot(p1 => playables.exists(p2 => p1 != p2 && me.thoughts(p2).id().exists(me.thoughts(p1).matches(_, infer = true))))
			}

			Log.info(s"playables $playableOrders")
			Log.info(s"discardable $discardOrders")

			val allClues =
				if (!state.canClue)
					List()
				else
					for
						offset <- 1 until state.numPlayers
						target = (state.ourPlayerIndex + offset) % state.numPlayers
						clue <- state.allValidClues(target)
					yield
						val perform = clueToPerform(clue)
						val action = performToAction(state, perform, state.ourPlayerIndex)
						(perform, action)

			val allPlays = playableOrders.map { order =>
				val action = me.thoughts(order).id(infer = true) match {
					case Some(Identity(suitIndex, rank)) =>
						PlayAction(state.ourPlayerIndex, order, suitIndex, rank)
					case None =>
						PlayAction(state.ourPlayerIndex, order, -1, -1)
				}
				(PerformAction.Play(order), action)
			}

			val cantDiscard = state.clueTokens == 8 ||
				RefSieve.mustClue(game, state.ourPlayerIndex) ||
				(state.pace == 0 && (allClues.nonEmpty || allPlays.nonEmpty))
			Log.info(s"can discard: ${!cantDiscard} ${state.clueTokens}")

			val allDiscards = if (cantDiscard) List() else
				discardOrders.map { order =>
					val action = me.thoughts(order).id() match {
						case Some(Identity(suitIndex, rank)) =>
							DiscardAction(state.ourPlayerIndex, order, suitIndex, rank, false)
						case None =>
							DiscardAction(state.ourPlayerIndex, order, -1, -1, false)
					}
					(PerformAction.Discard(order), action)
				}

			val allActions = {
				val as = allClues.concat(allPlays).concat(allDiscards)

				chop(game, state.ourPlayerIndex) match {
					case Some(chop) if !cantDiscard && (!state.canClue || allPlays.isEmpty) && allDiscards.isEmpty && !me.thinksLocked(game, state.ourPlayerIndex) =>
						as :+ (PerformAction.Discard(chop), DiscardAction(state.ourPlayerIndex, chop, -1, -1, false))
					case _ => as
				}
			}

			if (allActions.isEmpty)
				return PerformAction.Discard(me.lockedDiscard(state, state.ourPlayerIndex))

			allActions.maxBy((_, action) => evalAction(game, action))._1

		def updateTurn(prev: RefSieve, game: RefSieve, action: TurnAction): RefSieve =
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

		def findAllClues(game: RefSieve, giver: Int): List[PerformAction] =
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

		def findAllDiscards(game: RefSieve, playerIndex: Int): List[PerformAction] =
			val trash = game.common.discardable(game, playerIndex)
			val target = trash.headOption
				.orElse(chop(game, playerIndex))
				.getOrElse(game.players(playerIndex).lockedDiscard(game.state, playerIndex))

			List(PerformAction.Discard(target))
