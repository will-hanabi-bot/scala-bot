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
	future: Vector[IdentitySet] = Vector(),
	catchup: Boolean = false,
	notes: Map[Int, Note] = Map(),
	lastMove: Option[Interp] = None,
	queuedCmds: List[(String, String)] = Nil,
	nextInterp: Option[Interp] = None,
	noRecurse: Boolean = false,
	rewindDepth: Int = 0,
	inProgress: Boolean = false,

	goodTouch: Boolean = true,
	waiting: List[WaitingConnection] = Nil,
	zcsTurn: Option[Int] = None
) extends Game:
	def chop(playerIndex: Int) =
		state.hands(playerIndex).find {
			meta(_).status == CardStatus.CalledToDiscard
		}
		.orElse {
			state.hands(playerIndex).find { order =>
				zcsTurn.forall(_ >= state.deck(order).drawnIndex) &&
				!state.deck(order).clued &&
				meta(order).status == CardStatus.None
			}
		}

	def hasPtd(playerIndex: Int) =
		!common.thinksLocked(this, playerIndex) &&
		!common.thinksLoaded(this, playerIndex) &&
		!mustClue(playerIndex)

	def mustClue(playerIndex: Int) =
		val bob = state.nextPlayerIndex(playerIndex)

		state.canClue &&
		state.numPlayers > 2 &&
		!common.thinksLoaded(this, bob) &&
		chop(bob).flatMap(state.deck(_).id()).exists(id => state.isCritical(id) || state.isPlayable(id))

	def findFinesse(playerIndex: Int, connected: List[Int] = Nil, ignore: Set[Int] = Set()) =
		val order = state.hands(playerIndex).find { o =>
			!this.isTouched(o) && !connected.contains(o)
		}

		order.filter(!ignore.contains(_))

	def dependentConns(order: Int) =
		waiting.filter(_.connections.exists(_.order == order))

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

	def apply(tableID: Int, state: State, inProgress: Boolean) =
		init(tableID, state, inProgress, genPlayers(state))

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
				rewindDepth = updates.rewindDepth.getOrElse(game.rewindDepth),
				inProgress = updates.inProgress.getOrElse(game.inProgress),

				waiting = game.waiting
			)

		def blank(game: RefSieve, keepDeck: Boolean) =
			game.copy(
				tableID = game.tableID,
				state = game.base._1,
				inProgress = game.inProgress,
				deckIds = if keepDeck then game.deckIds else Vector(),
				meta = game.base._2,
				players = game.base._3,
				common = game.base._4,
				base = game.base
			)

		def interpretClue(prev: RefSieve, game: RefSieve, action: ClueAction): RefSieve =
			val ClueAction(giver, target, list, clue) = action
			val newlyTouched = list.filter(!prev.state.deck(_).clued)

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

			val fix = checkFix(prev, game, action).matches {
				case _: FixResult.Normal => true
			}
			val trashPush = !fix && newlyTouched.forall(game.common.orderKt(game, _))

			val intent = determineFocus(prev, game, action, push = false)
			val distributionIds = distributionClue(prev, game, action, intent)

			if !fix && !trashPush && distributionIds.isDefined then
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
				if !fix && prevLoaded then
					if newlyTouched.nonEmpty then		// Loaded ref play
						if clue.kind == ClueKind.Rank && stalling then
							Log.info("rank stall!")
							(Some(ClueInterp.Stall), game)
						else
							refPlay(prev, game, action, right = clue.kind == ClueKind.Rank && !trashPush)

					else if stalling then
						Log.info("fill-in stall!")
						(Some(ClueInterp.Stall), game)

					else	// Loaded reclue
						targetPlay(prev, game, action, list.max)

				else if newlyTouched.isEmpty then
					if loaded then
						Log.info(s"revealed a safe action, not continuing")
						(Some(ClueInterp.Reveal), game)

					else if stalling then
						Log.info("fill-in stall!")
						(Some(ClueInterp.Stall), game)

					else	// Reclue
						targetPlay(game, game, action, list.max)

				else if trashPush then
					Log.info(s"trash push")
					refPlay(prev, game, action)

				else if fix || (loaded && !(clue.kind == ClueKind.Colour && newPlayables.forall(newlyTouched.contains))) then
					Log.info(s"revealed a safe action${if fix then " (fix)" else ""}, not continuing")

					val newGame = cluedGame
						.when(_ => !fix && clue.kind == ClueKind.Rank && newPlayables.forall(newlyTouched.contains)) { g =>
							// Playable rank clue
							val focus = newlyTouched.max
							g.withMeta(focus)(_.copy(focused = true))
						}
					(Some(ClueInterp.Reveal), newGame)

				else if clue.kind == ClueKind.Colour then
					refPlay(prev, cluedGame, action)

				else
					refDiscard(prev, cluedGame, action)

			if interp.isEmpty then
				Log.warn("interpreted mistake!")

			newGame.copy(lastMove = Some(interp.getOrElse(ClueInterp.Mistake)))

		def interpretDiscard(prev: RefSieve, game: RefSieve, action: DiscardAction): RefSieve =
			val state = game.state
			val DiscardAction(playerIndex, order, suitIndex, rank, failed) = action
			val id = Identity(suitIndex, rank)

			if !failed && prev.state.deck(order).clued && suitIndex != -1 && rank != -1 && !state.isBasicTrash(id) then
				interpretUsefulDc(game, action) match {
					case DiscardResult.None =>
						game.copy(lastMove = Some(DiscardInterp.None))

					case DiscardResult.Mistake =>
						game.copy(lastMove = Some(DiscardInterp.Mistake))

					case DiscardResult.GentlemansDiscard(target) =>
						game.copy(
							common = game.common.withThought(target)(_.copy(
								inferred = IdentitySet.single(id)
							)),
							meta = game.meta.updated(target, game.meta(target).copy(
								status = CardStatus.GentlemansDiscard
							)),
							lastMove = Some(DiscardInterp.GentlemansDiscard)
						)
					case DiscardResult.Sarcastic(orders) =>
						game.copy(
							common = game.common.copy(
								links = Link.Sarcastic(orders, id) +: game.common.links
							),
							lastMove = Some(DiscardInterp.Sarcastic)
						)
				}
			else
				game

		def interpretPlay(prev: RefSieve, game: RefSieve, action: PlayAction): RefSieve =
			game

		def takeAction(game: RefSieve): PerformAction =
			val (state, me) = (game.state, game.me)

			if state.inEndgame && state.remScore <= state.variant.suits.length + 1 then
				Log.highlight(Console.MAGENTA, "trying to solve endgame...")

				EndgameSolver(monteCarlo = true).solve(game) match {
					case Left(err) => Log.info(s"couldn't solve endgame: $err")
					case Right((perform, _)) =>
						Log.info(s"endgame solved!")
						return perform
				}

			val discardOrders = me.discardable(game, state.ourPlayerIndex)
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

			val allPlays = playableOrders.map { o =>
				val action = PlayAction(state.ourPlayerIndex, o, me.thoughts(o).id(infer = true))
				(PerformAction.Play(o), action)
			}

			val cantDiscard = state.clueTokens == 8 ||
				game.mustClue(state.ourPlayerIndex) ||
				(state.pace == 0 && (allClues.nonEmpty || allPlays.nonEmpty))
			Log.info(s"can discard: ${!cantDiscard} ${state.clueTokens}")

			val allDiscards = if cantDiscard then Nil else
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

			if allActions.isEmpty then
				if state.clueTokens == 8 then
					Log.error("No actions available at 8 clues! Playing slot 1")
					return PerformAction.Play(state.ourHand.head)
				else
					return PerformAction.Discard(me.lockedDiscard(state, state.ourPlayerIndex))

			allActions.maxBy((_, action) => evalAction(game, action))._1

		def updateTurn(game: RefSieve, action: TurnAction): RefSieve =
			val currentPlayerIndex = action.currentPlayerIndex
			val state = game.state

			val (newCommon, newMeta) = state.hands(currentPlayerIndex).foldLeft((game.common, game.meta)) { case ((c, m), order) =>
				if m(order).status == CardStatus.CalledToPlay then
					val newInferred = c.thoughts(order).inferred.intersect(state.playableSet)

					if newInferred.isEmpty then
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

		def findAllClues(game: RefSieve, giver: Int) =
			val state = game.state

			val level = Logger.level
			Logger.setLevel(LogLevel.Off)

			def validClue(clue: Clue, target: Int) =
				val list = state.clueTouched(state.hands(target), clue)

				// Do not simulate clues that touch only previously-clued trash
				!(list.forall(o => state.deck(o).clued && state.isBasicTrash(state.deck(o).id().get)))

			val allClues =
				(for
					target <- (0 until state.numPlayers) if target != giver
					clue   <- state.allValidClues(target) if validClue(clue, target)
				yield
					clueToPerform(clue))

			Logger.setLevel(level)
			allClues

		def findAllDiscards(game: RefSieve, playerIndex: Int): List[PerformAction] =
			val trash = game.common.discardable(game, playerIndex)
			val target = trash.headOption
				.orElse(game.chop(playerIndex))
				.getOrElse(game.players(playerIndex).lockedDiscard(game.state, playerIndex))

			List(PerformAction.Discard(target))
