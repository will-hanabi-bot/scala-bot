package scala_bot.refSieve

import cats.effect.IO

import scala_bot.basics._
import scala_bot.endgame.EndgameSolver
import scala_bot.utils._
import scala_bot.logger.{Log, Logger, LogLevel}

enum UpdateResult:
	case Remove
	case Keep

case class RefSieve(
	tableID: Int,
	state: State,
	players: Vector[Player],
	common: Player,
	base: (State, Vector[ConvData], Vector[Player], Player),
	lastActions: Vector[Option[Action]],

	meta: Vector[ConvData] = Vector(),
	deckIds: Vector[Option[Identity]],
	future: Vector[IdentitySet],
	catchup: Boolean = false,
	notes: Map[Int, Note] = Map(),
	moveHistory: Vector[Interp] = Vector.empty,
	queuedCmds: List[(String, String)] = Nil,
	nextInterp: Option[Interp] = None,
	noRecurse: Boolean = false,
	rewindDepth: Int = 0,
	inProgress: Boolean = false,

	goodTouch: Boolean = true,
	waiting: List[WaitingConnection] = Nil,
	zcsTurn: Option[Int] = None
) extends Game:
	override def filterPlayables(player: Player, playerIndex: Int, orders: Seq[Int], assume: Boolean): Seq[Int] =
		orders.filter: o =>
			!player.links.exists(l => l.getOrders.contains(o) && l.getOrders.max != o)

	override def validArr(id: Identity, order: Int): Boolean =
		val playables = this.me.thinksPlayables(this, state.ourPlayerIndex)

		if playables.contains(order) then
			state.isPlayable(id)
		else if this.isTouched(order) && !this.common.thoughts(order).reset then
			val good = this.me.thoughts(order).possible.difference(state.trashSet)
			good.isEmpty || good.contains(id)
		else
			true

	def chop(playerIndex: Int) =
		state.hands(playerIndex).find:
			meta(_).status == CardStatus.CalledToDiscard
		.orElse:
			state.hands(playerIndex).find: order =>
				zcsTurn.forall(_ >= state.deck(order).turnDrawn) &&
				!state.deck(order).clued &&
				meta(order).status == CardStatus.None

	def mustClue(playerIndex: Int) =
		val bob = state.nextPlayerIndex(playerIndex)

		state.canClue &&
		state.numPlayers > 2 &&
		!common.thinksLoaded(this, bob) &&
		chop(bob).flatMap(state.deck(_).id()).exists(id => state.isCritical(id) || state.isPlayable(id))

	def findFinesse(playerIndex: Int, connected: Set[Int] = Set(), ignore: Set[Int] = Set()) =
		val order = state.hands(playerIndex).find: o =>
			!this.isTouched(o) && !connected.contains(o)

		order.filter(!ignore.contains(_))

	def dependentConns(order: Int) =
		waiting.filter(_.connections.exists(_.order == order))

	def reinterpPlay(prev: RefSieve, action: PlayAction | DiscardAction): Option[RefSieve] =
		val (order, suitIndex, rank) = action match
			case PlayAction(playerIndex, order, suitIndex, rank) => (order, suitIndex, rank)
			case DiscardAction(playerIndex, order, suitIndex, rank, _) => (order, suitIndex, rank)

		if suitIndex == -1 || rank == -1 then
			return None

		val needsReplay =
			action.playerIndex == state.ourPlayerIndex &&
			prev.me.thoughts(order).possible.length > 1 &&
			future(order).length > 1

		Option.when(needsReplay) {
			copy(future = future.updated(order, IdentitySet.single(Identity(suitIndex, rank))))
			.replay
			.toOption
		}.flatten

	/** Updates the game state if the partner now has PTD. */
	def check2pPtd(prev: RefSieve, action: PlayAction | ClueAction | DiscardAction): RefSieve =
		val playerIndex = action.playerIndex
		val partner = state.nextPlayerIndex(playerIndex)

		if state.numPlayers > 2 || prev.common.thinksLocked(prev, partner) || prev.common.thinksLoaded(prev, partner) then
			return this

		val writePtd = action match
			case _: DiscardAction => true
			case _: ClueAction => prev.common.thinksLocked(prev, playerIndex)
			case p: PlayAction =>
				// Try playing the common knowledge id, if known, then see if partner becomes loaded.
				val id = prev.common.thoughts(p.order).id(infer = true)
				val hypo = id.fold(prev)(i => prev.copy(state = prev.state.withPlay(i)))

				!hypo.common.thinksLoaded(hypo, partner)

		if writePtd then
			chop(partner).fold(this): chop =>
				Log.info(s"writing ptd on ${state.names(partner)}'s chop $chop")
				this.withMeta(chop)(_.copy(status = CardStatus.PermissionToDiscard))
		else
			this

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
		deckIds = Vector.fill(state.variant.totalCards)(None),
		future = Vector.fill(state.variant.totalCards)(state.allIds),
		lastActions = Vector.fill(state.numPlayers)(None),
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
				lastActions = updates.lastActions.getOrElse(game.lastActions),
				moveHistory = updates.moveHistory.getOrElse(game.moveHistory),
				queuedCmds = updates.queuedCmds.getOrElse(game.queuedCmds),
				nextInterp = updates.nextInterp.getOrElse(game.nextInterp),
				rewindDepth = updates.rewindDepth.getOrElse(game.rewindDepth),
				inProgress = updates.inProgress.getOrElse(game.inProgress),
				noRecurse = updates.noRecurse.getOrElse(game.noRecurse)
			)

		def blank(game: RefSieve, keepDeck: Boolean) =
			RefSieve(
				tableID = game.tableID,
				state = game.base._1,
				players = game.base._3,
				meta = game.base._2,
				common = game.base._4,
				base = game.base,
				inProgress = game.inProgress,

				deckIds = if keepDeck then game.deckIds else Vector.fill(game.state.variant.totalCards)(None),
				future = if keepDeck then game.future else Vector.fill(game.state.variant.totalCards)(game.state.allIds),
				lastActions = Vector.fill(game.state.numPlayers)(None)
			)

		def interpretClue(_prev: RefSieve, _game: RefSieve, action: ClueAction): RefSieve =
			val state = _game.state
			val ClueAction(giver, target, list, clue) = action
			val newlyTouched = list.filter(!_prev.state.deck(_).clued)

			val (prev, game) = if state.numPlayers > 2 then (_prev, _game) else
				list.foldLeft((_prev, _game)): (acc, o) =>
					val (prev, game) = acc
					// Remove PTD on clued cards that are not 1s/2s
					val removePtd =
						state.deck(o).clued &&
						prev.meta(o).status == CardStatus.PermissionToDiscard &&
						!state.deck(o).id(partial = true).exists(id => id.rank == 1 || id.rank == 2)

					if removePtd then
						(prev.withMeta(o)(_.copy(status = CardStatus.None)), game.withMeta(o)(_.copy(status = CardStatus.None)))
					else
						acc

			val ctx = ClueContext(prev, game, action)

			val fix = checkFix(prev, game, action).isInstanceOf[FixResult.Normal]
			val trashPush = !fix && newlyTouched.forall(game.common.orderKt(game, _))

			val intent = determineFocus(ctx, push = false)

			if !fix && !trashPush then
				distributionClue(prev, game, action, intent) match
					case Some(ids) =>
						return game.withThought(intent): t =>
							t.copy(
								inferred = t.inferred.intersect(ids),
								reset = false
							)
					case None => ()

			val prevPlayables = prev.common.thinksPlayables(prev, target)
			val prevLoaded = prev.common.thinksLoaded(prev, target)
			val stalling = prev.common.thinksLocked(prev, giver) || prev.state.clueTokens == 8

			val noInfo = state.numPlayers == 2 && !stalling && !state.inEndgame && list.forall: o =>
				prev.common.thoughts(o).possible == game.common.thoughts(o).possible

			if noInfo then
				// No-Info Double Bluff
				return game.findFinesse(action.target) match
					case Some(f1) =>
						def validNI(o1: Int, o2: Int) =
							val valid = for
								id1 <- state.deck(o1).id()
								id2 <- state.deck(o2).id()
							yield
								state.isPlayable(id1) &&
								(state.isPlayable(id2) || id1.next.exists(_ == id2))

							valid.forall(x => x)

						game.findFinesse(action.target, Set(f1)) match
							case Some(f2) =>
								Log.info(s"no-info double bluff on $f1 and $f2!")

								game.withMeta(f1)(_.copy(status = CardStatus.CalledToPlay))
									.withMeta(f2)(_.copy(status = CardStatus.CalledToPlay))
									.withMove(if validNI(f1, f2) then ClueInterp.Play else ClueInterp.Mistake)
							case None =>
								game.withMeta(f1)(_.copy(status = CardStatus.CalledToPlay))
									.withMeta(list.max)(_.copy(status = CardStatus.CalledToPlay))
									.withMove(if validNI(f1, list.max) then ClueInterp.Play else ClueInterp.Mistake)
					case None =>
						game.withMove(ClueInterp.Mistake)

			lazy val focus = determineFocus(ctx, push = trashPush || clue.kind == ClueKind.Colour)
			lazy val cluedGame = game.withMeta(focus)(_.copy(focused = true)).elim()

			lazy val newPlayables = cluedGame.common.thinksPlayables(cluedGame, target).filter(!prevPlayables.contains(_))
			lazy val loaded = cluedGame.common.thinksLoaded(cluedGame, target)

			val (interp, newGame) =
				if !fix && prevLoaded then
					if newlyTouched.nonEmpty then		// Loaded ref play
						if clue.kind == ClueKind.Rank && stalling then
							Log.info("rank stall!")
							(Some(ClueInterp.Stall), game)
						else if state.numPlayers == 2 && clue.kind == ClueKind.Colour then
							targetPlay(ctx, list.max)
						else
							refPlay(ctx, right = clue.kind == ClueKind.Rank && !trashPush)

					else if stalling then
						Log.info("fill-in stall!")
						(Some(ClueInterp.Stall), game)

					else	// Loaded reclue
						targetPlay(ctx, list.max)

				else if newlyTouched.isEmpty then
					if loaded then
						Log.info(s"revealed a safe action, not continuing")

						val newGame = newPlayables.foldLeft(game): (acc, p) =>
							acc.withMeta(p)(_.signal(state.turnCount))

						(Some(ClueInterp.Reveal), newGame)

					else if stalling then
						Log.info("fill-in stall!")
						(Some(ClueInterp.Stall), game)

					else	// Reclue
						targetPlay(ctx, list.max)

				else if newPlayables.isEmpty && trashPush then
					Log.info(s"trash push!")
					refPlay(ctx)

				else if fix || (loaded && !(clue.kind == ClueKind.Colour && newPlayables.forall(newlyTouched.contains))) then
					Log.info(s"revealed a safe action${if fix then " (fix)" else ""}, not continuing")

					val newGame = cluedGame
						.when(_ => !fix && clue.kind == ClueKind.Rank && newPlayables.forall(newlyTouched.contains)): g =>
							// Playable rank clue
							val focus = newlyTouched.max
							g.withMeta(focus)(_.copy(focused = true))
						.pipe: g =>
							newPlayables.foldLeft(g): (acc, p) =>
								acc.withMeta(p)(_.signal(state.turnCount))

					(Some(ClueInterp.Reveal), newGame)

				else if clue.kind == ClueKind.Colour then
					refPlay(ctx)

				else
					refDiscard(ctx)

			if interp.isEmpty then
				Log.warn("interpreted mistake!")

			newGame.withMove(interp.getOrElse(ClueInterp.Mistake)).elim().check2pPtd(prev, action)

		def interpretDiscard(prev: RefSieve, game: RefSieve, action: DiscardAction): RefSieve =
			val state = game.state
			val DiscardAction(playerIndex, order, suitIndex, rank, failed) = action
			val id = Identity(suitIndex, rank)

			if !failed && prev.state.deck(order).clued && suitIndex != -1 && rank != -1 && state.isUseful(id) then
				interpretUsefulDc(game, action) match
					case DiscardResult.None =>
						game.withMove(DiscardInterp.None)

					case DiscardResult.Mistake =>
						game.withMove(DiscardInterp.Mistake)

					case DiscardResult.GentlemansDiscard(targets) =>
						val target = targets.head
						game.copy(
							common = game.common.withThought(target):
								_.copy(inferred = IdentitySet.single(id))
							,
							meta = game.meta.updated(target, game.meta(target).copy(
								status = CardStatus.GentlemansDiscard
							))
						)
						.withMove(DiscardInterp.GentlemansDiscard)

					case DiscardResult.Sarcastic(orders) =>
						game.copy(
							common = game.common.copy(links = Link.Sarcastic(orders, id) +: game.common.links)
						)
						.withMove(DiscardInterp.Sarcastic)

					case DiscardResult.Baton(_) =>
						throw new Error("baton unsupported!")
			else
				game
			.elim()
			.check2pPtd(prev, action)

		def interpretPlay(prev: RefSieve, game: RefSieve, action: PlayAction): RefSieve =
			game.reinterpPlay(prev, action).getOrElse(game.check2pPtd(prev, action)).elim()

		def takeAction(game: RefSieve): IO[PerformAction] =
			val (state, me) = (game.state, game.me)

			val solveEndgame =
				if state.inEndgame && state.remScore <= state.variant.suits.length + 1 then
					IO.blocking:
						Log.highlight(Console.MAGENTA, "trying to solve endgame...")

						EndgameSolver(monteCarlo = true).solve(game) match
							case Left(err) =>
								Log.info(s"couldn't solve endgame: $err")
								None
							case Right((perform, _)) =>
								Log.info(s"endgame solved!")
								Some(perform)
				else
					IO.pure(None)

			solveEndgame.map: solved =>
				solved.getOrElse:
					val playableOrders = me.thinksPlayables(game, state.ourPlayerIndex)
					Log.info(s"playables $playableOrders")

					val allClues = if !state.canClue then Nil else
						for
							target <- (0 until state.numPlayers) if target != state.ourPlayerIndex
							clue   <- state.allValidClues(target)
						yield
							val perform = PerformAction.fromClue(clue)
							val action = perform.toAction(state, state.ourPlayerIndex)
							(perform, action)

					val allPlays = playableOrders.map: o =>
						val action = PlayAction(state.ourPlayerIndex, o, me.thoughts(o).id(infer = true))
						(PerformAction.Play(o), action)

					val cantDiscard = state.clueTokens == 8 ||
						game.mustClue(state.ourPlayerIndex) ||
						(state.pace == 0 && (allClues.nonEmpty || allPlays.nonEmpty))
					Log.info(s"can discard: ${!cantDiscard}")

					val allDiscards = if cantDiscard then Nil else
						val trash = me.thinksTrash(game, state.ourPlayerIndex)
						val expectedDiscards =
							if trash.nonEmpty then
								trash
							else if (!state.canClue || allPlays.isEmpty) && !me.thinksLocked(game, state.ourPlayerIndex) then
								game.chop(state.ourPlayerIndex).toVector
							else
								Vector.empty
						val discardOrders = (expectedDiscards ++ me.discardable(game, state.ourPlayerIndex)).distinct

						Log.info(s"discardable $discardOrders")
						discardOrders.map: o =>
							val action = DiscardAction(state.ourPlayerIndex, o, me.thoughts(o).id(infer = true))
							(PerformAction.Discard(o), action)

					val allActions = allClues.concat(allPlays).concat(allDiscards)

					if allActions.isEmpty then
						if state.clueTokens == 8 then
							Log.error("No actions available at 8 clues! Playing slot 1")
							PerformAction.Play(state.ourHand.head)
						else
							PerformAction.Discard(me.lockedDiscard(state, state.ourPlayerIndex))
					else
						allActions.maxBy((_, action) => evalAction(game, action))._1

		def updateTurn(game: RefSieve, action: TurnAction): RefSieve =
			val currentPlayerIndex = action.currentPlayerIndex
			val state = game.state

			val nextBlindPlay =
				game.common.thinksPlayables(game, currentPlayerIndex).filter(game.meta(_).signalTurn.isDefined)
					.minByOption(game.meta(_).signalTurn.getOrElse(99))

			nextBlindPlay.fold(game): order =>
				val newInferred = game.common.thoughts(order).inferred.intersect(state.playableSet)
				if newInferred == game.common.thoughts(order).inferred then
					game
				else
					Log.info(s"updating $order to ${newInferred.fmt(state)}")

					if newInferred.isEmpty then
						game.withThought(order)(_.resetInferences())
						.withMeta(order):
							_.copy(status = CardStatus.None, by = None, trash = true)
					else
						game.withThought(order)(_.copy(inferred = newInferred))
			.elim()

		def findAllClues(game: RefSieve, giver: Int) =
			val state = game.state

			val level = Logger.level
			Logger.setLevel(LogLevel.Off)

			var addedUselessClue = false

			def usefulFill(hypo: RefSieve, order: Int) =
				state.deck(order).clued &&
				state.isUseful(state.deck(order).id().get) &&
				hypo.common.thoughts(order).inferred.length < game.common.thoughts(order).inferred.length

			def clueBadness(clue: Clue, target: Int): Int =
				val list = state.clueTouched(state.hands(target), clue)
				val useless = list.forall(o => state.deck(o).clued && state.isBasicTrash(state.deck(o).id().get))

				// Clues that touch only previously-clued trash
				if useless && addedUselessClue then
					return 99

				val hypo = game.simulate(ClueAction(giver, clue.target, state.clueTouched(state.hands(clue.target), clue), clue.base))
				val legal = hypo.lastMove != Some(ClueInterp.Mistake)

				if !legal then
					return 99

				if useless then
					addedUselessClue = true

				// Newly-touched useful card
				if list.exists(o => !state.deck(o).clued && state.isUseful(state.deck(o).id().get)) then
					-2
				// Useful fill-in, or got a playable
				else if list.exists(usefulFill(hypo, _)) || hypo.common.hypoScore > game.common.hypoScore then
					-1
				else
					0

			val allClues =
				(for
					target <- (0 until state.numPlayers) if target != giver
					clue   <- state.allValidClues(target)
					badness = clueBadness(clue, target) if badness < 99
				yield
					(clue, badness))
				.sortBy(_._2)
				.map(t => PerformAction.fromClue(t._1))

			Logger.setLevel(level)
			allClues

		def findAllDiscards(game: RefSieve, playerIndex: Int): List[PerformAction] =
			val trash = game.common.discardable(game, playerIndex)
			val target = trash.headOption
				.orElse(game.chop(playerIndex))
				.getOrElse(game.players(playerIndex).lockedDiscard(game.state, playerIndex))

			List(PerformAction.Discard(target))

		def evalAction(game: RefSieve, action: Action): Double =
			_evalAction(game, action)
