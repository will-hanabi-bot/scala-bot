package scala_bot.refSieve

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
				lastMove = updates.lastMove.getOrElse(game.lastMove),
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

		def interpretClue(prev: RefSieve, game: RefSieve, action: ClueAction): RefSieve =
			val state = game.state
			val ClueAction(giver, target, list, clue) = action
			val newlyTouched = list.filter(!prev.state.deck(_).clued)
			val ctx = ClueContext(prev, game, action)

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

			val noInfo = state.numPlayers == 2 && !stalling && list.forall: o =>
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
									.copy(lastMove = if validNI(f1, f2) then Some(ClueInterp.Play) else Some(ClueInterp.Mistake))
							case None =>
								game.withMeta(f1)(_.copy(status = CardStatus.CalledToPlay))
									.withMeta(list.max)(_.copy(status = CardStatus.CalledToPlay))
									.copy(lastMove = if validNI(f1, list.max) then Some(ClueInterp.Play) else Some(ClueInterp.Mistake))
					case None =>
						game.copy(lastMove = Some(ClueInterp.Mistake))

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

			newGame.copy(lastMove = Some(interp.getOrElse(ClueInterp.Mistake))).elim()

		def interpretDiscard(prev: RefSieve, game: RefSieve, action: DiscardAction): RefSieve =
			val state = game.state
			val DiscardAction(playerIndex, order, suitIndex, rank, failed) = action
			val id = Identity(suitIndex, rank)

			if !failed && prev.state.deck(order).clued && suitIndex != -1 && rank != -1 && !state.isBasicTrash(id) then
				interpretUsefulDc(game, action) match
					case DiscardResult.None =>
						game.copy(lastMove = Some(DiscardInterp.None))

					case DiscardResult.Mistake =>
						game.copy(lastMove = Some(DiscardInterp.Mistake))

					case DiscardResult.GentlemansDiscard(targets) =>
						val target = targets.head
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
					case DiscardResult.Baton(_) =>
						throw new Error("baton unsupported!")
			else
				game
			.elim()

		def interpretPlay(prev: RefSieve, game: RefSieve, action: PlayAction): RefSieve =
			game.reinterpPlay(prev, action).getOrElse(game).elim()

		def takeAction(game: RefSieve): PerformAction =
			val (state, me) = (game.state, game.me)

			if state.inEndgame && state.remScore <= state.variant.suits.length + 1 then
				Log.highlight(Console.MAGENTA, "trying to solve endgame...")

				EndgameSolver(monteCarlo = true).solve(game) match
					case Left(err) => Log.info(s"couldn't solve endgame: $err")
					case Right((perform, _)) =>
						Log.info(s"endgame solved!")
						return perform

			val playableOrders = me.thinksPlayables(game, state.ourPlayerIndex)
			Log.info(s"playables $playableOrders")

			val allClues = if !state.canClue then Nil else
				for
					target <- (0 until state.numPlayers) if target != state.ourPlayerIndex
					clue   <- state.allValidClues(target)
				yield
					val perform = clueToPerform(clue)
					val action = performToAction(state, perform, state.ourPlayerIndex)
					(perform, action)

			val allPlays = playableOrders.map: o =>
				val action = PlayAction(state.ourPlayerIndex, o, me.thoughts(o).id(infer = true))
				(PerformAction.Play(o), action)

			val cantDiscard = state.clueTokens == 8 ||
				game.mustClue(state.ourPlayerIndex) ||
				(state.pace == 0 && (allClues.nonEmpty || allPlays.nonEmpty))
			Log.info(s"can discard: ${!cantDiscard} ${state.clueTokens}")

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
					return PerformAction.Play(state.ourHand.head)
				else
					return PerformAction.Discard(me.lockedDiscard(state, state.ourPlayerIndex))

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

			def validClue(clue: Clue, target: Int): Boolean =
				val list = state.clueTouched(state.hands(target), clue)
				val useless = list.forall(o => state.deck(o).clued && state.isBasicTrash(state.deck(o).id().get))

				// Do not simulate clues that touch only previously-clued trash
				if useless && addedUselessClue then
					return false

				val hypo = game.simulate(ClueAction(giver, clue.target, state.clueTouched(state.hands(clue.target), clue), clue.base))
				val legal = hypo.lastMove != Some(ClueInterp.Mistake)

				if !legal then
					return false

				if useless then
					addedUselessClue = true

				true

			val allClues =
				for
					target <- (0 until state.numPlayers) if target != giver
					clue   <- state.allValidClues(target) if validClue(clue, target)
				yield
					clueToPerform(clue)

			Logger.setLevel(level)
			allClues

		def findAllDiscards(game: RefSieve, playerIndex: Int): List[PerformAction] =
			val trash = game.common.discardable(game, playerIndex)
			val target = trash.headOption
				.orElse(game.chop(playerIndex))
				.getOrElse(game.players(playerIndex).lockedDiscard(game.state, playerIndex))

			List(PerformAction.Discard(target))
