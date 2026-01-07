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
	future: Vector[IdentitySet] = Vector(),
	catchup: Boolean = false,
	notes: Map[Int, Note] = Map(),
	lastMove: Option[Interp] = None,
	queuedCmds: List[(String, String)] = Nil,
	nextInterp: Option[Interp] = None,
	noRecurse: Boolean = false,
	rewindDepth: Int = 0,
	inProgress: Boolean = false,

	goodTouch: Boolean = false,
	zcsTurn: Option[Int] = None
) extends Game:
	def chop(playerIndex: Int) =
		state.hands(playerIndex).find:
			meta(_).status == CardStatus.CalledToDiscard
		.orElse:
			state.hands(playerIndex).find: order =>
				zcsTurn.forall(_ >= state.deck(order).drawnIndex) &&
				!state.deck(order).clued &&
				meta(order).status == CardStatus.None

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

	def apply(tableID: Int, state: State, inProgress: Boolean) =
		init(tableID, state, inProgress, genPlayers(state))

	private def checkMissed(game: Reactor, playerIndex: Int, actionOrder: Int) =
		game.state.hands(playerIndex)
			.find(o => game.meta(o).urgent && o != actionOrder)
			.fold(game): urgent =>
				val newCommon = game.common.withThought(urgent)(t => t.copy(
					inferred = t.oldInferred.getOrElse(throw new Exception(s"No old inferred on $urgent!")),
					oldInferred = IdentitySetOpt.empty,
					infoLock = IdentitySetOpt.empty
				))
				val newMeta = game.meta.updated(urgent,
					game.meta(urgent).cleared.reason(game.state.turnCount))

				game.copy(common = newCommon, meta = newMeta)

	private def resetZcs(game: Reactor) =
		game.copy(zcsTurn = None)

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
				rewindDepth = updates.rewindDepth.getOrElse(game.rewindDepth),
				inProgress = updates.inProgress.getOrElse(game.inProgress),
				noRecurse = updates.noRecurse.getOrElse(game.noRecurse)
			)

		def blank(game: Reactor, keepDeck: Boolean) =
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

		def interpretClue(prev: Reactor, game: Reactor, action: ClueAction): Reactor =
			val state = game.state
			val ClueAction(giver, target, _, _) = action

			val interpretedGame = checkMissed(game, giver, 99)
				.pipe: g =>
					g.waiting match
						case Some(wc) if wc.reacter == giver => g.copy(waiting = None)
						case _ => g
				.pipe: g =>
					val (interp, interpGame) = g.nextInterp match
						case Some(interp) =>
							Log.info(s"forcing rewinded interp $interp!")
							if interp == ClueInterp.Reactive then
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

								if playables.isEmpty then
									Log.info(s"reacter is ${state.names(playerIndex)}")
									Some(playerIndex)
								else
									Log.info(s"${state.names(playerIndex)} has playables $playables, not reacter")
									None
							}.find(_.isDefined).flatten

							val fixed = checkFix(prev, g, action) match
								case FixResult.Normal(cluedResets, duplicateReveals) =>
									cluedResets ++ duplicateReveals
								case _ => Nil
							val allowableFix = target == state.nextPlayerIndex(giver) && fixed.nonEmpty

							reacter match
								case None => (Option.when(allowableFix)(ClueInterp.Fix), g)

								case Some(reacter) if reacter == target =>
									interpretStable(prev, g, action, stall = false)

								case Some(reacter) =>
									val prevPlayables = prev.players(target).obviousPlayables(prev, target)

									// Urgent fix on previous playable
									if allowableFix && fixed.exists(prevPlayables.contains) then
										(Some(ClueInterp.Fix), g)
									else
										interpretReactive(prev, g, action, reacter, inverted = false)

					if interp.isEmpty then
						Log.warn("interpreted mistake!")

					interpGame.copy(lastMove = Some(interp.getOrElse(ClueInterp.Mistake)))

			val signalledPlays = interpretedGame.state.hands.flatten.filter: o =>
				prev.meta(o).status != CardStatus.CalledToPlay && interpretedGame.meta(o).status == CardStatus.CalledToPlay

			val eliminatedGame = interpretedGame.elim
			val playsAfterElim = eliminatedGame.state.hands.flatten.filter(eliminatedGame.meta(_).status == CardStatus.CalledToPlay)

			eliminatedGame
				.when(_ => playsAfterElim.length < signalledPlays.length): g =>
					Log.warn(s"lost play signal on ${signalledPlays.filterNot(playsAfterElim.contains)} after elim!")
					g.copy(lastMove = Some(ClueInterp.Mistake))
				.when(_ => prev.state.canClue):
					resetZcs
				.when(!_.state.canClue):
					_.copy(zcsTurn = Some(state.turnCount))
				.copy(nextInterp = None)

		def interpretDiscard(prev: Reactor, game: Reactor, action: DiscardAction): Reactor =
			val state = game.state
			val DiscardAction(playerIndex, order, suitIndex, rank, failed) = action
			val id = Identity(suitIndex, rank)

			checkMissed(game, playerIndex, order)
				.when(_ => failed): g =>
					Log.warn("bombed! clearing all information")

					val initial = (g.common, g.meta)
					val (clearedC, clearedM) = state.hands.flatten.foldLeft(initial) { case ((c, m), order) =>
						val newC = c.withThought(order)(t => t.copy(
							inferred = t.possible,
							oldInferred = IdentitySetOpt.empty,
							infoLock = IdentitySetOpt.empty,
						))
						val newM = m.updated(order, m(order).cleared)
						(newC, newM)
					}
					g.copy(
						waiting = None,
						common = clearedC,
						meta = clearedM
					)
				.pipe: g =>
					lazy val usefulDc = !failed && prev.state.deck(order).clued &&
						suitIndex != -1 && rank != -1 &&
						!state.isBasicTrash(id) &&
						prev.meta(order).status != CardStatus.CalledToDiscard &&
						!(prev.common.thinksLocked(prev, playerIndex) && prev.state.clueTokens == 0)

					g.waiting match
						case Some(wc) =>
							reactDiscard(prev, g, playerIndex, order, wc)

						case None if usefulDc  =>
							interpretUsefulDc(g, action) match
								case DiscardResult.None =>
									g.copy(lastMove = Some(DiscardInterp.None))

								case DiscardResult.Mistake =>
									g.copy(lastMove = Some(DiscardInterp.Mistake))

								case DiscardResult.GentlemansDiscard(target) =>
									g.copy(
										common = g.common.withThought(target)(_.copy(
											inferred = IdentitySet.single(id)
										)),
										meta = g.meta.updated(target, g.meta(target).copy(
											status = CardStatus.GentlemansDiscard
										)),
										lastMove = Some(DiscardInterp.GentlemansDiscard)
									)
								case DiscardResult.Sarcastic(orders) =>
									g.copy(
										common = g.common.copy(
											links = Link.Sarcastic(orders, id) +: g.common.links
										),
										lastMove = Some(DiscardInterp.Sarcastic)
									)
						case None => g
				.elim
				.when(_ => prev.state.canClue)(resetZcs)

		def interpretPlay(prev: Reactor, game: Reactor, action: PlayAction): Reactor =
			val PlayAction(playerIndex, order, _, _) = action

			checkMissed(game, playerIndex, order)
				.pipe: g =>
					g.waiting match
						case Some(wc) => reactPlay(prev, g, playerIndex, order, wc)
						case None => g
				.elim
				.when(_ => prev.state.canClue)(resetZcs)

		def updateTurn(game: Reactor, action: TurnAction): Reactor =
			val currentPlayerIndex = action.currentPlayerIndex
			val state = game.state

			if currentPlayerIndex == -1 then
				game
			else
				val waitedGame = game.waiting match
					case Some(wc) if wc.reacter == state.lastPlayerIndex(currentPlayerIndex) =>
						game.copy(waiting = None)
					case _ => game

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

				waitedGame.copy(common = newCommon, meta = newMeta)
					.elim

		def takeAction(game: Reactor): PerformAction =
			val (state, me) = (game.state, game.me)

			state.ourHand.find(game.meta(_).urgent) match
				case Some(urgent) => game.meta(urgent).status match
					case CardStatus.CalledToPlay if !me.thoughts(urgent).possible.forall(state.isBasicTrash) =>
						return PerformAction.Play(urgent)
					case CardStatus.CalledToDiscard =>
						return PerformAction.Discard(urgent)
					case _ =>
						Log.warn(s"Unexpected urgent card status ${game.meta(urgent).status}")
				case _ => ()

			if state.inEndgame && state.remScore <= state.variant.suits.length + 1 then
				Log.highlight(Console.MAGENTA, "trying to solve endgame...")

				EndgameSolver(monteCarlo = true).solve(game) match
					case Left(err) => Log.info(s"couldn't solve endgame: $err")
					case Right((perform, _)) =>
						Log.info(s"endgame solved!")
						return perform

			val playableOrders =
				val commonP = game.common.obviousPlayables(game, state.ourPlayerIndex)
				val knownP = me.obviousPlayables(game, state.ourPlayerIndex)

				// If there is are commonly known playables that might connect to the reacter,
				// we must play the oldest one.
				val possibleConnectors =
					if commonP.nonEmpty && game.waiting.exists(_.receiver == state.ourPlayerIndex) then
						val reacter = game.waiting.get.reacter
						commonP.filter: p =>
							me.thoughts(p).inferred.exists:
								_.next.exists: id =>
									state.hands(reacter).exists: o =>
										me.thoughts(o).matches(id)
					else Nil

				if possibleConnectors.nonEmpty then
					Log.highlight(Console.CYAN, s"restricting playables to $possibleConnectors, since reverse reacter might need to connect")
					Vector(possibleConnectors.minBy(game.meta(_).signalTurn.getOrElse(99)))

				else if knownP.nonEmpty then
					// Don't play if there is a focused card that looks exactly like this one (no OCM in reactor)
					knownP.filter: order =>
						game.meta(order).status == CardStatus.CalledToPlay ||
						!state.hands(state.ourPlayerIndex).exists: o =>
							o != order && me.thoughts(o).possible == me.thoughts(order).possible && game.meta(o).focused
				else
					me.thinksPlayables(game, state.ourPlayerIndex)

			Log.info(s"playables $playableOrders")

			val canClue = state.canClue && !game.waiting.exists(_.receiver == state.ourPlayerIndex)

			val allClues = if !canClue then Nil else
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

			val potentialReacter = state.nextPlayerIndex(state.ourPlayerIndex)

			// We have a play and the reacter might play on top of us
			lazy val potentialForcedPlay = allPlays.nonEmpty &&
				game.waiting.exists(_.reacter == potentialReacter) &&
				playableOrders.exists:
					game.me.thoughts(_).inferred.exists: id =>
						state.hands(potentialReacter).exists: o =>
							id.next.contains(state.deck(o).id().get)

			val cantDiscard = state.clueTokens == 8 ||
				(state.pace == 0 && (allClues.nonEmpty || allPlays.nonEmpty)) ||
				potentialForcedPlay
			Log.info(s"can discard: ${!cantDiscard} ${state.clueTokens}")

			val allDiscards: Seq[(PerformAction, Action)] = if cantDiscard then Nil else
				val trash = me.thinksTrash(game, state.ourPlayerIndex)
				val expectedDiscards =
					if trash.nonEmpty then
						trash
					else if (!state.canClue || allPlays.isEmpty) && !me.obviousLocked(game, state.ourPlayerIndex) then
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

		def findAllClues(game: Reactor, giver: Int) =
			val state = game.state

			val level = Logger.level
			Logger.setLevel(LogLevel.Off)

			def validClue(clue: Clue, target: Int) =
				val list = state.clueTouched(state.hands(target), clue)
				val action = ClueAction(giver, target, list, clue.toBase)

				// Do not simulate clues that touch only previously-clued trash
				if list.forall(o => state.deck(o).clued && state.isBasicTrash(state.deck(o).id().get)) then
					false
				else
					Log.highlight(Console.GREEN, s"===== Predicting value for ${clue.fmt(state)} =====")
					val hypoGame = game.simulateClue(action, log = true)
					getResult(game, hypoGame, action) > -1

			val allClues =
				(for
					target <- (0 until state.numPlayers) if target != giver
					clue   <- state.allValidClues(target) if validClue(clue, target)
				yield
					clue)
				.sortBy: clue =>
					val list = state.clueTouched(state.hands(clue.target), clue)
					val nonTrash = list.filterNot(o => state.isBasicTrash(state.deck(o).id().get))

					// Prefer not cluing trash, previously clued cards, and no-info clues
					if nonTrash.isEmpty then
						99
					else
						-nonTrash.count(!state.deck(_).clued) * 5 +					// unclued non-trash cards
						-nonTrash.count(!state.deck(_).clues.exists(_.isEq(clue)))	// fill-ins
				.map(clueToPerform)

			Logger.setLevel(level)
			allClues

		def findAllDiscards(game: Reactor, playerIndex: Int): List[PerformAction] =
			val trash = game.common.thinksTrash(game, playerIndex)
			val expectedDiscards = if trash.nonEmpty then trash else game.chop(playerIndex).toVector

			val target = expectedDiscards.headOption.getOrElse:
				game.players(playerIndex).lockedDiscard(game.state, playerIndex)

			List(PerformAction.Discard(target))
