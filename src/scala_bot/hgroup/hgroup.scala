package scala_bot.hgroup

import cats.effect.IO

import scala_bot.basics._
import scala_bot.endgame.EndgameSolver
import scala_bot.utils._
import scala_bot.logger.{Log, Logger, LogLevel}
import scala_bot.fraction.Frac

case class FocusResult(
	focus: Int,
	chop: Boolean = false,
	positional: Boolean = false
)

enum StallInterp:
	case Stall5, Tempo, Locked, SaveLHS, FillIn, Clues8, Burn

enum DcStatus:
	case None, Scream, Shout, Generation

object Level:
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
	val IntermediateBluffs = 13
	val TrashMoves = 14

enum FStatus:
	/** The card may be part of an *Ambiguous Finesse* on the player. */
	case PossiblyAmbiguous(to: Int)
	/** The card may or may not be playing, according to the player. */
	case PossiblyOn(to: Int)

case class XConvData(
	/** This card may be a different identity than expected (e.g. if it ended up being *Layered*). */
	idUncertain: Boolean = false,
	fStatus: Seq[FStatus] = Nil,
	turnFinessed: Option[Int] = None,
	/** The valid identities of the card that were promised when it was finessed, if they exist. */
	finesseIds: Option[IdentitySet] = None
)

case class HGroup(
	tableID: Int,
	state: State,
	players: Vector[Player],
	common: Player,
	base: (State, Vector[ConvData], Vector[Player], Player),
	lastActions: Vector[Option[Action]],
	importantAction: Vector[Boolean],

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

	level: Int = 1,
	waiting: List[WaitingConnection] = Nil,
	stalled5: Boolean = false,
	cluedOnChop: Set[Int] = Set(),
	dcStatus: DcStatus = DcStatus.None,
	dda: Option[Identity] = None,
	inEarlyGame: Boolean = true,
	stallInterp: Option[StallInterp] = None,
	xmeta: Vector[XConvData] = Vector(),

	allowFindOwn: Boolean = true,
	assumePlays: Boolean = true
) extends Game:
	val goodTouch = true

	override def filterPlayables(player: Player, playerIndex: Int, orders: Seq[Int], assume: Boolean = true) =
		val ordered1s = this.order1s(orders.filter(this.unknown1))

		orders.filter: o =>
			// if player.orderKp(this, o) then
			// 	true
			val possibleFocusDupe =
				!ordered1s.contains(o) &&
				!this.knownAs(o, PINKISH) &&
				!meta(o).focused &&
				state.deck(o).clued &&
				state.hands(playerIndex).exists: o2 =>
					meta(o2).focused &&
					player.thoughts(o).inferred == player.thoughts(o2).inferred &&
					state.deck(o).clues.forall(clue => state.deck(o2).clues.exists(_.isEq(clue)))

			val olderFinesseExists = state.hands(playerIndex).exists: o2 =>
				// An older finesse exists which could be swapped with this identity
				xmeta(o2).idUncertain &&
				xmeta(o2).turnFinessed.exists(t2 => xmeta(o).turnFinessed.exists(_ > t2)) &&
				xmeta(o2).finesseIds.exists(ids => player.thoughts(o).id(infer = true).exists(ids.contains))

			val unrevealedHidden = waiting.exists: wc =>
				!wc.symmetric && !wc.ambiguousSelf &&
				// This is part of a hidden (?) connection that is not currently revealed
				wc.connections.nonEmpty && {
					wc.currConn.hidden && wc.connections.tail.exists(_.order == o) ||
					wc.connections.tail.exists(c => c.order == o && c.hidden)
				}

			val unordered1 = (state.includesVariant(PINKISH) || this.level < Level.BasicCM) && ordered1s.nonEmpty && ordered1s.tail.contains(o)

			((assume && !xmeta(o).fStatus.contains(FStatus.PossiblyOn(state.ourPlayerIndex))) || isDefinite(o)) &&
			!possibleFocusDupe &&
			!olderFinesseExists &&
			!unrevealedHidden &&
			!unordered1 &&
			!waiting.find(_.connections.exists(_.order == o)).exists(this.potentialClandestineWc(playerIndex, o, _).isDefined)

	override def validArr(id: Identity, order: Int): Boolean =
		val playables = this.me.thinksPlayables(this, state.ourPlayerIndex)

		if playables.contains(order) then
			state.isPlayable(id)
		else if this.isTouched(order) && !this.common.thoughts(order).reset then
			val good = this.me.thoughts(order).possible.difference(state.trashSet)
			good.isEmpty || good.contains(id)
		else
			true

	def withXMeta(order: Int)(f: XConvData => XConvData) =
		copy(xmeta = xmeta.updated(order, f(xmeta(order))))

	def isCM(order: Int) =
		meta(order).cm && !state.deck(order).clued

	def isDefinite(order: Int) =
		xmeta(order).fStatus.forall: f =>
			f != FStatus.PossiblyAmbiguous(state.ourPlayerIndex) &&
			f != FStatus.PossiblyOn(state.ourPlayerIndex)

	def chop(playerIndex: Int) =
		state.hands(playerIndex).findLast: o =>
			!state.deck(o).clued &&
			(meta(o).status == CardStatus.None || !isDefinite(o))
		// If no chop seems to exist on someone other than us and they have hidden finesses to play, return the last finessed card instead.
		.when(_.isEmpty && playerIndex != state.ourPlayerIndex && state.hands(playerIndex).exists(o => meta(o).status == CardStatus.Finessed && meta(o).hidden)): _ =>
			state.hands(playerIndex).findLast: o =>
				!state.deck(o).clued &&
				meta(o).status == CardStatus.Finessed

	/** Returns how far a card is from chop. A card on chop is 0-away. */
	def chopDistance(playerIndex: Int, order: Int) =
		chop(playerIndex) match
			case None =>
				throw new IllegalArgumentException(s"${state.names(playerIndex)} has no chop!")
			case Some(c) =>
				if order < c then
					throw new IllegalArgumentException(s"order $order is right of chop $c!")

				state.hands(playerIndex).count: o =>
					o < order && o >= c &&
					!state.deck(o).clued &&
					meta(o).status == CardStatus.None

	def mustClue(playerIndex: Int) =
		val bob = state.nextPlayerIndex(playerIndex)

		state.canClue &&
		state.numPlayers > 2 &&
		!common.thinksLoaded(this, bob) &&
		chop(bob).flatMap(state.deck(_).id()).exists(state.isCritical)

	def invalidFocus(giver: Int, clue: ClueLike, id: Identity, focusResult: FocusResult) =
		val FocusResult(focus, _, positional) = focusResult

		state.isBasicTrash(id) ||
		(state.includesVariant(PINKISH) && !positional && clue.kind == ClueKind.Rank && clue.value != id.rank) ||
		visibleFind(state, common, id, infer = true, excludeOrder = focus).nonEmpty ||
		common.links.existsM:
			case Link.Promised(_, i, target) => target != focus && id.matches(i)
		||
		waiting.exists: wc =>
			!wc.symmetric && !wc.ambiguousSelf && wc.target == giver &&
			common.thoughts(wc.focus).inferred.contains(id) &&
			wc.connections.exists: conn =>
				xmeta(conn.order).fStatus.contains(FStatus.PossiblyOn(giver))

	def findFinesse(playerIndex: Int, connected: Set[Int] = Set(), ignore: Set[Int] = Set()): Option[Int] =
		val order = state.hands(playerIndex).find: o =>
			val card = state.deck(o)
			val status = this.meta(o).status

			!card.clued &&
			!this.meta(o).cm &&
			!connected.contains(o) &&
			(status != CardStatus.Finessed || this.xmeta(o).fStatus.contains(FStatus.PossiblyOn(state.ourPlayerIndex)))

		order.filter(!ignore.contains(_))

	def findFinesseId(playerIndex: Int, id: Identity, connected: Set[Int] = Set(), ignore: Set[Int] = Set(), overrideLayer: Boolean = false): Option[Int] =
		val order = state.hands(playerIndex).find: o =>
			val card = state.deck(o)
			val status = this.meta(o).status

			!card.clued &&
			!connected.contains(o) && {
				if level < Level.IntermediateFinesses then
					status != CardStatus.Finessed || this.xmeta(o).fStatus.contains(FStatus.PossiblyOn(state.ourPlayerIndex))
				else
					(overrideLayer || !(status == CardStatus.Finessed && !this.xmeta(o).finesseIds.get.contains(id)))
			}

		order.filter(!ignore.contains(_))

	def unknown1(order: Int) =
		val clues = state.deck(order).clues

		clues.nonEmpty && clues.forall(_.isEq(BaseClue(ClueKind.Rank, 1)))

	def order1s(orders: Seq[Int]) =
		orders.sortBy: o =>
			if cluedOnChop.contains(o) then
				-100 - o
			else if meta(o).cm then
				100 - o
			else if state.inStartingHand(o) then
				o
			else
				-o

	def priority(orders: List[Int]) =
		val initial = (0 to 5).map(_ => Vector.empty[Int])
		orders.foldLeft(initial): (acc, o) =>
			val thought = this.me.thoughts(o)

			val inFinesse = this.isBlindPlaying(o)	// TODO: play link?
			lazy val unknownCM = isCM(o) &&
				!state.deck(o).clued &&
				thought.possible.exists(!state.isPlayable(_))

			def connecting(playerIndex: Int, id: Identity) =
				state.hands(playerIndex).exists: o =>
					id.next.exists: i =>
						this.me.thoughts(o).matches(i, infer = true)

			lazy val connectsTo = (0 until state.numPlayers).filter: playerIndex =>
				thought.possibilities.exists(i => connecting(playerIndex, i))

			val priorityIndex =
				if inFinesse then
					0
				else if unknownCM then
					// Don't blind play CM cards at 2 strikes
					if state.strikes == 2 then -1 else 5
				else if connectsTo.exists(_ != state.ourPlayerIndex) then
					1
				else if connectsTo.nonEmpty then
					2
				else if thought.possibilities.forall(_.rank == 5) then
					3
				else if thought.possibilities.length > 1 then
					4
				else
					5

			if priorityIndex == -1 then
				acc
			else
				(acc.updated(priorityIndex, acc(priorityIndex) :+ o))
		.pipe: ps =>
			// Speed-up clues first, then oldest finesse to newest
			ps.updated(0, ps(0).sortBy: o =>
				if this.isBlindPlaying(o) then
					-200 - o
				else if state.deck(o).clued then
					-100 - o
				else if meta(o).hidden then
					-o
				else
					o
			)
			// Lowest rank, then leftmost
			.updated(5, ps(5).sortBy: o =>
				this.me.thoughts(o).possibilities.map(_.rank).min * 100 - o
			)

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
			state.variant.colourableSuits(clue.value).name.contains("Brown") &&
			reclue

		lazy val muddySuitIndex = state.variant.suits.indexWhere(suit => MUDDY.matches(suit.name))
		lazy val muddyCards = list.filter(this.knownAs(_, MUDDY))
		lazy val mudClue = clue.kind == ClueKind.Colour &&
			state.includesVariant(MUDDY) &&
			muddyCards.nonEmpty &&
			reclue &&
			// Mud clues should only work if the leftmost card is muddy.
			common.thoughts(list.max).possible.exists(_.suitIndex == muddySuitIndex)

		lazy val pinkStall5 =
			val valid = clue.isEq(BaseClue(ClueKind.Rank, 5)) &&
				state.includesVariant(PINKISH) &&
				stallSeverity(prev, prev.common, giver) > 0

			if !valid then None else
				list.filter(o => !prev.state.deck(o).clued && !prev.isCM(o)).minOption

		if chop.exists(list.contains) then
			FocusResult(chop.get, chop = true)

		else if pinkChoiceTempo then
			FocusResult(hand(clue.value - 1), positional = true)

		else if brownTempo then
			FocusResult(list.min, positional = true)

		else if clue.isEq(BaseClue(ClueKind.Rank, 1)) then
			// Custom implementation of ordered1s: chop is already covered above
			val focus = list.minBy: o =>
				if meta(o).cm then
					100 - o
				else if state.inStartingHand(o) then
					o
				else
					-o

			FocusResult(focus)

		else if mudClue then
			val coloursAvailable = state.variant.colourableSuits.length
			val focusIndex = (clue.value - coloursAvailable + 6*muddyCards.length) % muddyCards.length
			FocusResult(muddyCards(focusIndex), positional = true)

		else if pinkStall5.isDefined then
			FocusResult(pinkStall5.get)

		else
			val sortedList = list.sortBy(o => -o)
			val focus =
				sortedList.find(o => !prev.state.deck(o).clued && prev.meta(o).status == CardStatus.None)
				.orElse(sortedList.find(prev.isCM))
				.orElse(sortedList.headOption)

			focus match
				case Some(order) => FocusResult(order)
				case None        => throw new Error("No focus found!")

	def earlyGameClue(giver: Int): Option[Clue] =
		if noRecurse || !inEarlyGame || !state.canClue then
			return None

		val allClues = for
			target <- (0 until state.numPlayers).view if target != giver
			clue   <- state.allValidClues(target)
		yield
			clue

		allClues.find: clue =>
			val action = Action.fromClue(state, clue, giver)
			val focus = determineFocus(this, action).focus
			val focusCard = state.deck(focus)
			val focusId = focusCard.id().get

			meta(focus).status != CardStatus.Finessed &&
			!focusCard.clued &&
			state.isUseful(focusId) && {
				val hypo = this.copy(noRecurse = true, allowFindOwn = false).simulateClue(action)

				hypo.lastMove.matchesP:
					case Some(ClueInterp.Save) =>
						state.isCritical(focusId) || dupeResponsibility(this, focusId, action.target).contains(giver)

					case Some(ClueInterp.Stall) =>
						hypo.stallInterp == Some(StallInterp.Stall5) &&
						!stalled5

					case Some(ClueInterp.Play) =>
						val (badTouch, _, _) = badTouchResult(this, hypo, action)
						badTouch.isEmpty
			}

	def findDiscardable(playerIndex: Int) =
		state.hands(playerIndex).filter: o =>
			this.me.orderTrash(this, o) || {
				this.isTouched(o) &&
				this.me.thoughts(o).inferred.forall: id =>
					visibleFind(state, this.me, id, excludeOrder = o).nonEmpty
				&&
				!waiting.find(_.connections.exists(_.order == o)).exists(this.potentialClandestineWc(playerIndex, o, _).isDefined)
			}

	/** Returns a potential clandestine finesse that may alter the id of the card with the given order.
	  * @example See test "waits for a clandestine finesse to resolve".
	  */
	def potentialClandestineWc(playerIndex: Int, order: Int, containingWc: WaitingConnection) =
		waiting.find: wc =>
			!wc.symmetric && !wc.ambiguousSelf &&
			wc.focus == containingWc.focus &&
			wc.target == playerIndex &&
			wc.currConn.matchesP:
				case f: FinesseConn => f.order != order && f.reacting != state.holderOf(wc.focus)

	def reinterpPlay(prev: HGroup, action: PlayAction | DiscardAction): Option[HGroup] =
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
			copy(
				future = future.updated(order, IdentitySet.single(Identity(suitIndex, rank)))
			)
			.replay
			.toOption
		}.flatten

	/** Removes the 'idUncertain' flag if no longer applicable. */
	def refreshUncertain =
		val uncertain = (order: Int) =>
			this.me.thoughts(order).possible.length > 1 &&
			// There's an older card in our hand that allows for a swap
			this.me.thoughts(order).inferred.exists: i =>
				state.ourHand.exists: o =>
					o < order && this.me.thoughts(o).possible.contains(i)

		val newlyCertains = state.ourHand.filter(o => xmeta(o).idUncertain && !uncertain(o))

		newlyCertains.foldLeft(this): (acc, o) =>
			acc.copy(xmeta = xmeta.updated(o, xmeta(o).copy(idUncertain = false)))

	def resetImportant(playerIndex: Int) =
		copy(importantAction = importantAction.updated(playerIndex, false))

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
		deckIds = Vector.fill(state.variant.totalCards)(None),
		future = Vector.fill(state.variant.totalCards)(state.allIds),
		inProgress = inProgress,
		lastActions = Vector.fill(state.numPlayers)(None),
		importantAction = Vector.fill(state.numPlayers)(false),
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
				lastActions = updates.lastActions.getOrElse(game.lastActions),
				moveHistory = updates.moveHistory.getOrElse(game.moveHistory),
				queuedCmds = updates.queuedCmds.getOrElse(game.queuedCmds),
				nextInterp = updates.nextInterp.getOrElse(game.nextInterp),
				rewindDepth = updates.rewindDepth.getOrElse(game.rewindDepth),
				inProgress = updates.inProgress.getOrElse(game.inProgress),
				noRecurse = updates.noRecurse.getOrElse(game.noRecurse),

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
				inProgress = game.inProgress,

				level = game.level,
				deckIds = if keepDeck then game.deckIds else Vector.fill(game.state.variant.totalCards)(None),
				future = if keepDeck then game.future else Vector.fill(game.state.variant.totalCards)(game.state.allIds),
				lastActions = Vector.fill(game.state.numPlayers)(None),
				importantAction = Vector.fill(game.state.numPlayers)(false),
				xmeta = Vector.fill(game.base._2.length)(XConvData())
			)

		def interpretClue(prev: HGroup, game: HGroup, action: ClueAction): HGroup =
			val updatedGame = game.refreshUncertain.elim()

			checkRevealLayer(prev, updatedGame, action).getOrElse:
				val pre = refreshWCs(prev, updatedGame, action, beforeClueInterp = true)
					.resetImportant(action.playerIndex)

				def lostWcs(p: HGroup, g: HGroup) =
					p.waiting.exists: wc =>
						!wc.symmetric && !wc.ambiguousSelf &&
						!g.waiting.exists(w => w.inference == wc.inference && w.turn == wc.turn)

				if !game.allowFindOwn && lostWcs(prev, pre) then
					Log.warn("removed wc! mistake")
					pre.withMove(ClueInterp.Mistake)
				else
					val interpreted = interpClue(ClueContext(prev, pre, action))
					val updatedPre = pre.copy(importantAction = interpreted.importantAction)
						.withMove(interpreted.lastMove.get)

					refreshWCs(prev, updatedPre, action)
						.cond(_.waiting.length < pre.waiting.length && !game.noRecurse) { g =>
							Log.highlight(Console.GREEN, "----- REINTERPRETING CLUE -----")
							val res = interpClue(ClueContext(prev, g, action))
							Log.highlight(Console.GREEN, "----- DONE REINTERPRETING -----")
							res
						} {
							_ => interpreted
						}
						.pipe: g =>
							g.copy(
								dcStatus = DcStatus.None,
								dda = None,
								stallInterp = if g.lastMove == Some(ClueInterp.Stall) then g.stallInterp else None
							)
			.elim()

		def interpretDiscard(prev: HGroup, game: HGroup, action: DiscardAction): HGroup =
			val DiscardAction(playerIndex, order, suitIndex, rank, failed) = action

			val updatedGame = game.refreshUncertain.elim()
			val reinterpGame = if !failed then None else updatedGame.reinterpPlay(prev, action)

			if reinterpGame.isDefined then
				return reinterpGame.get

			val refreshedGame = refreshWCs(prev, updatedGame, action)
				.resetImportant(action.playerIndex)

			val ctx = DiscardContext(prev, refreshedGame, action)

			interpretBombOcm(ctx)
			.orElse(interpretUsefulDcH(ctx))
			.orElse(interpretSdcm(ctx))
			.orElse(interpretPosDc(ctx))
			.getOrElse:
				refreshedGame.copy(
					dcStatus = DcStatus.None,
					dda = Some(Identity(suitIndex, rank))
				)
				.withMove(DiscardInterp.None)
			.when(_.inEarlyGame): g =>
				val endEarlyGame = !failed &&
					!game.state.deck(order).clued &&
					game.meta(order).status == CardStatus.None

				g.copy(inEarlyGame = !endEarlyGame)
			.elim()
			.when(g => g.level < Level.Stalling || g.dda.exists(id => id.suitIndex == -1 || id.rank == -1 || g.state.isBasicTrash(id))):
				_.copy(dda = None)

		def interpretPlay(prev: HGroup, game: HGroup, action: PlayAction): HGroup =
			val PlayAction(playerIndex, order, suitIndex, rank) = action

			val updatedGame = game.refreshUncertain

			updatedGame.reinterpPlay(prev, action).getOrElse:
				refreshWCs(prev, updatedGame, action)
					.resetImportant(action.playerIndex)
					.when(_.level >= Level.BasicCM && rank == 1): g =>
						checkOcm(prev, action) match
							case None =>
								g.withMove(PlayInterp.None)
							case Some(orders) =>
								val chop = orders.min
								val mistake = game.state.deck(chop).id().exists: id =>
									game.state.isBasicTrash(id) || id.rank == 1

								if mistake then
									Log.warn("bad ocm!")

								performCM(g, orders).withMove:
									if mistake then PlayInterp.Mistake else PlayInterp.OrderCM
					.copy(
						dcStatus = DcStatus.None,
						dda = None
					)
			.elim()

		def takeAction(game: HGroup): IO[PerformAction] =
			val (state, me) = (game.state, game.me)

			val solveEndgame =
				if state.inEndgame && state.remScore <= state.variant.suits.length + 1 then
					IO.blocking:
						Log.highlight(Console.MAGENTA, "trying to solve endgame...")

						EndgameSolver(monteCarlo = true).solve(game) match
							case Left(err) =>
								Log.info(s"couldn't solve endgame: $err")
								None
							case Right((perform, winrate)) =>
								if winrate < Frac(1, 100) then
									Log.info(s"winrate below 1% (${winrate.toString}), skipping")
									None
								else
									Log.info(s"endgame solved!")
									Some(perform)
				else
					IO.pure(None)

			solveEndgame.map: solved =>
				solved.getOrElse:
					val discardOrders = game.findDiscardable(state.ourPlayerIndex)
					val playableOrders = me.thinksPlayables(game, state.ourPlayerIndex)

					Log.info(s"playables $playableOrders")
					Log.info(s"discardable $discardOrders")

					val urgent = playableOrders.find(game.meta(_).bluffed)

					if urgent.isDefined then
						Log.info(s"urgent bluffed play! ${urgent.get}")
						PerformAction.Play(urgent.get)
					else
						val allClues =
							for
								target <- (0 until state.numPlayers) if state.canClue && target != state.ourPlayerIndex
								clue   <- state.allValidClues(target)
							yield
								val perform = PerformAction.fromClue(clue)
								val action = perform.toAction(state, state.ourPlayerIndex)
								val value = evalAction(game, action)
								(perform, action, value)

						val earlyGameClue = game.earlyGameClue(state.ourPlayerIndex)

						// Log.info(s"early game clue? ${earlyGameClue.map(_.fmt(state))}")

						val hasEarlyGameClue = earlyGameClue.isDefined &&
							discardOrders.isEmpty &&
							!(state.clueTokens == 1 && valid1ClueScream(game, state.nextPlayerIndex(state.ourPlayerIndex)))

						if hasEarlyGameClue then
							Log.highlight(Console.YELLOW,s"must clue in early game! (found ${earlyGameClue.get.fmt(state)})")

						val allPlays = playableOrders.map: o =>
							val action = PlayAction(state.ourPlayerIndex, o, me.thoughts(o).id(infer = true, partial = true))
							val value = evalAction(game, action)
							(PerformAction.Play(o), action, value)

						val cantDiscard = state.clueTokens == 8 ||
							// playableOrders.exists(game.meta(_).status == CardStatus.Finessed) ||
							(state.pace == 0 && (allClues.exists(_._3 > 0) || allPlays.nonEmpty)) ||
							game.dcStatus != DcStatus.None ||
							hasEarlyGameClue

						Log.info(s"can discard: ${!cantDiscard}")

						val allDiscards = if cantDiscard then Nil else
							discardOrders.map: o =>
								val action = DiscardAction(state.ourPlayerIndex, o, me.thoughts(o).id(infer = true))
								val value = evalAction(game, action)
								(PerformAction.Discard(o), action, value)

						val chop = game.chop(state.ourPlayerIndex)

						val inDDA = game.level >= Level.Stalling &&
							game.state.numPlayers > 2 &&
							game.dda.exists(id => state.isCritical(id) && game.chop(state.ourPlayerIndex).exists(me.thoughts(_).possible.contains(id)))

						val canDiscardChop =
							chop.isDefined &&
							!cantDiscard &&
							!me.thinksLocked(game, state.ourPlayerIndex) &&
							!hasEarlyGameClue &&
							!inDDA &&
							{
								((!state.canClue || allPlays.isEmpty) && allDiscards.forall(_._3 == -100)) ||
								state.clueTokens == 0 ||
								(state.clueTokens == 1 && valid1ClueScream(game, state.nextPlayerIndex(state.ourPlayerIndex)))
							}

						val allActions =
							allClues.concat(allPlays).concat(allDiscards).when(_ => canDiscardChop): as =>
								val action = DiscardAction(state.ourPlayerIndex, chop.get, -1, -1, false)
								val value = evalAction(game, action)
								as :+ (PerformAction.Discard(chop.get), action, value)

						if allActions.isEmpty then
							val anxietyPlay = me.anxietyPlay(state, state.ourPlayerIndex)

							if game.level >= Level.Stalling && anxietyPlay.isDefined then
								Log.info("anxiety play!")
								PerformAction.Play(anxietyPlay.get)
							else if state.clueTokens == 8 then
								Log.error("No actions available at 8 clues! Playing slot 1")
								PerformAction.Play(state.ourHand.head)
							else
								PerformAction.Discard(me.lockedDiscard(state, state.ourPlayerIndex))
						else
							allActions.maxBy(_._3)._1

		def updateTurn(game: HGroup, action: TurnAction) =
			game

		override def cleanHypo(game: HGroup) =
			game.waiting.foldLeft(game): (acc, wc) =>
				if !wc.symmetric then acc else
					revert(acc, wc.focus, List(wc.inference))
						.pipe: g =>
							g.copy(players = g.players.map: player =>
								player.withThought(wc.focus)(t => t.copy(inferred = t.inferred.difference(wc.inference)))
							)

		override def refreshAfterPlay(prev: HGroup, game: HGroup, action: PlayAction) =
			refreshWCs(prev, game, action, elim = false, hypo = Some(-1))

		def findAllClues(game: HGroup, giver: Int) =
			val state = game.state

			val level = Logger.level
			Logger.setLevel(LogLevel.Off)

			var addedUselessClue = false

			def clueValue(clue: Clue): Double =
				val list = state.clueTouched(state.hands(clue.target), clue)
				val action = ClueAction(giver, clue.target, list, clue.base)

				// Only touches previously-clued trash
				if list.forall(o => state.deck(o).clued && state.isBasicTrash(state.deck(o).id().get)) then
					if addedUselessClue then
						return -99
					else
						addedUselessClue = true
						return 0

				Log.highlight(Console.GREEN, s"===== Predicting value for ${clue.fmt(state)} =====")
				val hypoGame = game.simulate(action)

				if hypoGame.lastMove == Some(ClueInterp.Mistake) then
					return -99

				val clueResult = getResult(game, hypoGame, action)
				val useful =
					clueResult > -10 &&		// A "useless" clue is already given a -10 mod from clueResult
					state.hands(clue.target).exists: o =>
						game.deckIds(o).forall(state.isUseful) &&
						hypoGame.state.deck(o).clued &&
						hypoGame.common.thoughts(o).possible.length < game.common.thoughts(o).possible.length

				if useful then
					clueResult
				else if addedUselessClue then
					return -99
				else
					addedUselessClue = true
					return 0

			val allClues =
				(for
					target <- (0 until state.numPlayers) if target != giver
					clue   <- state.allValidClues(target)
					value = clueValue(clue) if value > -10
				yield
					(clue, value))
				.sortBy((_, value) => -value)
				.map((clue, _) => PerformAction.fromClue(clue))

			Logger.setLevel(level)
			allClues

		def findAllDiscards(game: HGroup, playerIndex: Int) =
			val trash = game.common.thinksTrash(game, playerIndex)
			val expectedDc = trash.headOption
				.orElse(game.chop(playerIndex))
				.getOrElse(game.players(playerIndex).lockedDiscard(game.state, playerIndex))

			val targets =
				if game.level >= Level.Endgame && game.state.inEndgame then
					// ALlow discarding any known trash
					game.state.hands(playerIndex).filter(game.players(playerIndex).orderKt(game, _))
						.when(_.isEmpty)(_ => Vector(expectedDc))
				else
					Seq(expectedDc)

			targets.map(PerformAction.Discard(_))

		def evalAction(game: HGroup, action: Action): Double =
			_evalAction(game, action)

	def atLevel(level: Int) =
		(tableID: Int, state: State, inProgress: Boolean) =>
			HGroup(tableID, state, inProgress, level)
