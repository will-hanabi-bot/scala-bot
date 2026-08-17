package scala_bot.hgroup

import cats.effect.IO

import scala_bot.basics._
import scala_bot.endgame.EndgameSolver
import scala_bot.lib.{FastBitSet, Frac}
import scala_bot.utils._
import scala_bot.logger.{Log, Logger, LogLevel}

enum Positional:
	case Brown, Mud, Pink
	case Stale1(originalFocus: Int)

case class FocusResult(
	focus: Int,
	chop: Boolean = false,
	positional: Option[Positional] = None
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
	importantFinesse: Vector[Boolean],
	savedCtx: Vector[Option[ClueContext]],

	meta: Vector[ConvData] = Vector(),
	deckIds: Vector[Option[Identity]],
	future: Vector[IdentitySet],
	catchup: Boolean = false,
	notes: Map[Int, Note] = Map(),
	moveHistory: Vector[Interp] = Vector.empty,
	queuedCmds: List[(String, String)] = Nil,
	nextInterp: Option[Interp] = None,
	noRecurse: Boolean = false,
	hypothetical: Boolean = false,
	rewindDepth: Int = 0,
	inProgress: Boolean = false,

	level: Int = 1,
	waiting: List[WaitingConnection] = Nil,
	stalled5: Boolean = false,
	clued1sOnChop: FastBitSet = FastBitSet.empty,
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
		val unknown1s = orders.filter(this.unknown1)
		val next1 = this.next1(unknown1s)

		orders.filter: o =>
			if this.meta(o).bluffed then
				true
			else
				val possibleFocusDupe =
					player.thoughts(o).id().isEmpty &&
					!unknown1s.contains(o) &&
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

				val unrevealedHidden = waiting.find: wc =>
					!state.deck(o).clued &&
					!wc.symmetric && !wc.ambiguousSelf &&
					// This is part of a hidden (?) connection that is not currently revealed
					wc.connections.nonEmpty && {
						wc.currConn.hidden && wc.connections.tail.exists(_.order == o) ||
						wc.connections.tail.exists(c => c.order == o && c.hidden)
					}

				val unordered1 = (state.variant.pinkish || this.level < Level.BasicCM) && unknown1s.contains(o) && !next1.contains(o)

				val ambiguous1 = unknown1s.nonEmpty && {
					val possibleGDtargets = unknown1s.dropWhile(this.meta(_).status != CardStatus.GentlemansDiscard)
					possibleGDtargets.length > 1 &&
					unknown1s.length > state.playStacks.count(_ == 0) &&
					possibleGDtargets.length < unknown1s.length &&
					o != possibleGDtargets.head
				}

				val potentialClandestine =
					val dependentConns = waiting.filter(_.connections.exists(_.order == o))
					dependentConns.nonEmpty && dependentConns.forall(this.potentialClandestineWc(playerIndex, o, _).isDefined)

				// ((assume && !xmeta(o).fStatus.contains(FStatus.PossiblyOn(state.ourPlayerIndex))) || isDefinite(o)) &&
				(assume || isDefinite(o)) &&
				!possibleFocusDupe &&
				!olderFinesseExists &&
				unrevealedHidden.isEmpty &&
				!unordered1 &&
				!ambiguous1 &&
				!potentialClandestine

	override def validArr(id: Identity, order: Int): Boolean =
		val playables = this.me.thinksPlayables(this, state.ourPlayerIndex)

		if playables.contains(order) then
			state.isPlayable(id)
		else if this.isTouched(order) && !(if state.ourHand.contains(order) then this.me else this.common).thoughts(order).reset then
			val good = this.me.thoughts(order).possible.difference(state.trashSet)
			good.isEmpty || good.contains(id)
		else if !state.canClue && state.isCritical(id) && chop(state.ourPlayerIndex).contains(order) then
			false
		else
			true

	def withXMeta(order: Int)(f: XConvData => XConvData) =
		copy(xmeta = xmeta.updated(order, f(xmeta(order))))

	/** Marks DDA if the the next player could be double discarding. */
	def checkDDA(discarder: Int, id: Identity): HGroup =
		if level < Level.Stalling || state.numPlayers == 2 || id.suitIndex == -1 || id.rank == -1 || state.isBasicTrash(id) then
			return this

		val nextPlayerIndex = state.nextPlayerIndex(discarder)
		val nextPlayer = players(nextPlayerIndex)

		val dda =
			state.isCritical(id) &&
			chop(nextPlayerIndex).exists: chop =>
				nextPlayer.thoughts(chop).possible.contains(id) &&
				!visibleFind(state, nextPlayer, id, infer = true).exists(this.isTouched)

		if dda then copy(dda = Some(id)) else this

	def isCM(order: Int) =
		meta(order).cm && !state.deck(order).clued

	def isDefinite(order: Int) =
		xmeta(order).fStatus.forall: f =>
			f != FStatus.PossiblyAmbiguous(state.ourPlayerIndex) &&
			f != FStatus.PossiblyOn(state.ourPlayerIndex)

	def chop(playerIndex: Int) =
		state.hands(playerIndex).findLast: o =>
			!state.deck(o).clued &&
			(meta(o).status == CardStatus.None || meta(o).status == CardStatus.PermissionToDiscard || !isDefinite(o))
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
					!state.deck(o).clued && {
						meta(o).status == CardStatus.None ||
						meta(o).status == CardStatus.PermissionToDiscard
					}

	def mustClue(playerIndex: Int) =
		val bob = state.nextPlayerIndex(playerIndex)

		state.canClue &&
		state.numPlayers > 2 &&
		!common.thinksLoaded(this, bob) &&
		chop(bob).flatMap(state.deck(_).id()).exists(state.isCritical)

	def invalidFocus(giver: Int, clue: ClueLike, id: Identity, focusResult: FocusResult) =
		val FocusResult(focus, chop, positional) = focusResult

		val orangePlayClueAssumption =
			!chop &&
			state.variant.inverted &&
			state.variant.rainbowish &&
			clue.kind == ClueKind.Colour &&
			state.variant.colourableSuits(clue.value).suitType.inverted &&
			state.variant.suits(id.suitIndex).suitType.rainbowish

		state.isBasicTrash(id) ||
		(state.variant.pinkish && !positional.contains(Positional.Pink) && clue.kind == ClueKind.Rank && clue.value != id.rank) ||
		(state.variant.inverted && chop && state.isInverted(id) && state.isPlayable(id)) ||
		orangePlayClueAssumption ||
		visibleFind(state, common, id, infer = true, excludeOrder = focus).exists: o =>
			this.me.thoughts(o).matches(id) ||
			(this.me.thoughts(o).matches(id, assume = true) && this.me.thoughts(o).possible.contains(id))
		||
		common.links.existsM:
			case Link.Promised(_, i, target) => target != focus && id.matches(i)
		||
		waiting.exists: wc =>
			!wc.symmetric && !wc.ambiguousSelf && wc.target == giver &&
			common.thoughts(wc.focus).inferred.contains(id) &&
			wc.connections.exists: conn =>
				xmeta(conn.order).fStatus.contains(FStatus.PossiblyOn(giver))

	def findFinesse(playerIndex: Int, connected: FastBitSet = FastBitSet.empty, ignore: FastBitSet = FastBitSet.empty): Option[Int] =
		val order = state.hands(playerIndex).find: o =>
			val card = state.deck(o)
			val status = this.meta(o).status

			!card.clued &&
			!this.meta(o).cm &&
			!connected.contains(o) &&
			(status != CardStatus.Finessed || this.xmeta(o).fStatus.contains(FStatus.PossiblyOn(state.ourPlayerIndex)))

		order.filter(!ignore.contains(_))

	def findFinesseId(playerIndex: Int, id: Identity, connected: FastBitSet = FastBitSet.empty, ignore: FastBitSet = FastBitSet.empty, overrideLayer: Boolean = false): Option[Int] =
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

		!meta(order).trash &&
		meta(order).status != CardStatus.Finessed &&
		common.thoughts(order).possible.length > 1 &&
		clues.nonEmpty &&
		clues.forall(_.isEq(BaseClue(ClueKind.Rank, 1)))

	private def priority1(order: Int) =
		// play fresh 1s from a later turn before chop-focus on an earlier turn
		state.deck(order).clues.headOption.fold(Int.MaxValue): clue =>
			clue.turn * -1000 + {
				if clued1sOnChop.contains(order) then
					-100 - order
				else if meta(order).cm then
					100 - order
				else if state.inStartingHand(order) then
					order
				else
					-order
			}

	def next1(orders: Seq[Int]) =
		orders.minByOption(priority1)

	def order1s(orders: Seq[Int]) =
		orders.sortBy(priority1)

	def priority(orders: List[Int]) =
		val initial = (0 to 5).map(_ => Vector.empty[Int])
		orders.foldLeft(initial): (acc, o) =>
			val thought = this.me.thoughts(o)

			val inFinesse = this.isBlindPlaying(o)	// TODO: play link?
			lazy val unknownCM = isCM(o) &&
				!state.deck(o).clued &&
				thought.possible.difference(state.playableSet).nonEmpty

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
		val newlyClued = list.filter(!prev.state.deck(_).clued)

		val stale1 =
			prev.level >= Level.Context &&
			clue.isEq(ClueKind.Rank, 1) &&
			newlyClued.length > 1 &&
			newlyClued.forall(state.inStartingHand)

		if chop.exists(list.contains) && !stale1 then
			return FocusResult(chop.get, chop = true)

		val reclue = newlyClued.isEmpty

		val pinkChoiceTempo = clue.kind == ClueKind.Rank &&
			state.variant.pinkish &&
			reclue &&
			clue.value <= hand.length &&
			list.contains(hand(clue.value - 1)) &&
			List(list.max, hand(clue.value - 1)).forall(this.knownAs(_, PINKISH))

		if pinkChoiceTempo then
			return FocusResult(hand(clue.value - 1), positional = Some(Positional.Pink))

		val brownTempo = clue.kind == ClueKind.Colour &&
			state.variant.colourableSuits(clue.value).name.contains("Brown") &&
			reclue

		if brownTempo then
			val focus =
				val sortedBrowns = list.filter(o => prev.knownAs(o, "Brown".r.unanchored)).sorted

				if this.level >= Level.TempoClues then
					sortedBrowns.find(o => !(prev.meta(o).focused && prev.common.hypoPlays.contains(o)))
						.getOrElse(sortedBrowns.head)
				else
					sortedBrowns.head

			return FocusResult(focus, positional = Some(Positional.Brown))

		if clue.isEq(ClueKind.Rank, 1) && newlyClued.length > 1 then
			// Custom implementation of ordered1s
			val next1 = newlyClued.minBy: o =>
				if chop.contains(o) then
					-999
				else if meta(o).cm then
					100 - o
				else if state.inStartingHand(o) then
					o
				else
					-o

			val skip = prev.common.thoughts(next1).inferred.forall: id =>
				id.rank != 1 ||
				prev.meta(next1).staleIds.contains(id) ||
				prev.state.trashSet.contains(id)

			if stale1 && skip then
				return FocusResult(newlyClued.max, positional = Some(Positional.Stale1(next1)))
			else
				return FocusResult(next1)

		val muddyCards = list.filter: o =>
			val variant = state.variant

			// All possible ids are rainbowish
			this.common.thoughts(o).possible.forall: i =>
				(variant.rainbowS && variant.specialRank.contains(i.rank)) ||
				variant.suits(i.suitIndex).suitType.rainbowish
			&& {
				// Rank-clued cards can't be muddy unless it's an omni card or rainbow-X
				!state.deck(o).clues.exists(_.kind == ClueKind.Rank) ||
				this.common.thoughts(o).possible.forall: i =>
					val suitType = variant.suits(i.suitIndex).suitType

					(suitType.pinkish && suitType.rainbowish) ||
					(variant.specialRank.contains(i.rank) && variant.pinkS && variant.rainbowS)
			}
		.sortBy(o => -o)

		val mudClue = clue.kind == ClueKind.Colour &&
			(state.variant.muddy || (state.variant.suits.exists(s => s.suitType.pinkish && s.suitType.rainbowish)) || state.variant.rainbowS) &&
			reclue &&
			// Mud clues should only work if the leftmost (non-kt) card is muddy.
			muddyCards.contains(list.maxIf(o => !prev.common.orderKt(prev, o), -1))

		if mudClue then
			val coloursAvailable = state.variant.colourableSuits.length
			val focusIndex = (clue.value - coloursAvailable + 6*muddyCards.length) % muddyCards.length
			// Log.info(s"mud clue! value ${clue.value} focusing index ${focusIndex} slot ${state.hands(target).indexOf(muddyCards(focusIndex)) + 1}")
			return FocusResult(muddyCards(focusIndex), positional = Some(Positional.Mud))

		val pinkStall5 =
			val valid = clue.isEq(BaseClue(ClueKind.Rank, 5)) &&
				state.variant.pinkish &&
				stallSeverity(prev, prev.common, giver) > 0

			if !valid then None else
				newlyClued.filterNot(prev.isCM).minOption

		if pinkStall5.isDefined then
			FocusResult(pinkStall5.get)

		else
			val sortedList = list.sortBy(o => -o)
			val focus =
				sortedList.find(o => !prev.state.deck(o).clued && (prev.meta(o).status == CardStatus.None || prev.meta(o).status == CardStatus.PermissionToDiscard))
					.orElse(sortedList.find(prev.isCM))
					.when(_ => this.level >= Level.TempoClues):
						_.orElse(sortedList.find(o => !(prev.meta(o).focused && prev.common.hypoPlays.contains(o))))
					.orElse(sortedList.headOption)

			focus match
				case Some(order) => FocusResult(order)
				case None        => throw new Error("No focus found!")

	def earlyGameClue(giver: Int): Option[Clue] =
		if noRecurse || !inEarlyGame || !state.canClue || state.numPlayers == 2 then
			return None

		val allClues = for
			target <- (0 until state.numPlayers).view if target != giver
			clue   <- state.allValidClues(target)
		yield
			clue

		allClues.find: clue =>
			val action = Action.fromClue(state, clue, giver)
			val FocusResult(focus, chop, _) = determineFocus(this, action)
			val focusCard = state.deck(focus)
			val focusId = focusCard.id().get

			meta(focus).status != CardStatus.Finessed &&
			!focusCard.clued &&
			state.isUseful(focusId) && {
				val hypo = this.copy(noRecurse = true, allowFindOwn = false).simulateAction(action)

				hypo.lastMove.matchesP:
					case Some(ClueInterp.Save) =>
						state.isCritical(focusId) || {
							focusId.rank == 2 &&
							visibleFind(state, this.players(giver), focusId, infer = true, excludeOrder = focus).isEmpty &&
							dupeResponsibility(this, focusId, action.target).contains(giver)
						}

					case Some(ClueInterp.Stall) =>
						hypo.stallInterp == Some(StallInterp.Stall5) &&
						!stalled5

					case Some(ClueInterp.Play) =>
						val (badTouch, _, _) = badTouchResult(this, hypo, action)
						badTouch.isEmpty ||
						(chop && visibleFind(state, this.players(giver), focusId, infer = true, excludeOrder = focus).isEmpty)	// save principle
			}

	def availableClue(giver: Int): Option[Clue] =
		if noRecurse || !state.canClue || state.numPlayers == 2 then
			return None

		val allClues = for
			target <- (0 until state.numPlayers).view if target != giver && target != state.lastPlayerIndex(giver)
			clue   <- state.allValidClues(target)
		yield
			clue

		allClues.find: clue =>
			val action = Action.fromClue(state, clue, giver)
			val FocusResult(focus, chop, _) = determineFocus(this, action)
			val focusCard = state.deck(focus)
			val focusId = focusCard.id().get

			meta(focus).status != CardStatus.Finessed &&
			!focusCard.clued &&
			state.isUseful(focusId) && {
				val hypo = this.copy(noRecurse = true, allowFindOwn = false).simulateAction(action)

				hypo.lastMove.matchesP:
					case Some(ClueInterp.Save) =>
						state.isCritical(focusId) || {
							focusId.rank == 2 &&
							visibleFind(state, this.players(giver), focusId, infer = true, excludeOrder = focus).isEmpty &&
							dupeResponsibility(this, focusId, action.target).contains(giver)
						}

					case Some(ClueInterp.Stall) =>
						inEarlyGame &&
						hypo.stallInterp == Some(StallInterp.Stall5) &&
						!stalled5

					case Some(ClueInterp.Play) =>
						val (badTouch, _, _) = badTouchResult(this, hypo, action)
						badTouch.isEmpty ||
						(chop && visibleFind(state, this.players(giver), focusId, infer = true, excludeOrder = focus).isEmpty)	// save principle
			}

	def findDiscardable(playerIndex: Int) =
		state.hands(playerIndex).filter: o =>
			this.meta(o).status != CardStatus.Bluffed &&
			this.meta(o).status != CardStatus.MaybeBluffed &&
			this.meta(o).status != CardStatus.FMaybeBluffed &&
			!(this.meta(o).status == CardStatus.Finessed && (this.xmeta(o).idUncertain || this.xmeta(o).fStatus.contains(FStatus.PossiblyOn(playerIndex)))) && {
				this.me.orderTrash(this, o) || {
					this.isTouched(o) &&
					this.me.thoughts(o).inferred.forall: id =>
						visibleFind(state, this.me, id, excludeOrder = o).nonEmpty
					&&
					!waiting.find(_.connections.exists(_.order == o)).exists(this.potentialClandestineWc(playerIndex, o, _).isDefined)
				}
			}
		.sortBy: o =>
			// Discard leftmost clued trash (largest order), then rightmost unclued trash (smallest order)
			if state.deck(o).clued then -o else o

	/** Returns a potential clandestine finesse that may alter the id of the card with the given order.
	  * @example See test "waits for a clandestine finesse to resolve".
	  */
	def potentialClandestineWc(playerIndex: Int, order: Int, containingWc: WaitingConnection) =
		waiting.find: wc =>
			wc != containingWc &&
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
			copy(future = future.updated(order, IdentitySet.single(Identity(suitIndex, rank))))
				.replay(state.deck(order).turnDrawn)
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
		copy(
			importantFinesse = importantFinesse.updated(playerIndex, false),
			savedCtx = savedCtx.updated(playerIndex, None)
		)

	/** Returns whether a real waiting connection was lost, which usually indicates that a mistake occurred. */
	def wcLost(prev: HGroup, action: Action): Option[WaitingConnection] =
		prev.waiting.find: wc =>
			!wc.symmetric && !wc.ambiguousSelf &&
			!action.matchesP:
				case p: PlayAction => p.order == wc.currConn.order
			&&
			!this.waiting.exists(w => w.connections.forall(wc.connections.contains) && w.inference == wc.inference && w.turn == wc.turn)

	def importantAction(playerIndex: Int) =
		importantFinesse(playerIndex) || savedCtx(playerIndex).fold(false)(urgentSave)

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
		importantFinesse = Vector.fill(state.numPlayers)(false),
		savedCtx = Vector.fill(state.numPlayers)(None),
		level = level
	)

	def apply(tableID: Int, state: State, inProgress: Boolean, level: Int) =
		init(tableID, state, inProgress, genPlayers(state), level)

	given GameOps[HGroup] with
		def copyWith(game: HGroup, updates: GameUpdates) =
			val meta = updates.meta.getOrElse(game.meta)

			val newXMeta =
				if meta.length > game.xmeta.length + 1 then
					throw new Error("meta grew twice!")
				else if meta.length == game.xmeta.length + 1 then
					game.xmeta :+ XConvData()
				else
					game.xmeta

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
				hypothetical = updates.hypothetical.getOrElse(game.hypothetical),

				xmeta = newXMeta
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
				importantFinesse = Vector.fill(game.state.numPlayers)(false),
				savedCtx = Vector.fill(game.state.numPlayers)(None),
				xmeta = Vector.fill(game.base._2.length)(XConvData())
			)

		def interpretClue(prev: HGroup, game: HGroup, action: ClueAction): HGroup =
			val updatedGame = game.refreshUncertain.elim()

			checkRevealLayer(prev, updatedGame, action).getOrElse:
				val pre = refreshWCs(prev, updatedGame, action, beforeClueInterp = true)
					.resetImportant(action.playerIndex)

				if !game.allowFindOwn && pre.wcLost(prev, action).nonEmpty then
					Log.warn(s"removed wc ${game.state.logConns(pre.wcLost(prev, action).get.connections)}! mistake")
					pre.withMove(ClueInterp.Mistake)
				else
					val interpreted = interpClue(ClueContext(prev, pre, action))
					val updatedPre = pre.copy(importantFinesse = interpreted.importantFinesse, savedCtx = interpreted.savedCtx)
						.withMove(interpreted.lastMove.get)

					refreshWCs(prev, updatedPre, action)
						.cond(_.waiting.count(wc => !wc.symmetric && !wc.ambiguousSelf) < pre.waiting.count(wc => !wc.symmetric && !wc.ambiguousSelf) && !game.noRecurse) { g =>
							Log.highlight(Console.GREEN, "----- REINTERPRETING CLUE -----")
							val res = interpClue(ClueContext(prev, g.copy(moveHistory = g.moveHistory.dropRight(1)), action))
							Log.highlight(Console.GREEN, "----- DONE REINTERPRETING -----")
							res
						} {
							Log.highlight(Console.GREEN, s"refreshing interpreted!")
							_ => refreshWCs(prev, interpreted, action)
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

			val reinterp = failed || prev.common.thoughts(order).possibilities.forall(prev.state.isInverted)
			val reinterpGame = if reinterp then updatedGame.reinterpPlay(prev, action) else None

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
				refreshedGame.copy(dcStatus = DcStatus.None)
					.checkDDA(playerIndex, Identity(suitIndex, rank))
					.withMove(DiscardInterp.None)
			.when(_.inEarlyGame): g =>
				val endEarlyGame = !failed &&
					!game.state.deck(order).clued && {
						game.meta(order).status == CardStatus.None ||
						game.meta(order).status == CardStatus.PermissionToDiscard
					}

				// Write staleness if ending early game
				g.when(_ => endEarlyGame && g.level >= Level.Context): g =>
					g.state.heldOrders.foldLeft(g): (acc, order) =>
						acc.withMeta(order): m =>
							m.copy(staleIds = m.staleIds.union(g.state.playableSet))
				.copy(inEarlyGame = !endEarlyGame)
			.elim()

		def interpretPlay(prev: HGroup, game: HGroup, action: PlayAction): HGroup =
			val PlayAction(playerIndex, order, suitIndex, rank) = action

			val updatedGame = game.refreshUncertain

			updatedGame.reinterpPlay(prev, action).getOrElse:
				val pre = refreshWCs(prev, updatedGame, action)

				if !game.allowFindOwn && pre.wcLost(prev, action).nonEmpty then
					Log.warn(s"removed wc ${game.state.logConns(pre.wcLost(prev, action).get.connections)}! mistake")
					pre.withMove(PlayInterp.Mistake)
				else
					pre.resetImportant(action.playerIndex).pipe: g =>
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

			Log.info(s"ptd? ${game.chop(state.ourPlayerIndex).map(game.meta(_).status)}")

			val solveEndgame =
				if state.remScore <= state.variant.suits.length + 1 && state.pace + state.cardsLeft <= 7 then
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

			solveEndgame.flatMap: solved =>
				solved.map(IO.pure).getOrElse:
					IO.blocking:
						val discardOrders = game.findDiscardable(state.ourPlayerIndex)
						val playableOrders = me.thinksPlayables(game, state.ourPlayerIndex)

						Log.info(s"playables $playableOrders")
						Log.info(s"discardable $discardOrders")

						val urgent = playableOrders.find(game.meta(_).bluffed)

						if urgent.isDefined then
							Log.info(s"urgent bluffed play! ${urgent.get}")
							PerformAction.tryPlay(game, urgent.get)
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

							val allPlays = playableOrders.map: o =>
								val action = PlayAction(state.ourPlayerIndex, o, me.thoughts(o).id(infer = true, partial = true))
								val value = evalAction(game, action)
								(PerformAction.tryPlay(game, o), action, value)

							val cantDiscard =
								state.clueTokens == 8 ||
								(state.pace == 0 && (allClues.exists(_._3 > 0) || allPlays.nonEmpty))

							Log.highlight(Console.YELLOW, s"can discard: ${!cantDiscard}")

							val allDiscards = if cantDiscard then Vector.empty else discardOrders.map: o =>
								val action = DiscardAction(state.ourPlayerIndex, o, me.thoughts(o).id(infer = true))
								val value = evalAction(game, action)
								(PerformAction.tryDiscard(game, o), action, value)

							val screamAt1Clue = state.clueTokens == 1 && valid1ClueScream(game, state.nextPlayerIndex(state.ourPlayerIndex))
							val noKtToDiscard = allDiscards.forall((_, action, value) => !me.orderTrash(game, action.order) || value == -100)

							val chop = game.chop(state.ourPlayerIndex)

							val canDiscardChop =
								chop.isDefined &&
								!cantDiscard &&
								game.dcStatus == DcStatus.None &&
								game.dda.isEmpty &&
								!me.thinksLocked(game, state.ourPlayerIndex) &&
								{
									((!state.canClue || allPlays.isEmpty) && noKtToDiscard) ||
									state.clueTokens == 0 ||
									screamAt1Clue
								} &&
								{
									screamAt1Clue ||
									game.earlyGameClue(state.ourPlayerIndex).fold(true): clue =>
										Log.highlight(Console.YELLOW,s"must clue in early game! (found ${clue.fmt(state)})")
										false
								}

							Log.highlight(Console.YELLOW, s"can discard chop: ${canDiscardChop}")

							val allActions = allClues.concat(allPlays)
								.when(_ => !cantDiscard):
									_ ++ allDiscards
								.when(_ => canDiscardChop): as =>
									val action = DiscardAction(state.ourPlayerIndex, chop.get, -1, -1, false)
									val value = evalAction(game, action)
									as :+ (PerformAction.tryDiscard(game, chop.get), action, value)

							if allActions.isEmpty then
								val anxietyPlay = me.anxietyPlay(state, state.ourPlayerIndex)

								if game.level >= Level.Stalling && anxietyPlay.isDefined then
									Log.info("anxiety play!")
									PerformAction.tryPlay(game, anxietyPlay.get)
								else if state.clueTokens == 8 then
									Log.error("No actions available at 8 clues! Playing slot 1")
									PerformAction.tryPlay(game, state.ourHand.head)
								else
									PerformAction.Discard(me.lockedDiscard(state, state.ourPlayerIndex))
							else
								allActions.maxBy(_._3)._1

		def updateTurn(game: HGroup, action: TurnAction) =
			val currentPlayerIndex = action.currentPlayerIndex

			game.chop(currentPlayerIndex).fold(game): chop =>
				val hasPtd =
					game.state.clueTokens < 8 &&
					game.dcStatus == DcStatus.None &&
					game.meta(chop).status != CardStatus.PermissionToDiscard &&
					!game.common.thinksLoaded(game, currentPlayerIndex) &&
					!game.common.thinksLocked(game, currentPlayerIndex) &&
					game.availableClue(currentPlayerIndex).isEmpty

				if hasPtd then
					Log.info(s"writing ptd on ${game.state.names(currentPlayerIndex)}")
					game.withMeta(chop)(_.copy(status = CardStatus.PermissionToDiscard))
				else
					game

		override def cleanHypo(game: HGroup) =
			game.waiting.foldLeft(game): (acc, wc) =>
				if !wc.symmetric then acc else
					revert(acc, wc.focus, List(wc.inference)).pipe: g =>
						g.copy(
							players = g.players.map:
								_.withThought(wc.focus)(t => t.copy(inferred = t.inferred.difference(wc.inference)))
						)

		override def refreshAfterPlay(prev: HGroup, game: HGroup, action: PlayAction) =
			refreshWCs(prev, game, action, elim = false, hypo = Some(-1))

		def findAllClues(game: HGroup, giver: Int) =
			val state = game.state

			val level = Logger.level
			Logger.setLevel(LogLevel.Off)

			def clueValue(clue: Clue): Double =
				val list = state.clueTouched(state.hands(clue.target), clue)
				val action = ClueAction(giver, clue.target, list, clue.base)

				Log.highlight(Console.GREEN, s"===== Predicting value for ${clue.fmt(state)} =====")
				val hypoGame = game.simulate(action)

				if hypoGame.lastMove == Some(ClueInterp.Mistake) then
					return -100

				getResult(game, hypoGame, action)

			val allClues = (0 until state.numPlayers)
				.filter(_ != giver)
				.flatMap(state.allValidClues)
				.partition: clue =>
					val list = state.clueTouched(state.hands(clue.target), clue)
					list.exists: o =>
						state.deck(o).id().exists(state.isUseful)
				.pipe: (useful, useless) =>
					if useful.isEmpty then
						val validClue = useless.find(clueValue(_) > -11)

						validClue.map(PerformAction.fromClue).toList
					else
						useful.map(clue => (clue, clueValue(clue)))
							.partition((_, value) => value > -2)
							.pipe: (better, worse) =>
								if better.isEmpty then
									worse.filter(_._2 > -11)
										.maxByOption(_._2)
										.orElse:
											useless.find(clueValue(_) > -11).map((_, 0.0))
										.toList

								else
									better.sortBy((_, value) => -value)
							// .tap: clues =>
							// 	for (clue, value) <- clues do
							// 		Log.info(s"clue ${clue.fmt(state)} $value")
							.map((clue, _) => PerformAction.fromClue(clue))

			Logger.setLevel(level)
			allClues

		def findAllDiscards(game: HGroup, playerIndex: Int) =
			val state = game.state
			val expectedDc = game.findDiscardable(playerIndex).headOption
				.orElse(game.chop(playerIndex))
				.getOrElse(game.players(playerIndex).lockedDiscard(state, playerIndex))

			if game.level >= Level.Endgame && (game.inEndgame || state.remScore < state.variant.suits.length) then
				val expected = PerformAction.tryDiscard(game, expectedDc)
				val positionals = state.hands(playerIndex).filter: o =>
					val index = state.hands(playerIndex).indexOf(o)

					game.players(playerIndex).orderTrash(game, o) &&
					state.hands.zipWithIndex.exists: (hand, i) =>
						i != playerIndex &&
						hand.lift(index).exists: o2 =>
							state.deck(o2).id().exists(state.isUseful) &&
							!game.common.hypoPlays.contains(o2)
				.flatMap(o => Seq(PerformAction.tryDiscard(game, o), PerformAction.tryPlay(game, o)))

				if positionals.contains(expected) then positionals else positionals :+ expected
			else
				Seq(PerformAction.tryDiscard(game, expectedDc))

		def evalAction(game: HGroup, action: Action): Double =
			_evalAction(game, action)

		override def preferEndgameClue(game: HGroup): Boolean =
			val state = game.state
			val nextPlayerIndex = state.nextPlayerIndex(state.ourPlayerIndex)

			val saveBob =
				!game.players(nextPlayerIndex).thinksLoaded(game, nextPlayerIndex) &&
				game.chop(nextPlayerIndex).exists: o =>
					state.deck(o).id().exists(state.isCritical)

			val clueLastCard =
				state.cardsLeft == 1 &&
				state.heldOrders.exists: o =>
					val holder = state.holderOf(o)

					// Someone else is holding a focusable, useful card that they don't know about
					holder != state.ourPlayerIndex &&
					state.deck(o).id().exists(state.isUseful) &&
					game.players(holder).thoughts(o).id(infer = true).isEmpty &&
					state.allValidClues(holder).exists: clue =>
						game.determineFocus(game, Action.fromClue(state, clue, state.ourPlayerIndex)).focus == o

			saveBob || clueLastCard

		override def preferEndgameDiscard(game: HGroup, playerIndex: Int): Boolean =
			val state = game.state

			if state.remScore > 2 then
				return false

			// Remaining score can be 2 if the player knows about their own 5
			if state.remScore == 2 then
				val order5s = state.hands(playerIndex).count:
					game.players(playerIndex).thoughts(_).id(infer = true).exists(_.rank == 5)

				if order5s != 1 then
					return false

			// Look for the remaining card in everyone else's hand
			val remaining = state.hands.zipWithIndex.findSome: (hand, holder) =>
				if holder == playerIndex then None else
					val usefulOrder = hand.find(state.deck(_).id().exists(state.isUseful))
					usefulOrder.map((holder, _))

			// Prefer discarding when no clue can focus the remaining card.
			remaining.exists: (holder, order) =>
				!state.allValidClues(holder).exists: clue =>
					game.determineFocus(game, Action.fromClue(game.state, clue, playerIndex)).focus == order

		override def injectReplay(orig: HGroup, hypo: HGroup): HGroup =
			hypo.copy(
				allowFindOwn = orig.allowFindOwn,
				hypothetical = orig.hypothetical
			)

	def atLevel(level: Int) =
		(tableID: Int, state: State, inProgress: Boolean) =>
			HGroup(tableID, state, inProgress, level)
