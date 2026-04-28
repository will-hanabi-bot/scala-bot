package scala_bot.refSieve

import scala_bot.basics._
import scala_bot.utils._
import scala_bot.logger.Log

case class ClueContext(prev: RefSieve, game: RefSieve, action: ClueAction)

def determineFocus(ctx: ClueContext, push: Boolean, right: Boolean = false) =
	val ClueContext(prev, game, action) = ctx
	val state = game.state
	val newlyTouched = action.list.filter(o => state.deck(o).clued && !prev.state.deck(o).clued)

	if newlyTouched.isEmpty then
		action.list.max

	else if push then
		val hand = state.hands(action.target)

		if right then
			val leastPriority = hand.findLast(!prev.state.deck(_).clued).getOrElse(-1)
			newlyTouched.minBy(o => if o == leastPriority then 99 else o)
		else
			val leastPriority = hand.find(!prev.state.deck(_).clued).getOrElse(-1)
			newlyTouched.maxBy(o => if o == leastPriority then -99 else o)

	else
		newlyTouched.maxOption.get

def refPlay(ctx: ClueContext, right: Boolean = false): (Option[ClueInterp], RefSieve) =
	val ClueContext(prev, game, action) = ctx
	val (common, state) = (game.common, game.state)
	val clueTarget = action.target
	val hand = state.hands(clueTarget)
	val newlyTouched = action.list.filter(o => state.deck(o).clued && !prev.state.deck(o).clued)

	val focus = determineFocus(ctx, push = true, right)
	val target = if right then
		newlyTouched.map(common.refer(prev, hand, _, left = false)).min
	else
		newlyTouched.map(common.refer(prev, hand, _, left = true)).max

	if game.isBlindPlaying(target) then
		Log.info(s"targeting an already known playable!")
		(None, game)
	else if game.meta(target).status == CardStatus.CalledToDiscard then
		Log.info(s"targeting a card called to discard!")
		(None, game)
	else
		targetPlay(ctx, target) match
			case res @ (None, _) => res
			case (interp, result) =>
				Log.info(s"ref play on ${state.names(clueTarget)}'s slot ${hand.indexOf(target) + 1} (focus $focus) infs ${result.common.strInfs(result.state, target)}")
				(interp, result)

def refDiscard(ctx: ClueContext): (Some[ClueInterp], RefSieve) =
	val ClueContext(prev, game, action) = ctx
	val state = game.state
	val clueTarget = action.target
	val hand = state.hands(clueTarget)

	val focus = determineFocus(ctx, push = false)
	val targetIndex = hand.indexWhere: o =>
		val status = game.meta(o).status

		o < focus && !state.deck(o).clued && (status == CardStatus.None || status == CardStatus.ChopMoved)

	if targetIndex == -1 then
		if prev.common.thinksLocked(prev, action.giver) || prev.state.clueTokens == 8 then
			Log.info(s"rank stall!")
			(Some(ClueInterp.Stall), game)
		else
			Log.highlight(Console.YELLOW, "lock!")

			val newGame = hand.foldLeft(game) { (acc, o) =>
				acc.withMeta(o)(_.copy(status = CardStatus.ChopMoved, by = Some(action.giver)))
			}
			.withMeta(focus)(_.copy(focused = true))

			(Some(ClueInterp.Lock), newGame)
	else
		val target = hand(targetIndex)
		Log.info(s"ref discard on ${state.names(clueTarget)}'s slot ${targetIndex + 1} (focus $focus)")

		val newGame = game
			.withMeta(target)(_.copy(
				status = CardStatus.CalledToDiscard,
				by = Some(action.giver))
			.reason(state.turnCount))
			.withMeta(focus)(_.copy(focused = true))

		(Some(ClueInterp.Discard), newGame)

def targetPlay(ctx: ClueContext, targetOrder: Int): (Option[ClueInterp], RefSieve) =
	val ClueContext(prev, game, action) = ctx
	val (common, state) = (game.common, game.state)
	val unknown = common.thoughts(targetOrder).id(infer = true, symmetric = true).isEmpty

	val focusPoss =
		for
			inf   <- common.thoughts(targetOrder).inferred if visibleFind(state, common, inf, infer = true, excludeOrder = targetOrder).isEmpty
			conns <- connect(ctx, targetOrder, inf, unknown)
		yield
			FocusPossibility(inf, conns, ClueInterp.Play)

	Log.info(s"focus possibilities [${focusPoss.map(fp => state.logId(fp.id)).mkString(",")}]")

	val targetId = common.thoughts(targetOrder).id().orElse(state.deck(targetOrder).id())

	targetId match
		case Some(id) if !focusPoss.exists(_.id == id) =>
			if action.giver == state.ourPlayerIndex then
				(None, game)
			else
				Log.highlight(Console.YELLOW, s"finding own!")
				val conns = connect(ctx, targetOrder, id, unknown, findOwn = true)

				if conns.isEmpty then
					Log.warn(s"targeting an unplayable card!")
					(None, game)
				else
					val newFps = focusPoss :+ FocusPossibility(id, conns.get, ClueInterp.Play)
					(Some(ClueInterp.Play), resolvePlay(ctx, targetOrder, newFps, targetId))

		case _ =>
			(Some(ClueInterp.Play), resolvePlay(ctx, targetOrder, focusPoss, targetId))

def resolvePlay(ctx: ClueContext, targetOrder: Int, focusPoss: Seq[FocusPossibility], targetId: Option[Identity]): RefSieve =
	val ClueContext(_, game, action) = ctx
	val state = game.state
	val ClueAction(giver = giver, list = list, target = clueTarget, clue = _) = action
	val matchedFps = focusPoss.filter(fp => targetId.exists(_.matches(fp.id)))

	val initial = (game, Set[Int]())
	matchedFps.flatMap(_.connections).foldLeft(initial) { case ((acc, modified), conn) =>
		val order = conn.order
		val newGame = acc.withThought(order): t =>
			val newInferred = if modified.contains(order) then
				t.inferred.union(conn.ids)
			else
				t.inferred.intersect(conn.ids)
			t.copy(inferred = newInferred)

		.when(_ => conn.isInstanceOf[FinesseConn]):
			_.withMeta(order):
				_.copy(status = CardStatus.Finessed, by = Some(action.giver))
				.reason(state.turnCount)
				.signal(state.turnCount)
			.withThought(order)(_.copy(oldInferred = game.common.thoughts(order).inferred.toOpt))

		(newGame, modified + order)
	}._1

	.pipe: g =>
		g.copy(
			waiting = g.waiting :++ focusPoss.collect:
				case fp if fp.connections.nonEmpty => WaitingConnection(
					fp.connections,
					giver,
					clueTarget,
					g.state.turnCount,
					targetOrder,
					fp.id,
					symmetric = targetId.exists(!(_).matches(fp.id))
				)
		)

	.withThought(targetOrder): t =>
		val poss = IdentitySet.from(focusPoss.map(_.id))
		t.copy(
			inferred = t.inferred.intersect(poss),
			infoLock = t.possible.intersect(poss).toOpt
		)

	.withMeta(targetOrder): m =>
		m.copy(
			focused = m.focused || list.contains(targetOrder),
			status = CardStatus.CalledToPlay,
			by = Some(action.giver))
		.reason(game.state.turnCount)
		.signal(game.state.turnCount)

def interpretLockedClue(ctx: ClueContext) =
	val ClueContext(prev, game, action) = ctx
	val state = game.state
	val ClueAction(giver, target, list, clue) = action

	val lhPtd = state.hands(target).find(!game.isSaved(_))
	val prevTrash = game.common.thinksTrash(game, target)

	def writeLhPtd(g: RefSieve) =
		lhPtd.fold(g): order =>
			Log.info(s"writing locked hand ptd for $order")
			g.withMeta(order):
				_.copy(status = CardStatus.PermissionToDiscard, by = Some(giver))
					.reason(state.turnCount)
					.signal(state.turnCount)

	Log.info(s"interpreting locked clue!")

	if clue.kind == ClueKind.Rank then
		if prevTrash.nonEmpty then
			Log.info(s"target had previous trash $prevTrash, rank stall")
			game.withMove(ClueInterp.Stall)
		else if list.exists(!prev.state.deck(_).clued) then
			refDiscard(ctx)	match
				case (Some(ClueInterp.Stall), _) => writeLhPtd(game).withMove(ClueInterp.Stall)
				case (interp, newGame) => newGame.withMove(interp.get)
		else
			writeLhPtd(game).withMove(ClueInterp.Stall)
	else
		val slot1 = state.hands(target).head

		game.cond(_ => list.contains(slot1)) {
			_.withThought(slot1): t =>
				val delayedPlays = game.common.hypoStacks.zipWithIndex.map((stack, suitIndex) => Identity(suitIndex, stack + 1)).filter(state.isUseful)
				t.copy(inferred = t.inferred.intersect(delayedPlays))
			.withMeta(slot1):
				_.copy(status = CardStatus.CalledToPlay, by = Some(giver))
					.reason(state.turnCount)
					.signal(state.turnCount)
			.withMove(ClueInterp.Play)
			.tap: g =>
				Log.info(s"slot 1 play on $slot1, new infs [${g.common.strInfs(g.state, slot1)}]")
		} {
			_.withMove(ClueInterp.Stall)
		}
		.cond(_.common.thinksTrash(game, target).nonEmpty) { g =>
			Log.info(s"target had previous trash $prevTrash, colour stall")
			g
		} {
			writeLhPtd
		}
