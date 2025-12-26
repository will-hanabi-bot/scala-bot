package scala_bot.refSieve

import scala_bot.basics._
import scala_bot.utils._
import scala_bot.logger.Log

import scala.util.chaining.scalaUtilChainingOps

def determineFocus(prev: RefSieve, game: RefSieve, action: ClueAction, push: Boolean, right: Boolean = false) =
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

def refPlay(prev: RefSieve, game: RefSieve, action: ClueAction, right: Boolean = false): (Option[ClueInterp], RefSieve) =
	val (common, state) = (game.common, game.state)
	val clueTarget = action.target
	val hand = state.hands(clueTarget)
	val newlyTouched = action.list.filter(o => state.deck(o).clued && !prev.state.deck(o).clued)

	val focus = determineFocus(prev, game, action, push = true, right)
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
		targetPlay(prev, game, action, target) match
			case res @ (None, _) => res
			case (interp, result) =>
				Log.info(s"ref play on ${state.names(clueTarget)}'s slot ${hand.indexOf(target) + 1} (focus $focus) infs ${result.common.strInfs(result.state, target)}")
				(interp, result)

def refDiscard(prev: RefSieve, game: RefSieve, action: ClueAction): (Option[ClueInterp], RefSieve) =
	val state = game.state
	val clueTarget = action.target
	val hand = state.hands(clueTarget)

	val focus = determineFocus(prev, game, action, push = false)
	val targetIndex = hand.indexWhere(o => o < focus && !state.deck(o).clued)

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

def targetPlay(prev: RefSieve, game: RefSieve, action: ClueAction, targetOrder: Int): (Option[ClueInterp], RefSieve) =
	val (common, state) = (game.common, game.state)
	val unknown = common.thoughts(targetOrder).id(infer = true, symmetric = true).isEmpty

	val focusPoss =
		for
			inf   <- common.thoughts(targetOrder).inferred if visibleFind(state, common, inf, infer = true, excludeOrder = targetOrder).isEmpty
			conns <- connect(prev, game, targetOrder, inf, action, unknown)
		yield
			FocusPossibility(inf, conns, ClueInterp.Play)

	Log.info(s"focus possibilities [${focusPoss.map(fp => state.logId(fp.id)).mkString(",")}]")

	val targetId = common.thoughts(targetOrder).id().orElse(state.deck(targetOrder).id())

	targetId match
		case Some(id) if !focusPoss.exists(_.id == id) =>
			lazy val conns = connect(prev, game, targetOrder, id, action, unknown, findOwn = true)
			if action.giver == state.ourPlayerIndex then
				(None, game)
			else if conns.isEmpty then
				Log.warn(s"targeting an unplayable card!")
				(None, game)
			else
				val newFps = focusPoss :+ FocusPossibility(id, conns.get, ClueInterp.Play)
				(Some(ClueInterp.Play), resolvePlay(game, action, targetOrder, newFps, targetId))

		case _ =>
			(Some(ClueInterp.Play), resolvePlay(game, action, targetOrder, focusPoss, targetId))

def resolvePlay(game: RefSieve, action: ClueAction, targetOrder: Int, focusPoss: Seq[FocusPossibility], targetId: Option[Identity]): RefSieve =
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
				 _.copy(
					status = CardStatus.Finessed,
					by = Some(action.giver)
				).reason(game.state.turnCount)
			.withThought(order)(_.copy(oldInferred = Some(game.common.thoughts(order).inferred)))

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
			infoLock = Some(t.possible.intersect(poss))
		)

	.withMeta(targetOrder): m =>
		m.copy(
			focused = m.focused || list.contains(targetOrder),
			status = CardStatus.CalledToPlay,
			by = Some(action.giver))
		.reason(game.state.turnCount)
