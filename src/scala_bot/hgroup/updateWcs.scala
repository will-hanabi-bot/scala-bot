package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.utils._
import scala_bot.logger.Log

import scala.util.chaining.scalaUtilChainingOps
import scala_bot.utils.playersUntil

enum UpdateResult:
	case Remove
	case Complete
	case Keep
	case Advance(nextIndex: Int, skipped: Boolean = false)
	case Demonstrated(nextIndex: Option[Int], skipped: Boolean = false)
	case AmbiguousPassback
	case SelfPassback

def checkRevealLayer(prev: HGroup, game: HGroup, action: ClueAction) =
	val state = game.state
	val ClueAction(giver, target, list, clue) = action

	val revealed = state.hands(target).find: o =>
		prev.isBlindPlaying(o) &&
		!prev.common.thoughts(o).reset &&
		game.common.thoughts(o).reset

	// If revealed a layered finesse, replay with this future knowledge
	revealed.flatMap: o =>
		Log.highlight(Console.CYAN, s"revealed layered finesse on $o! (expected ${prev.common.strInfs(state, o)})")
		game.copy(
			future = game.future.updated(o, game.common.thoughts(o).possible)
		)
		.replay
		.toOption

def stompedWc(prev: HGroup, game: HGroup, action: Action, wc: WaitingConnection) =
	action.matches:
		case action @ ClueAction(giver, target, list, clue) if wc.target != giver =>
			val focus = game.determineFocus(prev, action).focus

			wc.connections.exists: conn =>
				!(conn.hidden && conn.reacting == giver) &&		// Allow a hidden player to stomp, since they don't know
				conn.ids.forall(game.state.deck(focus).id().contains)

def unplayableAlt(game: HGroup, action: Action, wc: WaitingConnection) =
	val (order, reacting) = (wc.currConn.order, wc.currConn.reacting)

	game.waiting.find: w =>
		w != wc && w.focus == wc.focus &&
		// We can't defer if we know that this finesse is symmetric
		!(w.symmetric && reacting == game.state.ourPlayerIndex) &&
		// We can only allow a defer if the reacting player has to wait for someone else, or they already tried to play
		(w.currConn.reacting != reacting || action.isInstanceOf[PlayAction]) &&
		w.connections.exists: conn =>
			conn.order == order &&
			conn.ids.exists: i =>
				!game.state.isPlayable(i) ||
				action.matches:
					case PlayAction(_, _, suitIndex, rank) => i.prev.contains(Identity(suitIndex, rank))

private def revert(g: HGroup, order: Int, ids: List[Identity]) =
	val newInferred = g.common.thoughts(order).inferred.difference(ids)
	// Log.info(s"reverting, removing ${ids.map(g.state.logId)} from $order: now [${newInferred.fmt(g.state)}]")

	if newInferred.isEmpty then
		g.withThought(order): t =>
			t.copy(
				inferred = t.oldInferred.mapO(_.intersect(t.possible)).getOrElse(t.possible),
				oldInferred = IdentitySetOpt.empty
			)
		.withMeta(order)(_.copy(status = CardStatus.None))
	else
		g.withThought(order)(_.copy(inferred = newInferred))

def refreshWCs(prev: HGroup, game: HGroup, action: Action, beforeClueInterp: Boolean = false): HGroup =
	case class Struct(
		newGame: HGroup = game.copy(waiting = Nil),
		wcs: List[WaitingConnection] = Nil,
		toRemove: List[Connection] = Nil,
		remFocuses: Set[Int] = Set.empty,
		demos: List[WaitingConnection] = Nil
	)

	val Struct(newGame, newWCs, toRemove, remFocuses, demos) = game.waiting.foldRight(Struct()) { case (wc, struct) =>
		val conns = wc.connections
		val res = updateWc(prev, game, action, wc, beforeClueInterp)

		res match
			case UpdateResult.Keep =>
				struct.copy(wcs = wc +: struct.wcs)

			case UpdateResult.Advance(nextIndex, skipped) =>
				struct.copy(
					wcs = wc.copy(connections = conns.drop(nextIndex)) +: struct.wcs,
					toRemove = if !skipped then struct.toRemove else conns.take(nextIndex) ++: struct.toRemove,
				)

			case UpdateResult.AmbiguousPassback =>
				struct.copy(wcs = wc.copy(ambiguousPassback = true) +: struct.wcs)

			case UpdateResult.SelfPassback =>
				struct.copy(wcs = wc.copy(selfPassback = true) +: struct.wcs)

			case UpdateResult.Demonstrated(nextIndex, skipped) =>
				struct.copy(
					wcs = nextIndex.fold(struct.wcs)(i => wc.copy(connections = conns.drop(i)) +: struct.wcs),
					toRemove = if !skipped then struct.toRemove else nextIndex.fold(conns)(conns.take) ++: struct.toRemove,
					demos = nextIndex.fold(wc)(i => wc.copy(connections = conns.drop(i))) +: struct.demos
				)
			case UpdateResult.Remove =>
				struct.copy(
					toRemove = PromptConn(-1, wc.focus, wc.inference) +: (if wc.symmetric || wc.ambiguousSelf then Nil else conns) ++: struct.toRemove,
					remFocuses = struct.remFocuses + wc.focus,
				)
			case UpdateResult.Complete => struct
	}

	val demoIds = demos.foldLeft(Map.empty[Int, IdentitySet]): (acc, wc) =>
		val order = wc.focus
		acc + (order -> (acc.getOrElse(order, IdentitySet.empty).union(wc.inference)))

	val (retainWCs, nonDemoedWCs) = newWCs.partition: wc =>
		demoIds.get(wc.focus).isEmpty || demos.contains(wc)
		// demoIds.get(wc.focus).forall(_.contains(wc.inference))

	demoIds.foldLeft(newGame) { case (acc, (order, ids)) =>
		acc.withThought(order): t =>
			val newInferred = t.inferred.intersect(ids)
			t.copy(
				inferred = newInferred,
				infoLock = newInferred.toOpt
			)
		.withXMeta(order)(_.copy(fStatus = None))
	}
	.pipe: g =>
		demos.foldLeft(g): (acc, wc) =>
			// Demonstrating a hidden connection means they must play all the hidden cards + the actual one
			if wc.connections(0).hidden then
				val nonHiddenIndex = wc.connections.indexWhere(!_.hidden)
				(1 to nonHiddenIndex).foldLeft(acc) { (a, i) =>
					a.withXMeta(wc.connections(i).order)(_.copy(fStatus = None))
				}
			else
				acc
	.pipe: g =>
		remFocuses.foldLeft(g): (acc, focus) =>
			val sameFocusWcs = retainWCs.filter(_.focus == focus)

			if sameFocusWcs.nonEmpty && sameFocusWcs.forall(wc => wc.symmetric || wc.ambiguousSelf) then
				val turn = sameFocusWcs.head.turn
				val action = acc.state.actionList(turn).collectFirst { case a: ClueAction => a }.get
				Log.info(s"assigning previously-ambiguous connections to ${sameFocusWcs.map(w => g.state.logId(w.inference))}")
				assignConns(acc, action, sameFocusWcs.map(wc => FocusPossibility(wc.inference, wc.connections, ClueInterp.Play)), focus)
			else
				acc
	.pipe: g =>
		val allRemove = toRemove ++ nonDemoedWCs.filterNot(wc => wc.symmetric || wc.ambiguousSelf).flatMap(_.connections)
		allRemove.foldLeft(g): (acc, conn) =>
			val shared = retainWCs.exists: wc =>
				(wc.focus == conn.order && conn.ids.forall(_ == wc.inference)) ||
				(wc.connections.exists(c => c.order == conn.order && c.ids.length == conn.ids.length && c.ids.forall(conn.ids.contains)))

			if shared || conn.isInstanceOf[KnownConn] || conn.isInstanceOf[PlayableConn] then
				acc
			else
				revert(acc, conn.order, conn.ids)
	.copy(waiting = retainWCs)
	.elim

def updateWc(prev: HGroup, game: HGroup, action: Action, wc: WaitingConnection, beforeClueInterp: Boolean): UpdateResult =
	if wc.connections.isEmpty then
		return UpdateResult.Complete

	val state = game.state
	val reacting = wc.currConn.reacting

	if !beforeClueInterp then
		Log.info(s"waiting for connecting ${wc.currConn.ids.map(state.logId).mkString(",")} (${wc.currConn.order}) ${wc.currConn.kind} for ${state.logId(wc.inference)}${if wc.ambiguousSelf then " (ambiguous self)" else ""}")

	val impossibleConn = wc.connections.find: conn =>
		game.common.thoughts(conn.order).possible.intersect(conn.ids).isEmpty

	lazy val impossibleFocus = wc.connections.headOption.forall(!_.isInstanceOf[PositionalConn]) &&
		!game.common.thoughts(wc.focus).possible.contains(wc.inference)

	if impossibleConn.isDefined then
		val c = impossibleConn.get
		Log.warn(s"future connection depends on ${c.order} having ids ${c.ids.map(state.logId).mkString(",")}, removing")
		UpdateResult.Remove

	else if impossibleFocus then
		Log.warn(s"connection depends on focus ${wc.focus} having id ${state.logId(wc.inference)}, removing")
		UpdateResult.Remove

	else if stompedWc(prev, game, action, wc) then
		Log.warn(s"connection was clued directly, cancelling")
		UpdateResult.Remove

	else if wc.ambiguousSelf || beforeClueInterp then
		UpdateResult.Keep

	else if otherPlay(game, action, wc) then
		Log.highlight(Console.CYAN, s"${state.names(action.playerIndex)} played connecting card, continuing")
		resolveOtherPlay(game, wc)

	// The turn we were waiting for
	else if action.playerIndex == reacting then
		if state.hands(reacting).contains(wc.currConn.order) then
			val altWc = unplayableAlt(game, action, wc)

			if altWc.isDefined then
				Log.highlight(Console.CYAN, s"not all possibilities playable ${game.state.logConns(altWc.get.connections)}, waiting")
				UpdateResult.Keep
			else
				resolveRetained(prev, game, action, wc)
		else
			action match
				case PlayAction(_, order, _, _) =>
					resolvePlayed(game, wc, order)
				case d: DiscardAction if d.failed =>
					resolvePlayed(game, wc, d.order)
				case _ =>
					Log.highlight(Console.YELLOW, s"waiting card ${state.logId(wc.currConn.order)} discarded??")
					UpdateResult.Remove

	else
		UpdateResult.Keep

def otherPlay(game: HGroup, action: Action, wc: WaitingConnection) =
	val thought = game.me.thoughts(wc.currConn.order)

	// The card needs to match our thoughts as well as the hypothesized identity in the connection
	action.matches { case PlayAction(_, order, suitIndex, rank) =>
		order != wc.currConn.order &&
		thought.matches(Identity(suitIndex, rank), infer = true)
	} &&
	// currConn.ids.length == 1 &&
	// thought.matches(currConn.ids.head, infer = true) &&
	!wc.currConn.hidden	// Don't advance if the real connection is layered, because that player won't skip

def resolveOtherPlay(game: HGroup, wc: WaitingConnection) =
	val nextIndex = wc.getNextIndex(game.state)

	if wc.currConn.isInstanceOf[FinesseConn] || game.level < Level.IntermediateFinesses then
		UpdateResult.Demonstrated(nextIndex, skipped = true)
	else
		nextIndex.fold(UpdateResult.Complete):
			UpdateResult.Advance(_, skipped = true)

def resolveRetained(prev: HGroup, game: HGroup, action: Action, wc: WaitingConnection): UpdateResult =
	val state = game.state
	val (reacting, connOrder) = (wc.currConn.reacting, wc.currConn.order)
	val name = state.names(reacting)

	val newFinesseQueued = wc.connections.find: conn =>
		conn.isInstanceOf[FinesseConn] &&
		state.hands(conn.reacting).exists: o =>
			// A newer finesse exists on this player that is not part of the same suit.
			game.isBlindPlaying(o) &&
			game.xmeta(o).turnFinessed.exists(t => game.xmeta(conn.order).turnFinessed.exists(t > _)) &&
			!game.common.thoughts(o).inferred.exists(_.suitIndex == conn.ids.head.suitIndex)

	val unplayableWcs = prev.waiting.filter: wc =>
		wc.connections.exists: conn =>
			conn.order == connOrder &&
			conn.ids.exists: i =>
				!state.isBasicTrash(i) && !prev.state.isPlayable(i)

	if unplayableWcs.nonEmpty then
		Log.info(s"$name didn't play into unplayable ${wc.currConn.kind}: ${unplayableWcs.map(w => s"${state.logId(w.inference)} ${w.connections.find(_.ids.exists(i => !state.isBasicTrash(i) && !prev.state.isPlayable(i))).map(_.ids.map(state.logId))}")}")
		return UpdateResult.Keep

	val hiddenWc = game.waiting.find: wc =>
		// This is a hidden connection that is not currently revealed
		wc.connections.nonEmpty && wc.connections.tail.exists(c => c.order == connOrder && c.hidden)

	if hiddenWc.nonEmpty then
		Log.info(s"hidden connection as part of ${state.logConns(hiddenWc.get.connections)}")
		return UpdateResult.Keep

	// val discarded = game.lastActions(reacting).exists(_.isInstanceOf[DiscardAction]) &&
	// 	game.dcStatus == DcStatus.None && newFinesseQueued.isEmpty

	// if (discarded)
	// 	Log.info(s"discarded with a waiting connection, removing ${state.logId(wc.inference)}")
	// 	// Does this need to be a rewind instead?
	// 	return UpdateResult.Remove

	val missedReaction = newFinesseQueued.isDefined ||
		wc.currConn.matches:
			case _: FinesseConn => true
			case _: PromptConn => true

	if !missedReaction then
		return UpdateResult.Keep

	val olderUnplayableFinesse =
		game.level >= Level.IntermediateFinesses &&
		wc.currConn.isInstanceOf[FinesseConn] &&
		state.hands(reacting).zipWithIndex.exists: (o, i) =>
			o < connOrder &&
			!{
				state.deck(o).clued ||
				state.hands(reacting).drop(i).forall(state.deck(_).clued)
			} &&
			game.isBlindPlaying(o) &&
			game.xmeta(o).turnFinessed.get < wc.turn &&
			game.common.thoughts(o).inferred.exists: i =>
				!state.isBasicTrash(i) && !state.isPlayable(i)

	if olderUnplayableFinesse then
		Log.info(s"allowing $name to clue, need to wait for unplayable older finesse")
		return UpdateResult.Keep

	action match
		case _: ClueAction =>
			if wc.currConn.isInstanceOf[PromptConn] && game.lastMove.exists(m => m != ClueInterp.Mistake && m != ClueInterp.Stall) then
				Log.info(s"allowing $name to defer a prompt by giving a useful clue ${game.lastMove.get}")
				return UpdateResult.Keep

			if game.level >= Level.IntermediateFinesses && game.importantAction(reacting) then
				if wc.currConn.isPossiblyBluff then
					Log.info(s"$name not allowed to defer a potential bluff")
				else
					Log.info(s"allowing $name to defer a finesse for an important clue")
					return UpdateResult.Keep

		case PlayAction(_, order, _, _) =>
			if game.isBlindPlaying(order) then
				if game.xmeta(order).turnFinessed.get < wc.turn then
					Log.info(s"$name played into older finesse, continuing to wait")
					return UpdateResult.Keep

				if game.meta(connOrder).hidden && state.deck(connOrder).clued then
					Log.info(s"$name jumped ahead in layered finesse, continuing to wait")
					return UpdateResult.Keep
			// else
			// 	Log.info(s"$name played into something else, continuing")
			// 	return UpdateResult.Keep
		case _ => ()

	val passback = wc.currConn.matches:
		case f: FinesseConn =>
			lazy val nonHiddenConns = wc.connections.count: conn =>
				!conn.hidden &&
				conn.reacting == wc.currConn.reacting

			reacting != state.ourPlayerIndex &&		// can't pass back to ourselves
			nonHiddenConns > 1 &&					// they need to play more than 1 card
			nonHiddenConns == wc.inference.rank - state.playStacks(wc.inference.suitIndex) - 1 &&	// they have all required cards
			!f.ambiguousPassback

	if passback then
		Log.info(s"$name didn't play into ${wc.currConn.kind} but they need to play multiple non-hidden cards, passing back")
		return UpdateResult.AmbiguousPassback

	val allowableHesitation = if reacting != wc.target then None else
		// Find all waiting connections using this order OR self-identities, and merge their possible identities
		val linkedIds = (for
			w     <- game.waiting if w != wc && w.focus == wc.focus
			conns <- w.connections if conns.order == connOrder || conns.reacting == w.target
		yield
			conns.ids
		).flatten

		playersUntil(state.numPlayers, state.nextPlayerIndex(reacting), wc.giver).find: playerIndex =>
			game.findFinesse(playerIndex).exists: finesse =>
				linkedIds.exists(game.common.thoughts(finesse).inferred.contains)

	if allowableHesitation.isDefined then
		Log.highlight(Console.CYAN, s"${state.names(reacting)} didn't play but allowable hesitation on ${state.names(allowableHesitation.get)}")
		return UpdateResult.SelfPassback

	Log.highlight(Console.CYAN, s"$name didn't react with a waiting connection, removing ${state.logId(wc.inference)}")
	UpdateResult.Remove

def resolvePlayed(game: HGroup, wc: WaitingConnection, play: Int): UpdateResult =
	val state = game.state

	Log.info(s"waiting card ${state.logId(play)} played")

	val nextIndex = wc.getNextIndex(state)

	val missedReaction = wc.currConn.matches:
		case _: FinesseConn => true
		case _: PromptConn => true

	lazy val stompedFinesse =
		val card = state.deck(wc.currConn.order)

		card.clued && card.clues.last.turn > wc.turn && (
			game.meta(wc.currConn.order).focused || (
				game.common.thinksPlayables(game, wc.currConn.reacting).isEmpty &&
				game.common.thoughts(wc.currConn.order).oldPossible.get.forall: i =>
					state.isPlayable(i) || card.matches(i, assume = true) || state.isBasicTrash(i)
			)
		)

	if missedReaction then
		if wc.currConn.isInstanceOf[FinesseConn] && stompedFinesse then
			Log.info(s"connecting card was stomped on, not confirming finesse")

			if nextIndex.exists(wc.connections(_).reacting == wc.currConn.reacting) then
				Log.info(s"connection requires a self blind play, removing due to occam's razor")
				return UpdateResult.Remove

		else if wc.currConn.isInstanceOf[PromptConn] || wc.currConn.isInstanceOf[FinesseConn] || game.level < Level.IntermediateFinesses then
			return UpdateResult.Demonstrated(nextIndex)

	nextIndex.fold(UpdateResult.Complete)(UpdateResult.Advance(_))
