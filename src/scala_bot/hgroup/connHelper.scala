package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.utils._
import scala_bot.logger.Log

import scala.util.chaining.scalaUtilChainingOps

def assignConns(game: HGroup, action: ClueAction, fps: Seq[FocusPossibility], focus: Int, ambiguousOwn: Seq[FocusPossibility] = Nil) =
	val state = game.state
	val ClueAction(giver, target, _, clue) = action

	val bluffPlayables = fps.collect:
		case fp if fp.isBluff => fp.connections.collect:
			case conn if conn.isInstanceOf[PlayableConn] => conn.order
	// 'Playable' cards used in every bluff connection
	val mustBluffPlayables = bluffPlayables.headOption.fold(Nil):
		_.filter(p => bluffPlayables.forall(_.contains(p)))

	fps.foldLeft((Set.empty[Int], game)) { case ((m, a), fp) =>
		// Don't assign symmetric/ambiguous/save connections
		if fp.symmetric || fp.ambiguous || fp.save then
			(m, a)
		else
			fp.connections.zipWithIndex.foldLeft((m, a)) { case ((modified, acc), (conn, connI)) =>
				if conn.isInstanceOf[PlayableConn] && fp.isBluff && !mustBluffPlayables.contains(conn.order) then
					(modified, acc)
				else
					// Log.info(s"assigning connection ${state.logConn(conn)}")
					val isBluff = conn.matches { case f: FinesseConn => f.bluff }

					// val playableIds = for
					// 	(stack, i) <- acc.common.hypoStacks.zipWithIndex
					// 	id = Identity(i, stack + 1) if !acc.common.isTrash(acc, id, conn.order)
					// yield
					// 	id

					val currPlayableIds =
						for
							(stack, i) <- state.playStacks.zipWithIndex
							id = Identity(i, stack + 1) if !acc.common.isTrash(acc, id, conn.order)
						yield
							id

					val isUnknownPlayable = conn.matches { case c: PlayableConn => c.linked.length > 1 }
					val thought = acc.common.thoughts(conn.order)

					val newInferred =
						if conn.matches { case f: PlayableConn => f.insertingInto.isDefined } then
							thought.inferred.union(conn.ids)

						else if conn.hidden then
							IdentitySet.from(conn.ids)

						else if isBluff then
							thought.inferred.intersect(currPlayableIds)

						else if modified.contains(conn.order) then
							thought.inferred.union(conn.ids)

						else if game.meta(conn.order).status != CardStatus.Finessed && !isUnknownPlayable then
							IdentitySet.from(conn.ids)

						else
							thought.inferred

					val newGame = acc.withThought(conn.order): t =>
						t.copy(inferred = newInferred, oldInferred = t.inferred.toOpt)
					.pipe: g =>
						val idUncertain =
							conn.reacting == state.ourPlayerIndex &&
							!conn.isInstanceOf[KnownConn] &&
							// There's an older card in our hand that allows for a swap
							conn.ids.exists: i =>
								state.ourHand.exists: o =>
									o < conn.order && g.me.thoughts(o).possible.contains(i)
							&&
							// Playable in some other suit
							thought.possible.exists: i =>
								i.suitIndex != conn.ids.head.suitIndex && i.rank <= g.common.hypoStacks(i.suitIndex) + 1

						val finesseIds = // Option.when(idUncertain):
							if isBluff then currPlayableIds else
								// Allow connecting on self blind plays (except hidden ones)
								state.hands(conn.reacting).foldLeft(state.playableSet.toList): (acc, order) =>
									if !g.isBlindPlaying(order) || g.meta(order).hidden then acc else
										acc.foldLeft(acc): (a, i) =>
											i.next.fold(a)(_ +: a)
								.filter(!state.isBasicTrash(_))
						.pipe(IdentitySet.from)

						val maybeFinessed =
							giver != state.ourPlayerIndex &&
							conn.reacting != state.ourPlayerIndex &&
							conn.matches:
								case _: FinesseConn =>
									// Finesse that could be ambiguous
									fps.length > 1 || ambiguousOwn.length > 1

								case _: PromptConn => g.me.thoughts(focus).possible.exists: i =>
									// Could be hidden? (TODO: Investigate why this is here.)
									i.suitIndex != conn.ids.head.suitIndex
							&& !(
								// All ids critical, can't be ambiguous? (TODO: Are these the same condition?)
								(conn.ids.forall(state.isCritical) && state.deck(focus).matches(fp.id)) ||
								// Colour finesses are guaranteed if the focus cannot be a finessed identity
								(clue.kind == ClueKind.Colour && conn.ids.forall(!g.me.thoughts(focus).possible.contains(_)))
							)

						val fStatus = Option.when(maybeFinessed):
							if target == state.ourPlayerIndex then FStatus.PossiblyOn else FStatus.PossiblyAmbiguous

						if fStatus != None then
							Log.highlight(Console.GREEN, s"setting finesse status to $fStatus on ${conn.order}! ${finesseIds.fmt(state)}")

						g.copy(
							xmeta = g.xmeta.updated(conn.order, g.xmeta(conn.order).copy(
								idUncertain = idUncertain,
								fStatus = fStatus,
								finesseIds = Some(finesseIds))))
					.pipe: g =>
						conn match
							case f: FinesseConn =>
								val status =
									if f.possiblyBluff then
										if conn.reacting == state.ourPlayerIndex then
											CardStatus.FMaybeBluffed
										else
											CardStatus.MaybeBluffed
									else if f.bluff then
										CardStatus.Bluffed
									else
										CardStatus.Finessed

								g.withMeta(conn.order): m =>
									m.copy(status = status, hidden = f.hidden)
								.withXMeta(conn.order): x =>
									x.copy(turnFinessed = x.turnFinessed.orElse(Some(state.turnCount)))

							case c: PlayableConn if isUnknownPlayable =>
								val target = fp.connections.lift(connI + 1).map(_.order).getOrElse(focus)
								val existingLink = g.common.links.existsM:
									case Link.Promised(orders, id, target) =>
										id == c.id && orders.toSet == c.linked.toSet

								if existingLink then g else
									Log.info(s"adding promised link ${c.linked} ${conn.ids.map(state.logId).mkString(",")} $target")
									g.copy(
										common = g.common.copy(
											links = Link.Promised(c.linked, c.id, target) +: g.common.links))
							case _ => g
					.withMeta(conn.order)(_.reason(state.turnCount))

					// TODO: Finesses while finessed?
					(modified + conn.order, newGame)
			}
		}._2

def importantFinesse(state: State, action: ClueAction, fps: Seq[FocusPossibility]) =
	val ClueAction(giver, target, _, _) = action

	fps.exists { fp =>
		val conns = fp.connections

		@annotation.tailrec
		def loop(playerIndex: Int): Boolean =
			// Looped around
			if playerIndex == giver then
				false

			// Clue must be given before the first finessed player, otherwise position may change.
			else if conns.existsM { case f: FinesseConn => f.reacting == playerIndex } then
				true

			// Target can't clue themselves, unknown conns can't clue
			else if playerIndex == target || conns.exists(c => c.reacting == playerIndex && !c.isInstanceOf[KnownConn]) then
				loop(state.nextPlayerIndex(playerIndex))

			else
				false	// This player could give the finesse

		loop(state.nextPlayerIndex(giver))
	}

def urgentSave(ctx: ClueContext): Boolean =
	val ClueContext(prev, game, action) = ctx
	val state = game.state

	// Log.info(s"checking if ${state.names(action.giver)} performed an urgent save")

	if !ctx.focusResult.chop then
		return false

	val earlyGameClue = game.earlyGameClue(action.target)

	if earlyGameClue.isDefined then
		// Log.info(s"${state.names(action.target)} could clue ${earlyGameClue.get.fmt(state)}, not urgent save")
		return false

	@annotation.tailrec
	def loop(hypoState: State, playerIndex: Int): Boolean =
		if playerIndex == action.target then
			return true

		def getFinessedOrder(includeHidden: Boolean) =
			state.hands(playerIndex).filter: o =>
				game.isBlindPlaying(o) &&
				(includeHidden || !game.meta(o).hidden) &&
				game.copy(state = hypoState).common.orderPlayable(game, o, excludeTrash = true)
			.minByOption(game.xmeta(_).turnFinessed.getOrElse(99))

		getFinessedOrder(includeHidden = false) match
			case None => false
			case Some(_) =>
				val order = getFinessedOrder(includeHidden = true).get
				game.common.thoughts(order).id(infer = true) match
					case None => loop(hypoState, state.nextPlayerIndex(playerIndex))
					case Some(id) => loop(hypoState.withPlay(id), state.nextPlayerIndex(playerIndex))

	loop(game.state, state.nextPlayerIndex(action.giver))

def resolveClue(ctx: ClueContext, fps: Seq[FocusPossibility], ambiguousOwn: Seq[FocusPossibility] = Nil) =
	val ClueContext(prev, game, action) = ctx
	val state = game.state
	val ClueAction(giver, target, _, _) = action
	val FocusResult(focus, chop, _) = ctx.focusResult

	val symmetricFps =
		if target == state.ourPlayerIndex || fps.exists(_.save) then
			Nil
		else
			Log.highlight(Console.YELLOW, "finding symmetric connections!")
			val symmetricFps =
				val looksDirect = game.common.thoughts(focus).id(symmetric = true).isEmpty &&
					fps.exists: fp =>
						game.players(target).thoughts(focus).possible.contains(fp.id) &&
						fp.connections.forall: c =>
							c.isInstanceOf[KnownConn] ||
							(c.isInstanceOf[PlayableConn] && c.reacting != state.ourPlayerIndex)

				game.common.thoughts(focus).inferred.filter: inf =>
					visibleFind(state, game.common, inf, infer = true, excludeOrder = focus).isEmpty &&
					!fps.exists(_.id == inf)
				.flatMap:
					connect(ctx, _, looksDirect, thinksStall = Set(), findOwn = Some(target))

			occamsRazor(game, symmetricFps, target, focus)

	val ambiguousFps = if game.level < Level.IntermediateFinesses || giver == state.ourPlayerIndex || fps.exists(_.save) then Nil else
		Log.highlight(Console.YELLOW, "finding ambiguous connections!")

		val ambiguousFps =
			val looksDirect = game.common.thoughts(focus).id(symmetric = true).isEmpty &&
				fps.exists: fp =>
					game.players(target).thoughts(focus).possible.contains(fp.id) &&
					fp.connections.forall: c =>
						c.isInstanceOf[KnownConn] ||
						(c.isInstanceOf[PlayableConn] && c.reacting != state.ourPlayerIndex)

			val poss = game.me.thoughts(focus).id().toList
				.when(_.isEmpty)(_ => game.me.thoughts(focus).inferred.toList)

			poss.filter: inf =>
				visibleFind(state, game.common, inf, infer = true, excludeOrder = focus).isEmpty &&
				!(fps ++ ambiguousOwn).exists: fp =>
					fp.id == inf && {
						fp.connections.forall(_.matches { case _: KnownConn => true ; case _: PlayableConn => true }) ||
						fp.connections.forall(_.reacting == state.ourPlayerIndex)
					}
			.flatMap:
				connect(ctx, _, looksDirect, thinksStall = Set(), preferOwn = true)
			.filter: fp =>
				!(fps ++ ambiguousOwn).exists(_.connections == fp.connections)

		occamsRazor(game, ambiguousFps, target, focus)

	val allFps = fps ++ symmetricFps ++ ambiguousFps
	val simplestFps = occamsRazor(game, allFps.filter(fp => !fp.ambiguous && game.players(target).thoughts(focus).possible.contains(fp.id)), target, focus)
	val fpsToWrite = if simplestFps.forall(_.symmetric) then simplestFps else allFps.filter(!_.ambiguous)

	val stompedWc = !state.inEndgame && game.waiting.exists: wc =>
		!wc.symmetric && !wc.ambiguousSelf &&
		wc.connections.exists: conn =>
			conn.order == focus ||
			(conn.ids.length == 1 && state.deck(focus).matches(conn.ids.head))

	val interp = if state.deck(focus).id().exists(id => !simplestFps.exists(_.id == id)) then
		Log.error(s"resolving clue but focus ${state.logId(focus)} doesn't match simplest fps [${simplestFps.map(fp => state.logId(fp.id)).mkString(",")}]!")
		ClueInterp.Mistake
	else if simplestFps.forall(_.symmetric) then
		Log.error(s"resolving clue but all focus possibilities are symmetric!")
		ClueInterp.Mistake
	else if stompedWc then
		Log.error("cluing a card part of a waiting connection!")
		ClueInterp.Mistake
	else if fps.exists(_.save) then
		ClueInterp.Save
	else
		ClueInterp.Play

	val undoScream =
		interp == ClueInterp.Save &&
		target == state.nextPlayerIndex(giver) &&
		game.dcStatus == DcStatus.Scream &&
		state.numPlayers > 2

	game.withThought(focus): t =>
		val newInferred = t.inferred.intersect(IdentitySet.from(allFps.map(_.id)))
			// If a non-finesse connection exists, the focus can't be a copy of it
			.filter: i =>
				!fps.filterNot(_.symmetric).flatMap(_.connections).existsM:
					case c: KnownConn    => c.id == i
					case c: PlayableConn => c.id == i
					case c: PromptConn   => c.id == i
		Log.highlight(Console.CYAN, s"final infs [${newInferred.fmt(state)}] $focus")
		t.copy(inferred = newInferred, infoLock = newInferred.toOpt)

	.pipe:
		assignConns(_, action, fpsToWrite, focus, ambiguousOwn)

	.pipe:
		allFps.foldLeft(_): (a, fp) =>
			fp.connections.zipWithIndex.foldLeft(a) { case (acc, (conn, i)) => conn match
				case PlayableConn(reacting, order, id, linked, hidden, _) if linked.length > 1 =>
					val playLinks = acc.common.playLinks
					val target = fp.connections.lift(i + 1).map(_.order).getOrElse(focus)
					val existingIndex = playLinks.indexWhere: l =>
						l.orders == linked && l.target == target

					if existingIndex == -1 then
						val link = PlayLink(linked, IdentitySet.single(id), target)
						Log.info(s"adding play link $linked -> $target")
						acc.copy(common = acc.common.copy(playLinks = link +: playLinks))
					else
						val existing = playLinks(existingIndex)
						val newLink = existing.copy(prereqs = existing.prereqs.union(id))
						acc.copy(
							common = acc.common.copy(
								playLinks = playLinks.updated(existingIndex, newLink)
							)
						)
				case _ => acc
			}
	.pipe: g =>
		def requiresWc(fp: FocusPossibility) = fp.connections.exists: c =>
			c.isInstanceOf[PlayableConn] ||
			c.isInstanceOf[PromptConn] ||
			c.isInstanceOf[FinesseConn]

		g.copy(
			waiting = g.waiting ++ allFps.filterNot(_.ambiguous).collect {
				case fp if requiresWc(fp) => WaitingConnection(
					fp.connections,
					giver,
					target,
					state.turnCount,
					focus,
					fp.id,
					symmetric = fp.symmetric
				)
			} ++ (ambiguousOwn ++ allFps.filter(_.ambiguous)).filter(_.connections.nonEmpty).map: fp =>
				WaitingConnection(
					fp.connections,
					giver,
					target,
					state.turnCount,
					focus,
					fp.id,
					ambiguousSelf = true
				),
			lastMove = Some(interp),
			cluedOnChop = if chop then g.cluedOnChop + focus else g.cluedOnChop
		)
	.when(_ => undoScream): g =>
		Log.highlight(Console.CYAN, "undoing sdcm due to immediate save clue!")

		// Must be leftmost chop moved
		state.hands(giver).find(game.meta(_).cm).fold(g): oldChop =>
			g.withMeta(oldChop)(_.copy(status = CardStatus.None))

	.when(g => importantFinesse(g.state, action, fps) || urgentSave(ctx)): g =>
		Log.highlight(Console.YELLOW, s"important action for ${g.state.names(giver)}!")
		g.copy(importantAction = g.importantAction.updated(giver, true))

	.withMeta(focus)(_.copy(focused = true))
