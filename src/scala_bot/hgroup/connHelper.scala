package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.utils._
import scala_bot.logger.Log

import scala.util.chaining.scalaUtilChainingOps

def assignConns(game: HGroup, action: ClueAction, fps: List[FocusPossibility], focus: Int, ambiguousOwn: List[FocusPossibility] = Nil) =
	val state = game.state
	val ClueAction(giver, target, _, clue) = action

	val bluffPlayables = fps.collect {
		case fp if fp.isBluff => fp.connections.collect {
			case conn if conn.isInstanceOf[PlayableConn] => conn.order
		}
	}
	// 'Playable' cards used in every bluff connection
	val mustBluffPlayables = bluffPlayables.headOption.fold(Nil) {
		_.filter(p => bluffPlayables.forall(_.contains(p)))
	}

	fps.foldLeft((Set[Int](), game)) { case ((m, a), fp) =>
		// Don't assign symmetric/save connections
		if (fp.symmetric || fp.save)
			(m, a)
		else
			fp.connections.zipWithIndex.foldLeft((m, a)) { case ((modified, acc), (conn, connI)) =>
				if (conn.isInstanceOf[PlayableConn] && fp.isBluff && !mustBluffPlayables.contains(conn.order))
					(modified, acc)
				else
					// Log.info(s"assigning connection ${state.logConn(conn)}")
					val isBluff = conn.matches { case f: FinesseConn => f.bluff }

					// val playableIds = for
					// 	(stack, i) <- acc.common.hypoStacks.zipWithIndex
					// 	id = Identity(i, stack + 1) if !acc.common.isTrash(acc, id, conn.order)
					// yield
					// 	id

					val currPlayableIds = for
						(stack, i) <- state.playStacks.zipWithIndex
						id = Identity(i, stack + 1) if !acc.common.isTrash(acc, id, conn.order)
					yield
						id

					val isUnknownPlayable = conn.matches { case c: PlayableConn => c.linked.length > 1 }
					val thought = acc.common.thoughts(conn.order)

					val newInferred = {
						if (conn.matches { case f: FinesseConn => f.inserted })
							thought.inferred.union(conn.ids)

						else if (conn.hidden)
							IdentitySet.from(conn.ids)

						else if (isBluff)
							thought.inferred.intersect(currPlayableIds)

						else if (modified.contains(conn.order))
							thought.inferred.union(conn.ids)

						else if (game.meta(conn.order).status != CardStatus.Finessed && !isUnknownPlayable)
							IdentitySet.from(conn.ids)

						else
							thought.inferred
					}

					val newGame = acc.withThought(conn.order) { t => t.copy(
						inferred = newInferred,
						oldInferred = Some(t.inferred)
					)}
					.pipe { g =>
						val idUncertain =
							conn.reacting == state.ourPlayerIndex &&
							!conn.isInstanceOf[KnownConn] &&
							// There's some other card in our hand that allows for a swap
							conn.ids.exists(i => state.ourHand.exists { o =>
								o != conn.order && g.me.thoughts(o).possible.contains(i)
							}) &&
							// Playable in some other suit
							thought.possible.exists { i =>
								i.suitIndex != conn.ids.head.suitIndex && i.rank <= g.common.hypoStacks(i.suitIndex) + 1
							}

						val finesseIds = Option.when(idUncertain) {
							if (isBluff)
								currPlayableIds
							else
								// Allow connecting on own blind plays
								state.ourHand.foldLeft(state.playStacks) { (acc, order) =>
									g.common.thoughts(order).id(infer = true) match {
										case Some(id) if game.isBlindPlaying(order) && acc(id.suitIndex) + 1 == id.rank =>
											acc.updated(id.suitIndex, acc(id.suitIndex) + 1)
										case _ => acc
									}
								}.zipWithIndex.collect {
									case (stack, i) if stack + 1 <= state.maxRanks(i) => Identity(i, stack + 1)
								}
						}.map(IdentitySet.from)

						val maybeFinessed =
							giver != state.ourPlayerIndex &&
							conn.reacting != state.ourPlayerIndex && {
								// Finesse that could be ambiguous
								(conn.isInstanceOf[FinesseConn] && (fps.length > 1 || ambiguousOwn.length > 1)) ||
								// Could be hidden? (TODO: Investigate why this is here.)
								(conn.isInstanceOf[PromptConn] && g.me.thoughts(focus).possible.exists { i =>
									i.suitIndex != conn.ids.head.suitIndex
								})
							} && !{
								// All ids critical, can't be ambiguous? (TODO: Are these the same condition?)
								(conn.ids.forall(state.isCritical) && state.deck(focus).matches(fp.id)) ||
								// Colour finesses are guaranteed if the focus cannot be a finessed identity
								(clue.kind == ClueKind.Colour && conn.ids.forall(!g.me.thoughts(focus).possible.contains(_)))
							}

						g.copy(
							xmeta = g.xmeta.updated(conn.order, g.xmeta(conn.order).copy(
								idUncertain = idUncertain,
								maybeFinessed = maybeFinessed,
								finesseIds = finesseIds
							))
						)
					}
					.pipe { g => conn match {
						case f: FinesseConn =>
							val status =
								if (f.possiblyBluff)
									if (conn.reacting == state.ourPlayerIndex)
										CardStatus.FMaybeBluffed
									else
										CardStatus.MaybeBluffed
								else if (f.bluff)
									CardStatus.Bluffed
								else
									CardStatus.Finessed

							g.withMeta(conn.order) { m => m.copy(
								status = status,
								hidden = f.hidden
							)}
							.withXMeta(conn.order) { x =>
								x.copy(turnFinessed = x.turnFinessed.orElse(Some(state.turnCount)))
							}
						case c: PlayableConn if isUnknownPlayable =>
							val target = fp.connections.lift(connI + 1).map(_.order).getOrElse(focus)
							val existingLink = g.common.links.existsM {
								case Link.Promised(orders, id, target) =>
									id == c.id && orders.toSet == c.linked.toSet
							}

							if (existingLink)
								g
							else
								Log.info(s"adding promised link ${c.linked} ${conn.ids.map(state.logId).mkString(",")} $target")
								g.copy(
									common = g.common.copy(
										links = Link.Promised(c.linked, c.id, target) +: g.common.links
									)
								)
						case _ => g
					}}
					.withMeta(conn.order)(_.reason(state.turnCount))

					// TODO: Finesses while finessed?
					(modified + conn.order, newGame)
			}
		}._2

def importantFinesse(state: State, action: ClueAction, fps: List[FocusPossibility]) =
	val ClueAction(giver, target, _, _) = action

	fps.exists { fp =>
		val conns = fp.connections

		@annotation.tailrec
		def loop(playerIndex: Int): Boolean =
			// Looped around
			if (playerIndex == giver)
				false

			// Clue must be given before the first finessed player, otherwise position may change.
			else if (conns.existsM { case f: FinesseConn => f.reacting == playerIndex })
				true

			// Target can't clue themselves, unknown conns can't clue
			else if (playerIndex == target || conns.exists(c => c.reacting == playerIndex && !c.matches { case _: KnownConn => true }))
				loop(state.nextPlayerIndex(playerIndex))

			else
				false	// This player could give the finesse

		loop(state.nextPlayerIndex(giver))
	}

def urgentSave(ctx: ClueContext): Boolean =
	val ClueContext(prev, game, action) = ctx
	val state = game.state

	// Log.info(s"checking if ${state.names(action.giver)} performed an urgent save ${game.earlyGameClue(action.target).isDefined}")

	val earlyGameClue = game.earlyGameClue(action.target)

	if (earlyGameClue.isDefined)
		// Log.info(s"${state.names(action.target)} could clue ${earlyGameClue.get.fmt(state)}, not urgent save")
		return false

	if (!ctx.focusResult.chop)
		return false

	@annotation.tailrec
	def loop(hypoState: State, playerIndex: Int): Boolean =
		if (playerIndex == action.target)
			return true

		def getFinessedOrder(playerIndex: Int, includeHidden: Boolean) =
			state.hands(playerIndex).filter { o =>
				game.isBlindPlaying(o) &&
				(includeHidden || !game.meta(o).hidden) &&
				game.copy(state = hypoState).common.orderPlayable(game, o, excludeTrash = true)
			}
			.minByOption(game.xmeta(_).turnFinessed.getOrElse(99))

		getFinessedOrder(playerIndex, includeHidden = false) match {
			case None => false
			case Some(_) =>
				val order = getFinessedOrder(playerIndex, includeHidden = true).get
				game.common.thoughts(order).id(infer = true) match {
					case None => loop(hypoState, state.nextPlayerIndex(playerIndex))
					case Some(id) => loop(hypoState.withPlay(id), state.nextPlayerIndex(playerIndex))
				}
		}

	loop(game.state, state.nextPlayerIndex(action.giver))

def resolveClue(ctx: ClueContext, fps: List[FocusPossibility], ambiguousOwn: List[FocusPossibility] = Nil) =
	val ClueContext(prev, game, action) = ctx
	val state = game.state
	val ClueAction(giver, target, _, _) = action
	val FocusResult(focus, chop, _) = ctx.focusResult

	val symmetricFps =
		if (target == state.ourPlayerIndex || fps.exists(_.save))
			List()
		else
			Log.highlight(Console.YELLOW, "finding symmetric connections!")
			val symmetricFps = {
				val looksDirect = game.common.thoughts(focus).id(symmetric = true).isEmpty &&
					fps.exists { fp =>
						game.players(target).thoughts(focus).possible.contains(fp.id) &&
						fp.connections.forall { c =>
							c.isInstanceOf[KnownConn] ||
							(c.isInstanceOf[PlayableConn] && c.reacting != state.ourPlayerIndex)
						}
					}

				game.common.thoughts(focus).inferred.filter { inf =>
					visibleFind(state, game.common, inf, infer = true, excludeOrder = focus).isEmpty &&
					!fps.exists(_.id == inf)
				}
				.flatMap {
					connect(ctx, _, looksDirect, thinksStall = Set(), findOwn = Some(target))
				}
			}.toList

			occamsRazor(symmetricFps, target)

	val allFps = fps ++ symmetricFps
	val simplestFps = occamsRazor(allFps.filter(fp => game.players(target).thoughts(focus).possible.contains(fp.id)), target)
	val fpsToWrite = if (simplestFps.forall(_.symmetric)) simplestFps else allFps

	val interp = if (state.deck(focus).id().exists(id => !simplestFps.exists(_.id == id)))
		Log.error(s"resolving clue but focus ${state.logId(focus)} doesn't match simplest fps [${simplestFps.map(fp => state.logId(fp.id)).mkString(",")}]!")
		ClueInterp.Mistake
	else if (simplestFps.forall(_.symmetric))
		Log.error(s"resolving clue but all focus possibilities are symmetric!")
		ClueInterp.Mistake
	else if (fps.exists(_.save))
		ClueInterp.Save
	else
		ClueInterp.Play

	val undoScream =
		interp == ClueInterp.Save &&
		target == state.nextPlayerIndex(giver) &&
		game.dcStatus == DcStatus.Scream &&
		state.numPlayers > 2

	game.withThought(focus) { t =>
		val newInferred = t.inferred.intersect(IdentitySet.from(allFps.map(_.id)))
			// If a non-finesse connection exists, the focus can't be a copy of it
			.filter(i => !fps.filterNot(_.symmetric).flatMap(_.connections).exists {
				case c: KnownConn => c.id == i
				case c: PlayableConn => c.id == i
				case c: PromptConn => c.id == i
				case _: FinesseConn => false
			})
		Log.highlight(Console.CYAN, s"final infs [${newInferred.fmt(state)}] $focus")
		t.copy(inferred = newInferred, infoLock = Some(newInferred))
	}
	.pipe(assignConns(_, action, fpsToWrite, focus, ambiguousOwn))
	.pipe {
		allFps.foldLeft(_) { (a, fp) =>
			fp.connections.zipWithIndex.foldLeft(a) { case (acc, (conn, i)) => conn match {
				case PlayableConn(reacting, order, id, linked, layered) if linked.length > 1 =>
					val playLinks = acc.common.playLinks
					val target = fp.connections.lift(i + 1).map(_.order).getOrElse(focus)
					val existingIndex = playLinks.indexWhere { l =>
						l.orders == linked && l.target == target
					}

					if (existingIndex == -1)
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
		}}
	}
	.pipe { g =>
		def requiresWc(fp: FocusPossibility) = fp.connections.exists { c =>
			c.isInstanceOf[PlayableConn] ||
			c.isInstanceOf[PromptConn] ||
			c.isInstanceOf[FinesseConn]
		}

		g.copy(
			waiting = g.waiting ++ allFps.collect {
				case fp if requiresWc(fp) => WaitingConnection(
					fp.connections,
					giver,
					target,
					state.turnCount,
					focus,
					fp.id,
					symmetric = fp.symmetric
				)
			} ++ ambiguousOwn.map { fp =>
				WaitingConnection(
					fp.connections,
					giver,
					target,
					state.turnCount,
					focus,
					fp.id,
					ambiguousSelf = true
				)
			},
			lastMove = Some(interp),
			cluedOnChop = if (chop) g.cluedOnChop + focus else g.cluedOnChop
		)
	}
	.when(_ => undoScream) { g =>
		// Must be leftmost chop moved
		state.hands(giver).find(game.meta(_).cm).fold(g) { oldChop =>
			g.withMeta(oldChop)(_.copy(status = CardStatus.None))
		}
	}
	.when(g => importantFinesse(g.state, action, fps) || urgentSave(ctx)) { g =>
		Log.highlight(Console.YELLOW, s"important action for ${g.state.names(giver)}!")
		g.copy(importantAction = g.importantAction.updated(giver, true))
	}
	.withMeta(focus)(_.copy(focused = true))
