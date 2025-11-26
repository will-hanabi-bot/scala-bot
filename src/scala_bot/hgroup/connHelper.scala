package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.utils._
import scala_bot.logger.Log

import scala.util.chaining.scalaUtilChainingOps

def assignConns(game: HGroup, action: ClueAction, fps: List[FocusPossibility], focus: Int) =
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
					val isBluff = conn match {
						case f: FinesseConn => f.bluff
						case _ => false
					}

					val playableIds = for
						(stack, i) <- acc.common.hypoStacks.zipWithIndex
						id = Identity(i, stack + 1) if !acc.common.isTrash(acc, id, conn.order)
					yield
						id

					val currPlayableIds = for
						(stack, i) <- state.playStacks.zipWithIndex
						id = Identity(i, stack + 1) if !acc.common.isTrash(acc, id, conn.order)
					yield
						id

					val isUnknownPlayable = conn match {
						case c: PlayableConn => c.linked.length > 1
						case _ => false
					}

					val thought = acc.common.thoughts(conn.order)

					val newInferred = {
						if (conn.hidden)
							thought.inferred.intersect(playableIds)

						else if (isBluff)
							thought.inferred.intersect(currPlayableIds)

						else if (modified.contains(conn.order))
							thought.inferred.union(conn.ids)

						else if (!isUnknownPlayable)
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
								(conn.isInstanceOf[FinesseConn] && fps.length > 1) ||
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
							.withXMeta(conn.order) {
								_.copy(turnFinessed = Some(state.turnCount))
							}
						case c: PlayableConn if isUnknownPlayable =>
							val target = fp.connections.lift(connI + 1).map(_.order).getOrElse(focus)
							val existingLink = g.common.links.exists {
								case Link.Promised(orders, id, target) =>
									id == c.id && orders.toSet == c.linked.toSet
								case _ => false
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

def resolveClue(ctx: ClueContext, fps: List[FocusPossibility]) =
	val ClueContext(prev, game, action) = ctx
	val state = game.state
	val ClueAction(giver, target, _, _) = action
	val FocusResult(focus, chop, _) = ctx.focusResult

	val interp = if (state.deck(focus).id().exists(id => !fps.exists(_.id == id)))
		Log.error(s"resolving clue but focus ${state.logId(focus)} doesn't match [${fps.map(fp => state.logId(fp.id)).mkString(",")}]!")
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

	val symmetricFps =
		if (target == state.ourPlayerIndex || interp == ClueInterp.Save)
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
	.pipe(assignConns(_, action, fps, focus))
	.pipe {
		allFps.foldLeft(_) { (a, fp) =>
			fp.connections.zipWithIndex.foldLeft(a) { case (acc, (conn, i)) => conn match {
				case PlayableConn(reacting, order, id, linked, layered) =>
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
			c.isInstanceOf[PromptConn] || c.isInstanceOf[FinesseConn]
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
	.withMeta(focus)(_.copy(focused = true))
