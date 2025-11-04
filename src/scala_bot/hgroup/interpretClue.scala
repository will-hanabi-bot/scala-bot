package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.basics.given_Conversion_IdentitySet_Iterable
import scala_bot.utils._
import scala_bot.logger.Log

import scala.util.chaining.scalaUtilChainingOps

def interpClue(prev: HGroup, game: HGroup, action: ClueAction): HGroup =
	val (common, state) = (game.common, game.state)
	val ClueAction(giver, target, list, clue) = action

	val focusResult @ FocusResult(focus, chop, positional) = game.determineFocus(prev, action)
	val (cluedResets, duplicateReveals) = checkFix(prev, game, action)

	if (cluedResets.nonEmpty || duplicateReveals.nonEmpty)
		Log.info(s"fix clue! not inferring anything else")

		lazy val oldOrdered1s = prev.order1s(list.filter(prev.unknown1), noFilter = true)
		val pinkFix1s = state.includesVariant(PINKISH) &&
			clue.kind == ClueKind.Rank && clue.value != 1 &&
			oldOrdered1s.nonEmpty

		return game.when (_ => pinkFix1s) { g =>
			val fixedOrder = oldOrdered1s.head

			if (chop && (clue.value == 2 || clue.value == 5))
				Log.info(s"pink fix!")
				g.withThought(fixedOrder)(t => t.copy(
					inferred = t.possible.retain(!state.isPlayable(_))
				))
			else
				Log.info(s"pink fix promise!")
				g.withThought(fixedOrder)(t => t.copy(
					inferred = t.inferred.retain(i => i.rank == clue.value && !state.isPlayable(i))
				))
		}
		.withMeta(focus) {
			// Focus doesn't matter for a fix clue
			_.copy(focused = prev.meta(focus).focused)
		}
		.copy(lastMove = Some(ClueInterp.Fix))

	val stall = stallingSituation(prev, game, action, focusResult)

	if (stall.isDefined)
		val (interp, thinksStall) = stall.get

		if (thinksStall.size > 0 && thinksStall.size < state.numPlayers)
			Log.warn(s"asymmetric move!")
			// return game.copy(lastMove = Some(ClueInterp.Mistake))

		else if (thinksStall.size == state.numPlayers)
			Log.info(s"stalling situation $interp")

			return game
				.when(g => interp == StallInterp.Stall5 && g.inEarlyGame) {
					_.copy(stalled5 = true)
				}
				// Pink promise on stalls
				.when(g => g.state.includesVariant(PINKISH) && clue.kind == ClueKind.Rank) {
					_.withThought(focus)(t => t.copy(inferred = t.inferred.retain(_.rank == clue.value)))
				}
				.copy(
					lastMove = Some(ClueInterp.Stall),
					stallInterp = Some(interp)
				)

	val distributionIds = distributionClue(prev, game, action, focus)

	if (distributionIds.isDefined)
		Log.info(s"distribution clue!")

		return game
			.withThought(focus) { t => t.copy(
				inferred = t.possible.intersect(distributionIds.get),
				infoLock = Some(t.possible.intersect(distributionIds.get)),
				reset = false
			)}
			.copy(lastMove = Some(ClueInterp.Distribution))

	if (game.level >= Level.BasicCM && !state.inEndgame)
		val tcm = interpretTcm(prev, game, action, focus)

		if (tcm.isDefined)
			// All newly cards are trash
			return list.foldLeft(game) { (acc, order) =>
				if (prev.state.deck(order).clued)
					acc
				else
					acc.withThought(order) { t =>
						val newInferred = t.possible.retain(state.isBasicTrash)
						t.copy(
							inferred = newInferred,
							infoLock = Some(newInferred)
						)
					}
					.withMeta(order)(_.copy(trash = true))
			}
			.pipe(performCM(_, tcm.get))
			.copy(lastMove = Some(ClueInterp.Discard))

		val cm5 = interpret5cm(prev, game, action, focus)

		if (cm5.isDefined)
			return performCM(game, cm5.get)
				.copy(lastMove = Some(ClueInterp.Discard))

	val pinkTrashFix = state.includesVariant(PINKISH) &&
		!positional && clue.kind == ClueKind.Rank &&
		list.forall(o => prev.state.deck(o).clued && game.knownAs(o, PINKISH)) &&
		state.variant.suits.zipWithIndex.forall { (suit, suitIndex) =>
			!PINKISH.matches(suit) ||
			common.isTrash(game, Identity(suitIndex, clue.value), focus)
		}

	if (pinkTrashFix)
		Log.info(s"pink trash fix!")
		return game
			.withThought(focus) { t =>
				val newInferred = t.possible.retain(common.isTrash(game, _, focus))
				t.copy(
					inferred = newInferred,
					infoLock = Some(newInferred)
				)
			}
			.withMeta(focus){ m => m.copy(
				trash = m.trash || state.variant.suits.zipWithIndex.forall { (suit, suitIndex) =>
					!PINKISH.matches(suit) ||
					game.state.isBasicTrash(Identity(suitIndex, clue.value))
				}
			)}
			.copy(lastMove = Some(ClueInterp.Fix))

	val savePoss = if (!chop) List() else (for
		inf <- common.thoughts(focus).inferred if
			visibleFind(state, common, inf, infer = true, cond = (_, order) => order != focus).isEmpty && {
			if (clue.kind == ClueKind.Colour)
				colourSave(prev, action, inf, focus)
			else
				rankSave(prev, action, inf, focus)
			}
	yield
		FocusPossibility(inf, List(), ClueInterp.Save)).toList

	if (savePoss.nonEmpty)
		Log.info(s"found saves: [${savePoss.map(fp => state.logId(fp.id)).mkString(",")}]")

	val thinksStall = stall.map(_._2).getOrElse(Set())
	val focusPoss = {
		val looksDirect = common.thoughts(focus).id(symmetric = true).isEmpty &&
			(action.clue.kind == ClueKind.Colour || savePoss.nonEmpty || positional)

		common.thoughts(focus).inferred.filter { inf =>
			visibleFind(state, common, inf, infer = true, cond = (_, order) => order != focus).isEmpty &&
			!savePoss.exists(_.id == inf)
		}
		.flatMap {
			connect(prev, game, action, _, focusResult, looksDirect, thinksStall)
		}
	}.toList

	val matched = savePoss ++ focusPoss.filterNot(_.illegal)

	// Card matches an inference
	if (matched.exists(fp => state.deck(focus).matches(fp.id)))
		val simplest = occamsRazor(matched, target)
		Log.info(s"simplest focus possibilities [${simplest.map(fp => state.logId(fp.id)).mkString(",")}]")
		return resolveClue(game, action, focusResult, simplest)

	Log.info(s"finding own!")

	val ownFps = {
		val looksDirect = common.thoughts(focus).id(symmetric = true).isEmpty && {
			clue.kind == ClueKind.Colour ||
			// Looks like an existing possibility
			focusPoss.exists(_.connections.forall { c =>
				c.isInstanceOf[KnownConn] || (c.isInstanceOf[PlayableConn] && c.reacting != state.ourPlayerIndex)
			})
		}

		common.thoughts(focus).inferred.filter { inf =>
			visibleFind(state, common, inf, infer = true, cond = (_, order) => order != focus).isEmpty &&
			!(savePoss.exists(_.id == inf) || focusPoss.exists(_.id == inf))
		}
		.flatMap {
			connect(prev, game, action, _, focusResult, looksDirect, thinksStall, findOwn = true)
		}
	}.toList

	val simplest = occamsRazor(matched ++ ownFps, state.ourPlayerIndex, game.me.thoughts(focus).id())
	resolveClue(game, action, focusResult, simplest)
