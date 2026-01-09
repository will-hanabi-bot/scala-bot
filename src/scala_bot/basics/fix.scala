package scala_bot.basics

import scala_bot.utils._

enum FixResult:
	case Normal(cluedResets: List[Int], duplicateReveals: List[Int])
	case NoNewInfo(fixes: List[Int])
	case None

def checkFix(prev: Game, game: Game, action: ClueAction): FixResult =
	val list = action.list
	val prevPlayables = prev.common.thinksPlayables(prev, action.target)

	val (cluedResets, duplicateReveals) = list.foldRight((List[Int](), List[Int]())) { case (order, (cluedResets, duplicateReveals)) =>
		lazy val duplicated = prev.state.deck(order).clued && list.exists: o =>
			o != order &&
			prev.state.deck(o).clued &&
			game.state.deck(order).matches(game.state.deck(o)) &&
			!prev.common.thoughts(order).matches(prev.common.thoughts(o), infer = true)

		if prev.meta(order).status == CardStatus.CalledToPlay && prev.isBlindPlaying(order) && game.common.thoughts(order).infoLock.existsO(_.forall(game.state.isBasicTrash)) then
			(order +: cluedResets, duplicateReveals)
		else if prev.state.deck(order).clued && !prev.common.thoughts(order).reset && !prev.common.orderKt(prev, order) && game.common.thoughts(order).reset then
			(order +: cluedResets, duplicateReveals)
		else if duplicated then
			(cluedResets, order +: duplicateReveals)
		else
			(cluedResets, duplicateReveals)
	}

	if cluedResets.nonEmpty || duplicateReveals.nonEmpty then
		return FixResult.Normal(cluedResets, duplicateReveals)

	val noNewInfoFixes = list.filter: o =>
		prevPlayables.contains(o) &&
		list.forall(prev.state.deck(_).clued) &&
		prev.common.thoughts(o).possible == game.common.thoughts(o).possible

	if noNewInfoFixes.nonEmpty then
		FixResult.NoNewInfo(noNewInfoFixes.toList)
	else
		FixResult.None

def connectableSimple[G <: Game](game: G, player: Player, start: Int, target: Int, id: Option[Identity] = None)(using ops: GameOps[G]): List[Int] =
	val state = game.state

	if id.exists(state.isPlayable) then
		List(99)
	else if start == target then
		player.obviousPlayables(game, target).toList
	else if game.state.ended then
		List()
	else
		val nextPlayerIndex = state.nextPlayerIndex(start)
		val playables = player.obviousPlayables(game, start)

		val connectables = playables.view.map { order =>
			player.thoughts(order).id(infer = true).map: playId =>
				val newGame = game
					.simulateAction(TurnAction(state.turnCount, start))	// Go to starting player's turn
					.simulateAction(PlayAction(start, order, playId.suitIndex, playId.rank))
					.simulateAction(TurnAction(state.turnCount + 1, nextPlayerIndex))

				connectableSimple(newGame, player, nextPlayerIndex, target, id)
		}.flatten.find(_.nonEmpty)

		connectables.getOrElse(connectableSimple(game, player, nextPlayerIndex, target, id))

/** Returns the possible ids of a distribution clue. */
def distributionClue(prev: Game, game: Game, action: ClueAction, focus: Int): Option[IdentitySet] =
	val state = game.state
	val ClueAction(_, target, list, clue) = action
	val thought = game.common.thoughts(focus)

	if list.forall(prev.state.deck(_).clued) || (!state.inEndgame && state.remScore > state.variant.suits.length) then
		return None

	if thought.id(infer = true).exists(state.isBasicTrash) then
		return None

	val poss = if clue.kind == ClueKind.Colour then
		thought.possible.toList
	else
		thought.possible.filter(_.rank == clue.value).toList

	val useful = poss.foldLeftOpt(IdentitySet.empty): (acc, id) =>
		lazy val duplicated = state.hands.zipWithIndex.exists: (hand, i) =>
			i != target && hand.exists(o => game.isTouched(o) && game.orderMatches(o, id, infer = true))

		if state.isBasicTrash(id) then
			Right(acc)
		else if duplicated then
			Right(acc.union(id))
		else
			Left(IdentitySet.empty)

	Option.when(useful.nonEmpty)(useful)

def rainbowMismatch(game: Game, action: ClueAction, id: Identity, prompt: Int, focus: Int) =
	val state = game.state
	val ClueAction(_, target, list, clue) = action

	lazy val rainbowFocus =
		if target == state.ourPlayerIndex then
			game.me.thoughts(focus).possible.forall: c =>
				state.variant.suits(c.suitIndex).suitType.rainbowish
		else
			state.variant.suits(state.deck(focus).suitIndex).suitType.rainbowish

	lazy val matchingClues = state.allColourClues(target).filter: c =>
		state.clueTouched(state.hands(target), c).sorted == list.sorted &&	// touches the same cards
		state.deck(prompt).clues.contains(c)								// prompt was clued with this

	clue.kind == ClueKind.Colour &&
	state.variant.suits(id.suitIndex).suitType.rainbowish &&
	!game.knownAs(prompt, RAINBOWISH) &&
	rainbowFocus &&
	// There was free choice to clue a matching colour, but didn't
	matchingClues.nonEmpty && !matchingClues.exists(_.isEq(clue))
