package scala_bot.basics

import scala_bot.utils._

enum FixResult:
	/** A "conventional" fix clue, indicating either trash or a duplicate (or both). */
	case Normal(cluedResets: Seq[Int], duplicateReveals: Seq[Int])
	/** A fix clue that gives no new information. */
	case NoNewInfo(fixes: Seq[Int])
	/** Not a fix clue. */
	case None

/** Returns a [[FixResult]] describing whether a fix clue was just given..
  *
  * @param prev   The game state before the clue.
  * @param game   The game state after the clue (and associated elim).
  * @param action The clue action.
  */
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
		!prev.meta(o).focused &&
		prevPlayables.contains(o) &&
		list.forall(prev.state.deck(_).clued) &&
		prev.common.thoughts(o).possible == game.common.thoughts(o).possible &&
		game.common.thoughts(o).possible.exists(game.state.isBasicTrash)

	if noNewInfoFixes.nonEmpty then
		FixResult.NoNewInfo(noNewInfoFixes)
	else
		FixResult.None

/** If id is provided, returns a non-empty list if it can be made playable by [[target]]'s turn.
  * Otherwise, returns the orders of playable cards in [[target]]'s hand by their turn.
  *
  * @param game   The initial game state.
  * @param player The observing player (determines which cards are playable).
  * @param start  The index of the first player allowed to play a card.
  * @param target The index of the destination player (they do not play a card).
  * @param id     Optionally, the identity desired to be made playable by [[target]]'s turn.
  */
def connectableSimple[G <: Game](game: G, player: Player, start: Int, target: Int, id: Option[Identity] = None)(using ops: GameOps[G]): Seq[Int] =
	val state = game.state

	if id.exists(state.isPlayable) then
		List(99)
	else if start == target then
		player.obviousPlayables(game, target)
	else if game.state.ended then
		List.empty
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

	if state.deck(focus).id().exists(state.isBasicTrash) then
		return None

	val poss = if clue.kind == ClueKind.Colour then
		thought.possible
	else
		thought.possible.filter(_.rank == clue.value)

	val useful = poss.iter.foldLeftOpt(IdentitySet.empty): (acc, id) =>
		lazy val duplicated = state.hands.zipWithIndex.exists: (hand, i) =>
			i != target && hand.exists(o => game.isTouched(o) && game.orderMatches(o, id, infer = true))

		if state.isBasicTrash(id) then
			Right(acc)
		else if duplicated then
			Right(acc.union(id))
		else
			Left(IdentitySet.empty)

	Option.when(useful.nonEmpty)(useful)

/** Returns whether the prompted card should be ignored as a *Free Choice Finesse*.
  *
  * @param game    The game state (after the clue).
  * @param action  The clue action.
  * @param id      The identity that this card would be prompted as.
  * @param prompt  The order of the prompted card.
  */
def rainbowMismatch(game: Game, action: ClueAction, id: Identity, prompt: Int): Boolean =
	val state = game.state
	val ClueAction(_, target, list, clue) = action

	val skip =
		clue.kind != ClueKind.Colour ||
		!state.variant.suits(id.suitIndex).suitType.rainbowish ||
		game.knownAs(prompt, RAINBOWISH) ||
		state.deck(prompt).clues.exists(_.isEq(clue))

	if skip then
		return false

	val allRainbow =
		list.forall: o =>
			if target == state.ourPlayerIndex then
				game.me.thoughts(o).possible.forall: c =>
					state.variant.suits(c.suitIndex).suitType.rainbowish
			else
				state.variant.suits(state.deck(o).suitIndex).suitType.rainbowish

	if !allRainbow then
		return false

	// A matching colour would have touched the same cards.
	state.deck(prompt).clues.exists: c =>
		state.clueTouched(state.hands(target), c).sorted == list.sorted
