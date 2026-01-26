package scala_bot.hgroup

import scala_bot.basics._

def validBluff(game: HGroup, action: ClueAction, blind: Identity, truth: Identity, reacting: Int, connected: Set[Int], symmetric: Boolean = false) =
	val state = game.state
	val ClueAction(giver, target, _, clue) = action
	val focus = connected.head

	lazy val disconnect = symmetric ||
		(clue.kind == ClueKind.Rank && clue.value != blind.rank + 1) ||
		blind.next.forall(!game.common.thoughts(focus).possible.contains(_))

	lazy val interferes = state.hands(reacting).exists: o =>
		game.players(reacting).thoughts(o).possible.contains(truth) &&
		(game.isBlindPlaying(o) || game.meta(o).status == CardStatus.GentlemansDiscard)

	game.level >= Level.Bluffs &&
	state.nextPlayerIndex(giver) == reacting &&
	connected.size == 1 &&
	disconnect &&
	!(clue.kind == ClueKind.Colour && reacting == target) &&	// not self-colour bluff
	!interferes

/** Returns whether the colour clue could be a save on the given identity. */
def colourSave(prev: HGroup, action: ClueAction, id: Identity, focus: Int): Boolean =
	val state = prev.state
	val ClueAction(giver, target, list, clue) = action
	val Identity(suitIndex, rank) = id

	val thought = prev.common.thoughts(focus)
	val suit = state.variant.suits(suitIndex)

	if !state.variant.cardTouched(id, clue) || !thought.possible.contains(id) || state.isBasicTrash(id) then
		return false

	if rank == 5 && suit.name != "Black" && !suit.suitType.brownish then
		return false

	if suit.name == "Black" && (rank == 2 || rank == 5) then
		// Newly touched or fill-in cards
		val fillIns = list.count: o =>
			!state.deck(o).clued ||
			prev.common.thoughts(o).possible.exists(!state.variant.idTouched(_, clue))

		// Trash that would be picked up by a rank clue
		val trash = state.hands(target).count: o =>
			val card = state.deck(o)
			!card.clued && card.id().exists(i => i.rank == rank && state.isBasicTrash(i))

		if fillIns < 2 && trash == 0 then
			return false

	if suit.suitType.brownish && prev.common.thinksLoaded(prev, giver) then
		return false

	if "Dark Rainbow|Dark Prism".r.matches(suit.name) then
		val completed = prev.common.hypoStacks(suitIndex) == state.maxRanks(suitIndex)
		val savedCrit = list.exists: o =>
			val card = state.deck(o)
			!card.clued && card.id().exists: i =>
				state.isCritical(i) && i.rank != 5 &&
				"Dark Rainbow|Dark Prism".r.matches(state.variant.suits(i.suitIndex).name)

		if !completed && !savedCrit then
			return false

	// If there is a dark colour, save with that, otherwise red.
	val muddySaveColour = if !state.includesVariant("Black|Dark Brown|Dark Pink".r) then 0 else
		state.variant.suits.length - 2

	// Note that critical 2,3,4 can be saved with anything.
	if suit.name.contains("Muddy") && clue.value != muddySaveColour && !(state.isCritical(id) && Set(2,3,4).contains(rank)) then
		return false

	if suit.name.contains("Cocoa") && clue.value != muddySaveColour then
		return false

	state.isCritical(id) || (suit.suitType.brownish && rank == 2)

def rankSave(prev: HGroup, action: ClueAction, id: Identity, focus: Int): Boolean =
	val state = prev.state
	val ClueAction(giver, target, list, clue) = action
	val Identity(suitIndex, rank) = id
	val thought = prev.common.thoughts(focus)

	if !thought.possible.contains(id) || state.isBasicTrash(id) then
		return false

	// Don't consider save on k3,k4 (or dark i3,i4) with rank
	// TODO: Florrat Save
	if "Black|Dark Pink".r.matches(state.variant.suits(suitIndex).name) && (rank == 3 || rank == 4) then
		return false

	val loaded34 = prev.common.thinksLoaded(prev, giver) &&
		(state.includesVariant(WHITISH) || state.includesVariant("Dark Rainbow|Dark Prism".r)) &&
		(rank == 3 || rank == 4)

	if loaded34 then
		return false

	state.isCritical(id) || rank == 2
