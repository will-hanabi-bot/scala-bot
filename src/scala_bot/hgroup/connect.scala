package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.basics.given_Conversion_IdentitySet_Iterable
import scala_bot.logger.Log
import scala_bot.utils.inBetween

def findKnownConn(game: HGroup, giver: Int, id: Identity, ignore: Set[Int] = Set()) =
	val state = game.state

	def validKnown(order: Int) =
		!ignore.contains(order) &&
		game.state.deck(order).matches(id, assume = true) &&
		game.common.thoughts(order).matches(id, infer = true) &&
		!game.xmeta(order).maybeFinessed &&
		!game.common.linkedOrders(state).contains(order)

	def validLink(link: Link, order: Int) =
		link match {
			case Link.Promised(orders, i, _) => i == id && orders.contains(order)
			case Link.Sarcastic(orders, i) => i == id && orders.contains(order)
			case _ => false
		}

	def validPlayable(order: Int) =
		!ignore.contains(order) &&
		game.state.deck(order).matches(id, assume = true) &&
		game.common.thoughts(order).inferred.contains(id) &&
		!game.xmeta(order).maybeFinessed &&
		game.common.orderPlayable(game, order)

	// Globally known
	val knownConns = for
		playerIndex <- (0 until state.numPlayers).view
		order <- state.hands(playerIndex) if validKnown(order)
	yield
		KnownConn(playerIndex, order, id)

	// Promised
	val linkedConns = for
		playerIndex <- (0 until state.numPlayers).view
		order <- state.hands(playerIndex)
		link <- game.common.links if validLink(link, order)
	yield
		PlayableConn(playerIndex, order, id, linked = link.getOrders)

	println(s"finding known ${state.logId(id)} $ignore ${game.common.linkedOrders(state)}")

	// Visible and going to be played (excludes giver)
	val playableConns = for
		playerIndex <- (0 until state.numPlayers).view if playerIndex != giver
		order <- state.hands(playerIndex) if validPlayable(order)
	yield
		PlayableConn(playerIndex, order, id)

	knownConns.headOption
	.orElse(linkedConns.headOption)
	.orElse(playableConns.headOption)

def findUnknownConnecting(prev: HGroup, game: HGroup, action: ClueAction, reacting: Int, id: Identity, connected: List[Int], ignore: Set[Int], noLayer: Boolean, assumeTruth: Boolean, bluff: Boolean, findOwn: Boolean = false): Option[Connection] =
	val (state, level) = (game.state, game.level)
	val ClueAction(giver, target, _, _) = action
	val FocusResult(focus, _, _) = game.determineFocus(prev, action)

	println(s"finding unknown connecting for ${state.logId(id)} (${state.names(reacting)}), $connected, own? $findOwn")

	if (bluff)
		val clued = prev.common.findClued(prev, reacting, id, ignore ++ connected)
		val matched = clued.find(state.deck(_).matches(id, assume = findOwn))

		if (matched.isDefined)
			return Some(PlayableConn(matched.get, reacting, id, linked = clued.toList))

	def tryPrompt(order: Int) =
		Option.when(!rainbowMismatch(prev, action, id, order, focus)) {
			state.deck(order).id() match {
				case None =>
					Option.when(findOwn && game.me.thoughts(order).possible.contains(id))
						(PromptConn(reacting, order, id))

				case Some(promptId) =>
					if (promptId == id)
						Some(PromptConn(reacting, order, id))

					else if (level >= Level.IntermediateFinesses && state.isPlayable(promptId))
						Some(PromptConn(reacting, order, promptId, hidden = true))

					else
						// Log.warn(s"wrong prompt on ${state.logId(order)} $order, stacks ${state.playStacks}")
						None
			}
		}.flatten

	val prompt = prev.common.findPrompt(prev, reacting, id, connected, ignore)
	val finesse = prev.findFinesse(reacting, connected, ignore)
	val promptConn = prompt.flatMap(tryPrompt)

	if (promptConn.isDefined)
		return promptConn

	// Try prompting a wrongly-ranked pink card
	val tryPinkPrompt = state.includesVariant(PINKISH) &&
		(level < Level.Bluffs || finesse.forall(game.common.thoughts(_).possible.forall(!state.isPlayable(_))))

	if (tryPinkPrompt)
		val pinkPrompt = prev.common.findPrompt(prev, reacting, id, connected, ignore, forcePink = true)
		val pinkPromptConn = pinkPrompt.filter(!prompt.contains(_)).flatMap(tryPrompt)

		if (pinkPromptConn.isDefined)
			return pinkPromptConn

	val cluedDupe = state.hands.zipWithIndex.exists { (hand, i) =>
		i != giver && hand.exists(o => prev.state.deck(o).clued && state.deck(o).matches(id))
	}

	if (cluedDupe)
		Log.warn(s"disallowed finesse, ${state.logId(id)} already clued")
		return None

	finesse.flatMap(state.deck(_).id()) match {
		case None if findOwn && finesse.isDefined =>
			val thought = game.me.thoughts(finesse.get)
			val bluffableIds = thought.inferred.retain { i =>
				validBluff(prev, action, i, id, reacting, connected)
			}
			val possiblyBluff = !assumeTruth &&
				bluffableIds.nonEmpty &&
				thought.possible.contains(id)

			val trueFinesse = thought.inferred.contains(id) &&
				thought.matches(id, assume = true)

			Option.when(trueFinesse || bluffableIds.nonEmpty) {
				val certain = state.hands(giver).view
					.map(state.deck)
					.exists(c => c.matches(id) && c.clued)

				FinesseConn(
					reacting,
					finesse.get,
					ids = if (trueFinesse) List(id) else bluffableIds.toList,
					self = true,
					bluff = !assumeTruth && !thought.possible.contains(id),
					possiblyBluff = possiblyBluff,
					certain = certain)
			}

		case None => None

		case Some(finesseId) =>
			val possiblyBluff = !assumeTruth &&
				validBluff(prev, action, finesseId, id, reacting, connected, symmetric = true)

			if (finesseId == id)
				// At level 1, only forward finesses are allowed.
				if (level == 1 && !inBetween(state.numPlayers, reacting, giver, target))
					Log.warn(s"found non-forward finesse ${state.logId(id)} in ${state.names(reacting)}'s hand at lv 1!")
					None
				else
					Some(FinesseConn(reacting, finesse.get, List(id), bluff = false, possiblyBluff = possiblyBluff))

			else if (!noLayer && level >= Level.IntermediateFinesses && state.isPlayable(finesseId))
				val bluff = validBluff(prev, action, finesseId, id, reacting, connected)

				// TODO: Check in resolver that we don't give uncertain finesses or likely dupes
				val uncertainFinesse = !bluff &&
					state.hands(giver).exists(o => state.deck(o).clued && state.deck(o).matches(finesseId))

				Option.when(!uncertainFinesse)
					(FinesseConn(reacting, finesse.get, List(finesseId), hidden = !bluff, bluff = bluff, possiblyBluff = possiblyBluff))

			else
				None
	}

def findConnecting(prev: HGroup, game: HGroup, action: ClueAction, id: Identity, looksDirect: Boolean, thinksStall: Set[Int], connected: List[Int], ignore: Set[Int] = Set(), knownOnly: Set[Int] = Set(), assumeTruth: Boolean = false, bluff: Boolean = false, findOwn: Boolean = false): Option[List[Connection]] =
	val state = game.state
	val ClueAction(giver, target, _, _) = action

	// Log.info(s"find connecting ${state.logId(id)}")

	if (state.baseCount(id.toOrd) == state.cardCount(id.toOrd))
		Log.info(s"all ${state.logId(id)} in trash!")
		return None

	val known = findKnownConn(prev, giver, id, ignore ++ connected)

	if (known.isDefined)
		return Some(known.toList)

	val connPlayerOrder = List(
		List(target),	// first, try without layered
		(0 until state.numPlayers)
			.map(i => (giver - i - 1 + state.numPlayers) % state.numPlayers)
			.filterNot(_ == target),
		List(target),	// allow self-layered if no one will fix a self-clandestine
	).flatten

	@annotation.tailrec
	def loop(hypo: HGroup, i: Int = 0, connections: List[Connection] = Nil): Option[List[Connection]] =
		if (i == connPlayerOrder.length)
			return None

		val playerIndex = connPlayerOrder(i)
		val skip = playerIndex == giver ||
			knownOnly.contains(playerIndex) ||
			(playerIndex == target && looksDirect) ||
			thinksStall.contains(playerIndex)

		// TODO: When resolving, disallow prompting/finessing a player that may need to prove sth to us
		if (skip)
			// println(s"skipping $giver $target $looksDirect ${state.names(playerIndex)} $knownOnly $thinksStall")
			loop(hypo, i + 1)
		else
			val noLayer = i == 0
			findUnknownConnecting(prev, game, action, playerIndex, id, connected, ignore, noLayer, assumeTruth, bluff, findOwn) match {
				case None =>
					loop(hypo, i + 1)

				case Some(conn) if conn.hidden =>
					val newGame = state.deck(conn.order).id().fold(hypo)(id => hypo.withState(_.withPlay(id)))
					// Try again with same player
					loop(newGame, i, conn +: connections)

				case Some(_) if playerIndex == target && looksDirect =>
					Log.warn(s"looks direct!")
					None

				case Some(conn) =>
					Some((conn +: connections).reverse)
			}

	loop(game)

def validBluff(game: HGroup, action: ClueAction, blind: Identity, truth: Identity, reacting: Int, connected: List[Int], symmetric: Boolean = false) =
	val state = game.state
	val ClueAction(giver, target, _, clue) = action
	val focus = connected.head

	lazy val disconnect = symmetric ||
		(blind.rank == 5) ||
		(clue.kind == ClueKind.Rank && clue.value != blind.rank + 1) ||
		!game.common.thoughts(focus).possible.contains(blind.next)

	lazy val selfColourBluff =
		clue.kind == ClueKind.Colour && reacting == target

	lazy val interferes = state.hands(reacting).exists { o =>
		game.players(reacting).thoughts(o).possible.contains(truth) &&
		(game.isBlindPlaying(o) || game.meta(o).status == CardStatus.GentlemansDiscard)
	}

	game.level >= Level.Bluffs &&
	state.nextPlayerIndex(giver) == reacting &&
	connected.length == 1 &&
	disconnect &&
	!selfColourBluff &&
	!interferes

/** Returns whether the colour clue could be a save on the given identity. */
def colourSave(prev: HGroup, action: ClueAction, id: Identity, focus: Int): Boolean =
	val state = prev.state
	val ClueAction(giver, target, list, clue) = action
	val Identity(suitIndex, rank) = id

	val thought = prev.common.thoughts(focus)
	val suit = state.variant.suits(suitIndex)

	if (!state.variant.cardTouched(id, clue) || !thought.possible.contains(id) || state.isBasicTrash(id))
		return false

	if (rank == 5 && suit != "Black" && !BROWNISH.matches(suit))
		return false

	if (suit == "Black" && (rank == 2 || rank == 5))
		// Newly touched or fill-in cards
		val fillIns = list.count { o =>
			!state.deck(o).clued ||
			prev.common.thoughts(o).possible.exists(!state.variant.idTouched(_, clue))
		}

		// Trash that would be picked up by a rank clue
		val trash = state.hands(target).count { o =>
			val card = state.deck(o)
			!card.clued && card.id().exists(i => i.rank == rank && state.isBasicTrash(i))
		}

		if (fillIns < 2 && trash == 0)
			return false

	if (BROWNISH.matches(suit) && prev.common.thinksLoaded(prev, giver))
		return false

	if ("Dark Rainbow|Dark Prism".r.matches(suit))
		val completed = prev.common.hypoStacks(suitIndex) == state.maxRanks(suitIndex)
		val savedCrit = list.exists { o =>
			val card = state.deck(o)
			!card.clued && card.id().exists { i =>
				state.isCritical(i) && i.rank != 5 &&
				"Dark Rainbow|Dark Prism".r.matches(state.variant.suits(i.suitIndex))
			}
		}

		if (!completed && !savedCrit)
			return false

	// If there is a dark colour, save with that, otherwise red.
	val muddySaveColour = if (!state.includesVariant("Black|Dark Brown|Dark Pink".r)) 0 else
		state.variant.suits.length - 2

	// Note that critical 2,3,4 can be saved with anything.
	if (suit.contains("Muddy") && clue.value != muddySaveColour && !(state.isCritical(id) && Set(2,3,4).contains(rank)))
		return false

	if (suit.contains("Cocoa") && clue.value != muddySaveColour)
		return false

	state.isCritical(id) || (BROWNISH.matches(suit) && rank == 2)

def rankSave(prev: HGroup, action: ClueAction, id: Identity, focus: Int): Boolean =
	val state = prev.state
	val ClueAction(giver, target, list, clue) = action
	val Identity(suitIndex, rank) = id
	val thought = prev.common.thoughts(focus)

	if (!thought.possible.contains(id) || state.isBasicTrash(id))
		return false

	// Don't consider save on k3,k4 (or dark i3,i4) with rank
	// TODO: Florrat Save
	if ("Black|Dark Pink".r.matches(state.variant.suits(suitIndex)) && (rank == 3 || rank == 4))
		return false

	val loaded34 = prev.common.thinksLoaded(prev, giver) &&
		(state.includesVariant(WHITISH) || state.includesVariant("Dark Rainbow|Dark Prism".r)) &&
		(rank == 3 || rank == 4)

	if (loaded34)
		return false

	state.isCritical(id) || rank == 2

def connect(prev: HGroup, game: HGroup, action: ClueAction, id: Identity, focusResult: FocusResult, looksDirect: Boolean, thinksStall: Set[Int], assumeTruth: Boolean = false, findOwn: Boolean = false): Option[FocusPossibility] =
	val state = game.state
	val FocusResult(focus, _, positional) = focusResult
	val Identity(suitIndex, rank) = id
	var attemptedBluff = false

	// Log.info(s"trying to connect ${state.logId(id)}")

	@annotation.tailrec
	def loop(hypo: HGroup, nextRank: Int, connections: List[Connection]): Option[List[Connection]] =
		if (nextRank == rank)
			Some(connections)
		else
			val nextId = Identity(suitIndex, nextRank)
			val connected = focus +: connections.map(_.order)
			val direct = looksDirect && !connections.exists(_.isInstanceOf[FinesseConn])
			val bluffed = connections.exists {
				case f: FinesseConn => f.bluff
				case _ => false
			}
			attemptedBluff ||= bluffed

			val conn = findConnecting(prev, game, action, nextId, direct, thinksStall, connected, bluff = bluffed)
				.orElse(Option.when(findOwn) {
					findConnecting(prev, game, action, nextId, direct, thinksStall, connected, bluff = bluffed, findOwn = true)
				}.flatten)

			conn match {
				case None =>
					// Log.info(s"failed connection to ${state.logId(id)}: ${state.logConns(connections, nextId)}")
					None
				case Some(conns) =>
					// println(s"found conns ${state.logConns(conns)}")
					val newGame = conns.foldLeft(hypo) { (acc, conn) =>
						acc.state.deck(conn.order).id()
							.orElse(Option.when(conn.ids.length == 1)(conn.ids.head))
							.fold(acc)(i => acc.withState(_.withPlay(i)))
					}
					loop(newGame, nextRank + 1, connections ++ conns)
			}

	loop(game, state.playStacks(suitIndex) + 1, List()).map { conns =>
		val selfClandestine = !findOwn && conns.zipWithIndex.exists { (conn, i) =>
			conn.reacting == action.target && conn.isInstanceOf[FinesseConn] && conn.hidden &&
			game.common.thoughts(focus).possible.contains(conn.ids.head.next) &&
			// Someone else finessing will prove this is a clandestine self.
			!conns.take(i).exists(conn2 => conn2.isInstanceOf[FinesseConn] && conn2.reacting != action.target)
		}

		if (selfClandestine)
			Log.warn("illegal clandestine self-finesse!")

		Log.info(s"found connections: ${state.logConns(conns, id)}")
		FocusPossibility(id, connections = conns, interp = ClueInterp.Play, illegal = selfClandestine)
	}
	.orElse {
		Option.when (game.level > Level.Bluffs && !assumeTruth && attemptedBluff) {
			Log.highlight(Console.YELLOW, "bluff connection failed, retrying true finesse")

			connect(prev, game, action, id, focusResult, looksDirect, thinksStall, assumeTruth = true, findOwn)
		}.flatten
	}
