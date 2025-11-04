package scala_bot.test

import scala_bot.basics._
import scala_bot.utils.visibleFind
import scala.util.chaining.scalaUtilChainingOps

enum Colour:
	case Red, Yellow, Green, Blue, Purple

enum Player:
	case Alice, Bob, Cathy, Donald, Emily

val VARIANTS = Map(
	"No Variant" 		-> Variant(0, "No Variant", Vector("Red", "Yellow", "Green", "Blue", "Purple"), Vector('r', 'y', 'g', 'b', 'p')),
	"6 Suits" 			-> Variant(0, "6 Suits", Vector("Red", "Yellow", "Green", "Blue", "Purple", "Teal"), Vector('r', 'y', 'g', 'b', 'p', 't')),
	"Rainbow (5 Suits)" -> Variant(16, "Rainbow", Vector("Red", "Yellow", "Green", "Blue", "Rainbow"), Vector('r', 'y', 'g', 'b', 'm')),
	"Black (5 Suits)" 	-> Variant(2, "Black", Vector("Red", "Yellow", "Green", "Blue", "Black"), Vector('r', 'y', 'g', 'b', 'k')),
	"Pink (5 Suits)" 	-> Variant(2, "Pink", Vector("Red", "Yellow", "Green", "Blue", "Pink"), Vector('r', 'y', 'g', 'b', 'i')),
	"Brown (5 Suits)" 	-> Variant(2, "Brown", Vector("Red", "Yellow", "Green", "Blue", "Brown"), Vector('r', 'y', 'g', 'b', 'n'))
)

val NAMES = Vector("Alice", "Bob", "Cathy", "Donald", "Emily")

def setup[G <: Game](
	constructor: (Int, State, Boolean) => G,
	hands: Vector[Vector[String]],
	playStacks: Option[Vector[Int]] = None,
	discarded: Vector[String] = Vector(),
	strikes: Int = 0,
	clueTokens: Int = 8,
	starting: Player = Player.Alice,
	variant: String = "No Variant",
	init: G => G = (x: G) => x
)(using ops: GameOps[G]) =
	val playerNames = NAMES.slice(0, hands.length)
	val _state = State(playerNames, 0, VARIANTS(variant))

	ops.copyWith(constructor(0, _state, false), GameUpdates(catchup = Some(true)))
		.pipe { g =>
			playStacks.fold(g) { stacks =>
				if (stacks.length != g.state.variant.suits.length)
					throw new IllegalArgumentException("Invalid play stacks length")
				ops.copyWith(g, GameUpdates(
					state = Some(g.state.copy(
						playStacks = stacks,
						baseCount = stacks.zipWithIndex
							.flatMap((stack, suitIndex) => (1 to stack).map(Identity(suitIndex, _)))
							.foldLeft(g.state.baseCount)((acc, id) => acc.updated(id.toOrd, acc(id.toOrd) + 1))
					)),
					players = Some(g.players.map(_.copy(hypoStacks = stacks))),
					common = Some(g.common.copy(hypoStacks = stacks))
				))
			}
		}
		.pipe { g =>
			// Draw all the hands
			val drawActions =
				var orderCounter = -1
				for
					(hand, playerIndex) <- hands.zipWithIndex
					short <- hand.reverse
				yield
					orderCounter += 1
					if (short == "xx")
						DrawAction(playerIndex, orderCounter, -1, -1)
					else
						val Identity(suitIndex, rank) = g.state.expandShort(short)
						DrawAction(playerIndex, orderCounter, suitIndex, rank)

			drawActions.foldLeft(g)(_.handleAction(_))
		}
		.pipe { g =>
			discarded.foldLeft(g) { case (game, short) =>
				val id = game.state.expandShort(short)
				game.withState(_.withDiscard(id, 99))
			}
		}
		.tap { g =>
			for (id <- g.state.variant.allIds) {
				val count =	g.state.baseCount(id.toOrd) + visibleFind(g.state, g.me, id).length

				if (count > g.state.cardCount(id.toOrd))
					throw new IllegalArgumentException(s"Found $count copies of ${g.state.logId(id)}!")
			}
		}
		.pipe { g =>
			g.withState(s => s.copy(
				cardsLeft = s.cardsLeft - s.score - discarded.length,
				currentPlayerIndex = starting.ordinal,
				clueTokens = clueTokens,
				strikes = strikes
			))
		}
		.pipe(init)
		.pipe(_.elim(goodTouch = true))
		.pipe { g =>
			ops.copyWith(g, GameUpdates(
				base = Some(g.state, g.meta, g.players, g.common)
			))
		}

def takeTurn[G <: Game](rawAction: String, drawS: String = "")(game: G)(using ops: GameOps[G]) =
	val state = game.state
	val action = parseAction(state, rawAction)

	val draw = drawS match {
		case "" => None
		case s => Some(state.expandShort(s))
	}

	if (action.playerIndex != state.currentPlayerIndex)
		throw new IllegalArgumentException(s"Expected '${state.names(state.currentPlayerIndex)}'s turn for action (${action.fmt(state)})!")

	game.withCatchup {
		_.handleAction(action)
		.pipe { g =>
			action match {
				case PlayAction(_, _, _, _) | DiscardAction(_, _, _, _, _) =>
					draw match {
						case Some(_) if state.cardsLeft == 0 =>
							throw new IllegalArgumentException(s"Cannot draw at 0 cards left")

						case None if action.playerIndex != state.ourPlayerIndex =>
							throw new IllegalArgumentException(s"Missing draw for ${state.names(action.playerIndex)}'s action $action")

						case Some(id) =>
							val Identity(suitIndex, rank) = id
							val count = state.baseCount(id.toOrd) + visibleFind(state, game.me, id).length

							if (count + 1 > state.cardCount(id.toOrd))
								throw new IllegalArgumentException(s"Found ${count + 1} copies of ${state.logId(id)}")
							g.handleAction(DrawAction(state.currentPlayerIndex, state.nextCardOrder, suitIndex, rank))

						case None =>
							g.handleAction(DrawAction(state.currentPlayerIndex, state.nextCardOrder, -1, -1))
					}
				case _ if draw.isDefined =>
					throw new IllegalArgumentException(s"Unexpected draw for action $action")
				case _ => g
			}
		}
		.handleAction(TurnAction(state.turnCount, state.nextPlayerIndex(action.playerIndex)))
	}

def parseAction(state: State, action: String) =
	val cluePattern = """(\w+) clues (\d|\w+) to (\w+)(?: \(slots? (\d(?:,\d)*)\))?""".r
	val playPattern = """(\w+) plays (\w\d)(?: \(slot (\d)\))?""".r
	val discardPattern = """(\w+) (discards|bombs) (\w\d)(?: \(slot (\d)\))?""".r

	def parsePlayer(str: String) =
		val index = state.names.indexOf(str)
		if (index == -1)
			throw new IllegalArgumentException(s"Player $str not found in [${state.names.mkString(",")}]")
		index

	action match {
		case cluePattern(giverS, valueS, targetS, slotsS) =>
			if (!state.canClue)
				throw new IllegalArgumentException(s"Tried to clue with 0 clue tokens")

			val giver = parsePlayer(giverS)
			val target = parsePlayer(targetS)

			val clue =
				if ("12345".contains(valueS))
					BaseClue(ClueKind.Rank, valueS.toInt)
				else
					val colour = state.variant.suits.indexWhere(_.toLowerCase() == valueS.toLowerCase())
					if (colour == -1)
						throw new IllegalArgumentException(s"Colour $valueS not found in [${state.variant.suits.mkString(",")}]")

					BaseClue(ClueKind.Colour, colour)

			if (target != state.ourPlayerIndex)
				val list = state.clueTouched(state.hands(target), clue)

				if (list.isEmpty)
					throw new IllegalArgumentException(s"No cards touched by clue $valueS to $targetS")

				ClueAction(giver, target, list, clue)
			else if (slotsS.isEmpty)
				throw new IllegalArgumentException(s"Not enough arguments provided (clue to us) in $action, needs '(slot x)'")
			else
				val list = slotsS.split(",").map(s => state.ourHand(s.toInt - 1))
				ClueAction(giver, target, list.toSeq, clue)

		case playPattern(playerS, short, slotS) =>
			val playerIndex = parsePlayer(playerS)
			val id = state.expandShort(short)
			val Identity(suitIndex, rank) = id

			if (playerIndex != state.ourPlayerIndex)
				val matching = state.hands(playerIndex).filter(state.deck(_).matches(id))

				matching match {
					case Vector() =>
						throw new IllegalArgumentException(s"Unable to find $short to play in ${state.names(playerIndex)}'s hand")
					case Vector(order) =>
						// If slot provided, it must be correct
						if (slotS != null)
							val slot = slotS.toInt
							if (state.hands(playerIndex)(slot - 1) != order)
								throw new IllegalArgumentException(s"Identity $short not in slot $slotS")

						PlayAction(playerIndex, order, suitIndex, rank)
					case _ =>
						if (slotS == null)
							throw new IllegalArgumentException(s"Not enough arguments provided (ambiguous identity) in $action, needs '(slot x)'")
						else
							val order = state.hands(playerIndex)(slotS.toInt - 1)
							if (!state.deck(order).matches(id))
								throw new IllegalArgumentException(s"Identity $short not in slot $slotS")
							PlayAction(playerIndex, order, suitIndex, rank)
				}
			else if (slotS == null)
				throw new IllegalArgumentException(s"Not enough arguments provided (play from us) in $action, needs '(slot x)'")
			else
				val order = state.hands(state.ourPlayerIndex)(slotS.toInt - 1)
				PlayAction(playerIndex, order, suitIndex, rank)

		case discardPattern(playerS, action, short, slotS) =>
			if (state.clueTokens == 8)
				throw new IllegalArgumentException(s"Tried to discard with 8 clue tokens")

			val playerIndex = parsePlayer(playerS)
			val id = state.expandShort(short)
			val Identity(suitIndex, rank) = id
			val failed = action == "bombs"

			if (playerIndex != state.ourPlayerIndex)
				val matching = state.hands(playerIndex).filter(state.deck(_).matches(id))

				matching match {
					case Vector() =>
						throw new IllegalArgumentException(s"Unable to find $short to discard in $playerIndex's hand")
					case Vector(order) =>
					// If slot provided, it must be correct
						if (slotS != null)
							val slot = slotS.toInt
							if (state.hands(playerIndex)(slot - 1) != order)
								throw new IllegalArgumentException(s"Identity $short not in slot $slotS")

						DiscardAction(playerIndex, order, suitIndex, rank, failed)
					case _ =>
						if (slotS == null)
							throw new IllegalArgumentException(s"Not enough arguments provided (ambiguous identity) in $action, needs '(slot x)'")
						else
							val order = state.hands(playerIndex)(slotS.toInt - 1)
							if (!state.deck(order).matches(id))
								throw new IllegalArgumentException(s"Identity $short not in slot $slotS")
							DiscardAction(playerIndex, order, suitIndex, rank, failed)
				}
			else if (slotS == null)
				throw new IllegalArgumentException(s"Not enough arguments provided (discard from us) in $action, needs '(slot x)'")
			else
				val order = state.hands(state.ourPlayerIndex)(slotS.toInt - 1)
				DiscardAction(playerIndex, order, suitIndex, rank, failed)

		case _ =>
			throw new IllegalArgumentException(s"Invalid action: $action")
	}

case class TestClue(
	kind: ClueKind,
	value: Int,
	giver: Player
):
	def toBase =
		BaseClue(kind, value)

def preClue[G <: Game](playerIndex: Player, slot: Int, clues: Seq[TestClue])(game: G)(using ops: GameOps[G]) =
	val state = game.state
	val order = state.hands(playerIndex.ordinal)(slot - 1)

	state.deck(order).id().foreach { id =>
		clues.find(c => !state.variant.idTouched(id, c.toBase)).foreach { nonTouchingClue =>
			throw new IllegalArgumentException(
				s"Clue ${nonTouchingClue.toBase.fmt(state, playerIndex.ordinal)} doesn't touch order $order!")
		}
	}

	val possibilities = IdentitySet.from(state.variant.allIds.filter { i =>
		clues.forall(c => state.variant.idTouched(i, c.toBase))
	})

	ops.copyWith(game, GameUpdates(
		common = Some(game.common.withThought(order) { _.copy(
			inferred = possibilities,
			possible = possibilities
		)}),
		state = Some(state.copy(
			deck = state.deck.updated(order, state.deck(order).copy(
				clued = true,
				clues = clues.map(c => CardClue(c.kind, c.value, c.giver.ordinal, 0)).toList
			))
		))
	))

/**
* Pre-clues the slot with obth colour and rank (only works for simple variants).
*/
def fullyKnown[G <: Game](playerIndex: Player, slot: Int, short: String)(game: G)(using ops: GameOps[G]) =
	val state = game.state
	val card = state.deck(state.hands(playerIndex.ordinal)(slot - 1))
	val id = state.expandShort(short)

	card.id().foreach { deckId =>
		if (deckId != id)
			throw new IllegalArgumentException(
				s"${state.names(playerIndex.ordinal)}'s card at slot $slot is not ${state.logId(id)}! found ${state.logId(deckId)}")
	}

	val giver = if (playerIndex == Player.Alice) Player.Bob else Player.Alice

	preClue(playerIndex, slot, Vector(
		TestClue(ClueKind.Rank, id.rank, giver),
		TestClue(ClueKind.Colour, id.suitIndex, giver)
	))(game)
