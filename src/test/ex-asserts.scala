package scala_bot.test

import scala_bot.basics.Game
import scala_bot.basics.IdentitySet

def hasInfs(game: Game, playerIndex: Option[Player], target: Player, slot: Int, infs: Vector[String]) =
	val order = game.state.hands(target.ordinal).lift(slot - 1).getOrElse(
		throw new Error(s"Slot $slot doesn't exist"))

	val player = playerIndex.map(i => game.players(i.ordinal)).getOrElse(game.common)
	val thought = player.thoughts(order)

	val idset = IdentitySet.from(infs.map(game.state.expandShort))
	assert(thought.inferred == idset, s"Differing inferences (order $order). Expected ${infs.mkString(",")}, got ${player.strInfs(game.state, order)}")

def hasPoss(game: Game, playerIndex: Option[Player], target: Player, slot: Int, poss: Vector[String]) =
	val order = game.state.hands(target.ordinal).lift(slot - 1).getOrElse(
		throw new Error(s"Slot $slot doesn't exist"))

	val player = playerIndex.map(i => game.players(i.ordinal)).getOrElse(game.common)
	val thought = player.thoughts(order)

	val idset = IdentitySet.from(poss.map(game.state.expandShort))
	assert(thought.possible == idset, s"Differing possibilities (order $order). Expected ${poss.mkString(",")}, got ${player.strPoss(game.state, order)}")
