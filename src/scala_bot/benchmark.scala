package scala_bot

import scala_bot.basics._
import scala_bot.logger.{Logger, LogLevel}
import scala_bot.reactor.Reactor
import scala_bot.refSieve.RefSieve
import scala_bot.hgroup.HGroup
import scala_bot.utils._

import java.time.Instant
import scala.util.Random

@main
def benchmark(args: String*) =
	val parsedArgs = parseArgs(args)

	val variantName = parsedArgs.getOrElse("variant", "No Variant")
	val convention = parsedArgs.lift("convention").flatMap(Convention.from(_).toOption).getOrElse(Convention.Reactor)
	val numPlayers = parsedArgs.getOrElse("players", "3").toInt

	Logger.setLevel(LogLevel.Off)
	Variant.init()
	val variant = Variant.getVariant(variantName)

	val deck = variant.allIds.flatMap(id => List.fill(variant.cardCount(id))(id))
	val rng = Random(314)
	val shuffledDeck = rng.shuffle(deck).toVector
	val states = (0 until numPlayers).map(State(NAMES.take(numPlayers), _, variant, TableOptions(numPlayers, variant.name)))

	var sumTimes = 0L

	loop(0, _ < 10, _ + 1): i =>
		val start = Instant.now()
		val GameSummary(score, result, actions, notes) = convention match
			case Convention.Reactor       => simulateGame(states.map(Reactor(0, _, false)), shuffledDeck)
			case Convention.RefSieve      => simulateGame(states.map(RefSieve(0, _, false)), shuffledDeck)
			case Convention.HGroup(level) => simulateGame(states.map(HGroup(0, _, false, level)), shuffledDeck)

		if i > 3 then
			sumTimes = sumTimes + start.until(Instant.now()).toMillis()

	val avgTime = sumTimes / 6.0
	println(s"average time: $avgTime ms")
