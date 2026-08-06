package scala_bot

import scala_bot.basics._
import scala_bot.logger.{Logger, LogLevel}
import scala_bot.reactor.Reactor
import scala_bot.refSieve.RefSieve
import scala_bot.hgroup.HGroup
import scala_bot.utils._

import java.time.Instant
import scala.util.Random

// NOTE: Don't forget to comment out the endgame solver!

@main
def benchmark(args: String*) =
	val parsedArgs = parseArgs(args)

	val variantName = parsedArgs.getOrElse("variant", "No Variant")
	val convention = parsedArgs.lift("convention").flatMap(Convention.from(_).toOption).getOrElse(Convention.Reactor)
	val numPlayers = parsedArgs.getOrElse("players", "3").toInt

	val seed = 314
	val numGames = 5
	val repetitions = 50
	val warmup = 30

	Logger.setLevel(LogLevel.Off)
	Variant.init()
	val variant = Variant.getVariant(variantName)
	val playerNames = NAMES.take(numPlayers)
	val deck = variant.allIds.flatMap(id => Vector.fill(variant.cardCount(id))(id))
	val tableOpts = TableOptions(numPlayers, variant.name)

	val gameData =
		for
			seed <- seed until (seed + numGames)
			rng = Random(seed)
			shuffledDeck = rng.shuffle(deck).toVector
			states = (0 until numPlayers).map(State(playerNames, _, variant, tableOpts))
		yield
			(states, shuffledDeck)

	var sumTimes = 0L

	loop(0, _ < repetitions, _ + 1): i =>
		val start = Instant.now()

		loop(0, _ < numGames, _ + 1): j =>
			val (states, shuffledDeck) = gameData(j)

			val GameSummary(score, result, actions, notes) = convention match
				case Convention.Reactor       => simulateGame(states.map(Reactor(0, _, inProgress = false)), shuffledDeck)
				case Convention.RefSieve      => simulateGame(states.map(RefSieve(0, _, inProgress = false)), shuffledDeck)
				case Convention.HGroup(level) => simulateGame(states.map(HGroup(0, _, inProgress = false, level)), shuffledDeck)

		if i >= warmup then
			sumTimes = sumTimes + start.until(Instant.now()).toMillis()

	val avgTime = sumTimes / (repetitions - warmup)
	println(s"average time: $avgTime ms")
