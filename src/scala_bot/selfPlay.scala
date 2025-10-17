package scala_bot

import scala_bot.basics._
import scala_bot.logger.{Logger, LogLevel}
import scala_bot.reactor.Reactor
import scala_bot.utils._

import java.nio.file.{Files, Paths}
import scala.util.Random
import scala.util.{Try, chaining}, chaining.scalaUtilChainingOps
import java.nio.charset.StandardCharsets

enum GameResult:
	case Perfect, Strikeout, DiscardedCrit, OutOfPace

case class GameSummary(
	score: Int,
	result: GameResult,
	actions: Vector[PerformAction],
	notes: List[List[String]]
)

def simulateGame(deck: Vector[Identity], variant: Variant): GameSummary =
	val games = (0 until 3).map { i =>
		val names = Vector("Alice", "Bob", "Cathy")
		val state = State(names, i, variant)

		Game(tableID = 0, state, inProgress = false, convention = Reactor)
			.copy(catchup = true)
			.pipe { g =>
				val state = g.state
				(0 until state.numPlayers).foldLeft(g) { (acc, playerIndex) =>
					(0 until HAND_SIZE(state.numPlayers)).foldLeft(acc) { (game, _) =>
						val order = game.state.nextCardOrder
						val action = DrawAction(
							playerIndex,
							order,
							if (playerIndex == i) -1 else deck(order).suitIndex,
							if (playerIndex == i) -1 else deck(order).rank,
						)
						game.handleAction(action)
					}
				}
			}
	}.toList

	def advance(result: (List[Game], Vector[PerformAction])) =
		val (games, actions) = result
		val currentPlayerIndex = games.head.state.currentPlayerIndex
		val currentGame = games(currentPlayerIndex)
		val perform = currentGame.takeAction

		val newGames = games.map { game =>
			val state = game.state
			val action = performToAction(state, perform, currentPlayerIndex, Some(deck))

			game.handleAction(action).when(!(_).state.ended) {
				_.when(_.state.nextCardOrder < deck.length) { g =>
					perform match {
						case PerformAction.Play(_) | PerformAction.Discard(_) =>
							val order = g.state.nextCardOrder

							g.handleAction(DrawAction(
								currentPlayerIndex,
								order,
								if (currentPlayerIndex == g.state.ourPlayerIndex) -1 else deck(order).suitIndex,
								if (currentPlayerIndex == g.state.ourPlayerIndex) -1 else deck(order).rank,
							))
						case _ => g
					}

				}
				.pipe { g =>
					g.handleAction(TurnAction(
						g.state.turnCount,
						g.state.nextPlayerIndex(currentPlayerIndex)
					))
				}
			}
		}

		(newGames, actions :+ perform)

	val initial = (games, Vector[PerformAction]())
	val (finalGames, actions) = Iterator.iterate(initial)(advance)
		.dropWhile((games, _) => !games.head.state.ended).next

	val game = finalGames.head
	val state = game.state
	val target = state.lastPlayerIndex(state.currentPlayerIndex)

	val finalActions = actions.when(_.last != PerformAction.Terminate)
		(_ :+ PerformAction.Terminate(target, 0))

	val result = if (state.strikes == 3)
		GameResult.Strikeout
	else if (state.score == state.variant.suits.length * 5)
		GameResult.Perfect
	else if (state.maxScore < state.variant.suits.length * 5)
		GameResult.DiscardedCrit
	else
		GameResult.OutOfPace

	GameSummary(
		score = state.score,
		result = result,
		actions = finalActions,
		notes = finalGames.map { g =>
			(0 until state.nextCardOrder)
				.map(g.notes.lift(_).map(_.full).getOrElse("")).toList
		}
	)

@main
def selfPlay(args: String*) =
	val parsedArgs = parseArgs(args)

	val numGames = parsedArgs.getOrElse("games", "1").toInt
	val seed = parsedArgs.getOrElse("seed", "0").toInt
	val variantName = parsedArgs.getOrElse("variant", "No Variant")

	Logger.setLevel(LogLevel.Error)
	Variant.init()
	val variant = Variant.getVariant(variantName)

	val deck = variant.allIds.flatMap(id => List.fill(variant.cardCount(id))(id))

	for (i <- (seed until seed + numGames)) {
		Random.setSeed(i)
		val shuffledDeck = Random.shuffle(deck).toVector
		val GameSummary(score, result, actions, notes) = simulateGame(shuffledDeck, variant)

		val actionsJSON = actions.map(_.json(tableID = 0))
		val data = ujson.Obj(
			"players" -> Seq("Alice", "Bob", "Cathy"),
			"deck" -> shuffledDeck.map(id => ujson.Obj("suitIndex" -> id.suitIndex, "rank" -> id.rank)),
			"actions" -> actionsJSON,
			"notes" -> notes,
			"options" -> ujson.Obj("variant" -> variant.name)
		).toString

		if (!Files.exists(Paths.get("seeds")))
			Try(Files.createDirectory(Paths.get("seeds")))
				.orElse(throw new Exception("failed to create seeds directory!")): Unit

		Files.write(Paths.get(s"seeds/$i.json"), data.getBytes(StandardCharsets.UTF_8))

		println(s"Seed $i: score $score, $result")
	}
