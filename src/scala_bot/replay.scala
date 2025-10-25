package scala_bot

import scala_bot.basics._
import scala_bot.console.{ConsoleCmd, spawnConsole}
import scala_bot.reactor.Reactor
import scala_bot.utils._

import scala.io.Source._
import scala.util.{Try, chaining}, chaining.scalaUtilChainingOps
import cats.effect.std.Queue
import cats.effect.{IO, IOApp, ExitCode}
import cats.effect.kernel.Ref
import scala_bot.refSieve.RefSieve

case class GameData(
	players: Vector[String],
	deck: Vector[Identity],
	actions: Vector[PerformAction],
	options: Option[ReplayOptions]
)

case class ReplayOptions(variant: String)

object GameData:
	def fetchId(id: String) =
		parse(ujson.read(requests.get(s"https://hanab.live/export/${id}").text()))

	def fetchFile(file: String) =
		parse(ujson.read(fromFile(file).mkString))

	def parse(data: ujson.Value) =
		val players = data("players").arr.map(_.str).toVector
		val deck = data("deck").arr
			.map(id => Identity(id("suitIndex").num.toInt, id("rank").num.toInt))
			.toVector

		val actions = data("actions").arr.map(PerformAction.fromJSON).toVector
		val options = Try {
			val variant = data("options").obj("variant").str
			Some(ReplayOptions(variant))
		}.getOrElse(None)

		GameData(players, deck, actions, options)

def fetchGame(args: Seq[String]) =
	val parsedArgs = parseArgs(args)

	val List(id, indexR, file, conventionR) = List("id", "index", "file", "convention").map(parsedArgs.lift(_))

	if (id.isEmpty && file.isEmpty)
		throw new IllegalArgumentException("Must provide either id or file argument.")

	if (indexR.isEmpty)
		throw new IllegalArgumentException("Missing required argument 'index'!")

	val index = indexR.get.toInt
	val convention = Convention.from(conventionR.getOrElse("Reactor"))

	val data @ GameData(players, deck, actions, options) = id match {
		case Some(id) => GameData.fetchId(id)
		case None => GameData.fetchFile(file.get)
	}

	if (index > players.length)
		throw new IllegalArgumentException(s"Replay only has ${players.length} players!")

	Variant.init()
	val variant = Variant.getVariant(options.map(_.variant).getOrElse("No Variant"))

	val state = State(players, index, variant)
	convention match {
		case Convention.Reactor =>
			val game = Reactor(0, state, false).copy(catchup = true)
			processGame(game, data, index)
		case Convention.RefSieve =>
			val game = RefSieve(0, state, false).copy(catchup = true)
			processGame(game, data, index)
	}

def processGame[G <: Game](game: G, data: GameData, index: Int)(using ops: GameOps[G]) =
	val GameData(players, deck, actions, options) = data
	game
		.pipe { g =>
			(0 until g.state.numPlayers).foldLeft(g) { (a, playerIndex) =>
				(0 until HAND_SIZE(g.state.numPlayers)).foldLeft(a) { (acc, _) =>
					val order = acc.state.nextCardOrder
					acc.handleAction(DrawAction(
						playerIndex,
						order,
						if (playerIndex == index) -1 else deck(order).suitIndex,
						if (playerIndex == index) -1 else deck(order).rank
					))
				}
			}
		}
		.pipe {
			actions.foldLeft(_) { (acc, action) =>
				val playerIndex = acc.state.currentPlayerIndex
				acc.handleAction(performToAction(acc.state, action, playerIndex, Some(deck)))
					.when(_.state.nextCardOrder < deck.length) { a =>
						action match {
							case PerformAction.Play(_) | PerformAction.Discard(_) =>
								val order = a.state.nextCardOrder
								a.handleAction(DrawAction(
									playerIndex,
									order,
									if (playerIndex == index) -1 else deck(order).suitIndex,
									if (playerIndex == index) -1 else deck(order).rank
								))
							case _ => a
						}
					}
					.when(a => action.isInstanceOf[PerformAction.Play] && a.state.strikes == 3) {
						_.handleAction(GameOverAction(0, playerIndex))
					}
					.pipe { g =>
						val nextPlayerIndex = g.state.nextPlayerIndex(g.state.currentPlayerIndex)
						g.handleAction(TurnAction(g.state.turnCount, nextPlayerIndex))
					}
			}
		}
		.pipe(ops.copyWith(_, GameUpdates(catchup = Some(false))))

object replay extends IOApp {
	def run(args: List[String]) =
		for {
			wsQueue  <- Queue.unbounded[IO, String]
			consoleQ <- Queue.unbounded[IO, ConsoleCmd]
			game = fetchGame(args)
			gameRef  <- Ref.of[IO, Option[Game]](Some(game))
			client = new BotClient(wsQueue, gameRef)
			console  <- spawnConsole(consoleQ, client)
			_ <- console.join
		} yield (ExitCode.Success)
}
