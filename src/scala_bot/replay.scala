package scala_bot

import scala_bot.basics._
import scala_bot.console.{ConsoleCmd, spawnConsole}
import scala_bot.reactor.Reactor
import scala_bot.refSieve.RefSieve
import scala_bot.hgroup.HGroup
import scala_bot.utils._

import scala.io.Source._
import cats.effect.std.Queue
import cats.effect.{IO, IOApp, ExitCode}
import cats.effect.kernel.Ref

case class GameData(
	players: Vector[String],
	deck: Vector[Identity],
	actions: Vector[PerformAction],
	options: TableOptions
)

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

		val opts = data.obj.lift("options").map(_.objOpt).flatten
		val variant = opts.map(_.lift("variant").map(_.str)).flatten
		val deckPlays = opts.map(_.lift("deckPlays").map(_.bool)).flatten

		val options = TableOptions(
			players.length,
			variantName = variant.getOrElse("No Variant"),
			deckPlays = deckPlays.getOrElse(false)
		)

		GameData(players, deck, actions, options)

def fetchGame(args: Seq[String]) =
	val parsedArgs = parseArgs(args)

	val List(id, indexR, file, conventionR, levelR) = List("id", "index", "file", "convention", "level").map(parsedArgs.lift(_))

	if id.isEmpty && file.isEmpty then
		throw new IllegalArgumentException("Must provide either id or file argument.")

	if indexR.isEmpty then
		throw new IllegalArgumentException("Missing required argument 'index'!")

	val index = indexR.get.toInt
	val convention = conventionR.flatMap(Convention.from).getOrElse(Convention.Reactor)
	val level = levelR.map(_.toInt).getOrElse(1)

	val data @ GameData(players, deck, actions, options) = id match
		case Some(id) => GameData.fetchId(id)
		case None     => GameData.fetchFile(file.get)

	if index > players.length then
		throw new IllegalArgumentException(s"Replay only has ${players.length} players!")

	Variant.init()
	val variant = Variant.getVariant(options.variantName)

	val state = State(players, index, variant, options)
	convention match
		case Convention.Reactor =>
			val game = Reactor(0, state, false).copy(catchup = true)
			processGame(game, data, index)
		case Convention.RefSieve =>
			val game = RefSieve(0, state, false).copy(catchup = true)
			processGame(game, data, index)
		case Convention.HGroup =>
			val game = HGroup(0, state, false, level).copy(catchup = true)
			processGame(game, data, index)

def processGame[G <: Game](game: G, data: GameData, index: Int)(using ops: GameOps[G]) =
	val GameData(players, deck, actions, options) = data
	game
		.pipe: g =>
			(0 until g.state.numPlayers).foldLeft(g): (a, playerIndex) =>
				(0 until HAND_SIZE(g.state.numPlayers)).foldLeft(a): (acc, _) =>
					val order = acc.state.nextCardOrder
					acc.handleAction(DrawAction(
						playerIndex,
						order,
						if playerIndex == index then -1 else deck(order).suitIndex,
						if playerIndex == index then -1 else deck(order).rank
					))
		.pipe:
			actions.foldLeft(_): (acc, perform) =>
				val playerIndex = acc.state.currentPlayerIndex
				acc.handleAction(perform.toAction(acc.state, playerIndex, Some(deck)))
					.when(_.state.nextCardOrder < deck.length): a =>
						perform match
							case PerformAction.Play(_) | PerformAction.Discard(_) =>
								val order = a.state.nextCardOrder
								a.handleAction(DrawAction(
									playerIndex,
									order,
									if playerIndex == index then -1 else deck(order).suitIndex,
									if playerIndex == index then -1 else deck(order).rank
								))
							case _ => a
					.when(p => perform.isInstanceOf[PerformAction.Play] && p.state.strikes == 3):
						_.handleAction(GameOverAction(0, playerIndex))
					.pipe: g =>
						val nextPlayerIndex = g.state.nextPlayerIndex(g.state.currentPlayerIndex)
						g.handleAction(TurnAction(g.state.turnCount, nextPlayerIndex))
		.pipe(ops.copyWith(_, GameUpdates(catchup = Some(false))))

object replay extends IOApp:
	def run(args: List[String]) =
		for
			wsQueue  <- Queue.unbounded[IO, String]
			consoleQ <- Queue.unbounded[IO, ConsoleCmd]
			game = fetchGame(args)
			gameRef  <- Ref.of[IO, Option[Game]](Some(game))
			client = new BotClient(wsQueue, gameRef)(using runtime)
			console  <- spawnConsole(consoleQ, client)
			_ <- console.join
		yield (ExitCode.Success)
