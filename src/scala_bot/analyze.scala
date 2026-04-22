package scala_bot

import scala_bot.basics._
import scala_bot.reactor.Reactor
import scala_bot.refSieve.RefSieve
import scala_bot.hgroup.HGroup
import scala_bot.utils._

import scala_bot.logger.{Logger, LogLevel}

import cats.effect.{ExitCode, IO, IOApp, unsafe}, unsafe.IORuntime

def fetchAnalyzeGame(args: Seq[String])(using runtime: IORuntime) =
	val parsedArgs = parseArgs(args)

	val List(id, file, conventionR) = List("id", "file", "convention").map(parsedArgs.lift(_))

	if id.isEmpty && file.isEmpty then
		throw new IllegalArgumentException("Must provide either id or file argument.")

	val convention = conventionR.flatMap(Convention.from(_).toOption).getOrElse(Convention.Reactor)

	val data @ GameData(players, deck, actions, options) = id match
		case Some(id) => GameData.fetchId(id)
		case None     => GameData.fetchFile(file.get)

	Variant.init()
	val variant = Variant.getVariant(options.variantName)

	val state = State(players, 0, variant, options)
	convention match
		case Convention.Reactor =>
			val game = Reactor(0, state, false).copy(catchup = true)
			analyzeGame(game, data)
		case Convention.RefSieve =>
			val game = RefSieve(0, state, false).copy(catchup = true)
			analyzeGame(game, data)
		case Convention.HGroup(level) =>
			val game = HGroup(0, state, false, level).copy(catchup = true)
			analyzeGame(game, data)

def analyzeGame[G <: Game](game: G, data: GameData)(using ops: GameOps[G], runtime: IORuntime): Seq[String] =
	val GameData(players, deck, actions, options) = data

	Logger.setLevel(LogLevel.Off)

	// Reconstruct the game from data
	val finalGame = game
		.pipe: g =>
			(0 until g.state.numPlayers).foldLeft(g): (a, playerIndex) =>
				(0 until HAND_SIZE(g.state.numPlayers)).foldLeft(a): (acc, _) =>
					val order = acc.state.nextCardOrder
					acc.handleAction(DrawAction(playerIndex, order, deck(order).suitIndex, deck(order).rank))
		.pipe:
			actions.foldLeft(_): (acc, perform) =>
				val playerIndex = acc.state.currentPlayerIndex
				acc.handleAction(perform.toAction(acc.state, playerIndex, Some(deck)))
					.when(_.state.nextCardOrder < deck.length): a =>
						perform match
							case PerformAction.Play(_) | PerformAction.Discard(_) =>
								val order = a.state.nextCardOrder
								a.handleAction(DrawAction(playerIndex, order, deck(order).suitIndex, deck(order).rank))
							case _ => a
					.when(p => perform.isInstanceOf[PerformAction.Play] && p.state.strikes == 3):
						_.handleAction(GameOverAction(0, playerIndex))
					.pipe: g =>
						val nextPlayerIndex = g.state.nextPlayerIndex(g.state.currentPlayerIndex)
						g.handleAction(TurnAction(g.state.turnCount, nextPlayerIndex))
		.pipe(ops.copyWith(_, GameUpdates(catchup = Some(false))))

	Logger.setLevel(LogLevel.Info)

	finalGame.analyze

object analyze extends IOApp:
	def run(args: List[String]) =
		IO.blocking {
			val comments = fetchAnalyzeGame(args)(using runtime)
			println(comments.mkString("\n"))
			ExitCode.Success
		}
