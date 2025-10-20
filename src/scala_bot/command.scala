package scala_bot

import cats.effect.IO
import cats.syntax.all._
import cats.effect.std.Queue
import cats.effect.kernel.Ref
import upickle.default._
import java.util.{Timer,TimerTask}
import scala.concurrent.duration._

import scala_bot.basics._
import scala_bot.basics.given_Conversion_IdentitySet_Iterable
import scala_bot.console.{ConsoleCmd, NavArg}
import scala_bot.reactor.Reactor
import scala_bot.logger._

val timer = new Timer
def delay(f: () => Unit, n: Long) =
	timer.schedule(new TimerTask() { def run = f() }, n)

val BOT_VERSION = "v0.1.0 (scala-bot)"

case class ChatMessage(
	msg: String,
	who: String,
	room: String,
	recipient: String
) derives ReadWriter

case class GameActionMessage(
	tableID: Int,
	action: Action
)

object GameActionMessage:
	def fromJSON(json: ujson.Value): Option[GameActionMessage] =
		Action.fromJSON(json.obj("action")).map {
			GameActionMessage(json.obj("tableID").num.toInt, _)
		}

case class Spectator(
	name: String,
	shadowingPlayerIndex: Int,
	shadowingPlayerUsername: String
) derives ReadWriter

case class Table(
	id: Int,
	name: String,
	passwordProtected: Boolean,
	joined: Boolean,
	owned: Boolean,
	running: Boolean,
	variant: String,
	options: TableOptions,
	sharedReplay: Boolean,
	progress: Int,
	players: Vector[String],
	spectators: List[Spectator],
	maxPlayers: Int
) derives ReadWriter

case class TableOptions(
	numPlayers: Int,
	startingPlayer: Int,
	variantName: String
) derives ReadWriter

case class InitMessage(
	tableID: Int,
	playerNames: Vector[String],
	ourPlayerIndex: Int,
	replay: Boolean,
	seed: String,
	options: TableOptions
) derives ReadWriter

case class SelfData(
	userID: Int,
	username: String,
	playingAtTables: List[Int],
	randomTableName: String
) derives ReadWriter

case class GameActionListMessage(
	tableID: Int,
	list: Seq[Action]
)

object GameActionListMessage:
	def fromJSON(json: ujson.Value): GameActionListMessage =
		GameActionListMessage(
			json.obj("tableID").num.toInt,
			json.obj("list").arr.map(Action.fromJSON).flatten.toSeq
		)

class BotClient(queue: Queue[IO, String], gameRef: Ref[IO, Option[Game]]):
	private var info: Option[SelfData] = None
	private var tableID: Option[Int] = None
	private var gameStarted = false
	private var tables: Map[Int, Table] = Map()

	def debug(cmd: ConsoleCmd) = gameRef.get.flatMap {
		case None => IO.println("no active game")
		case Some(game) =>
			val state = game.state
			cmd match {
				case ConsoleCmd.Hand(name, from) => state.names.indexWhere(_ == name) match {
					case -1 =>
						Log.error(s"Player $name not found.")
						IO.unit
					case i =>
						val hand = state.hands(i)
						val player = from match {
								case None => game.common
								case Some(name) => state.names.indexWhere(_ == name) match {
									case -1 =>
										println(s"Player $from not found.")
										null
									case index =>
										game.players(index)
								}
						}

						if (player != null)
							val output = List(
								s"viewing from common",
								s"===================="
							).concat {
								hand.flatMap { order =>
									val meta = game.meta(order)
									val flags = List(
										Option.when(meta.focused)("focused"),
										Option.when(meta.trash)("trash"),
										Option.when(meta.urgent)("urgent"),
										Option.when(player.thoughts(order).reset)("reset")
									).flatten

									List(
										Some(s"$order: ${state.logId(order)} ${meta.status}"),
										Some(s"inferred: [${player.strInfs(state, order)}]"),
										player.thoughts(order).infoLock.map(info => s"info lock: [${info.map(state.logId).mkString(",")}]"),
										Some(s"possible: [${player.strPoss(state, order)}]"),
										Some(s"reasoning: ${meta.reasoning}"),
										Option.when(!flags.isEmpty)(s"flags: ${flags.toList}"),
										Some("====================")
									).flatten
								}
							}
							IO.println(output.mkString("\n"))
						else
							IO.unit
					}
				case ConsoleCmd.Navigate(arg) =>
					val turn = arg match {
						case NavArg.Turn(turn) => turn
						case NavArg.NextRound => state.turnCount + state.numPlayers
						case NavArg.Next => state.turnCount + 1
						case NavArg.Prev => state.turnCount - 1
						case NavArg.PrevRound => state.turnCount - state.numPlayers
					}

					if (turn < 1 || turn >= state.actionList.length) {
						Log.error(s"Turn $turn does not exist.")
						IO.unit
					}
					else {
						gameRef.set(Some(game.navigate(turn)))
					}
			}
		}

	def handleMsg(data: String): IO[Unit] =
		val (command, args) = data.splitAt(data.indexOf(' '))

		command match {
			case "chat" =>
				handleChat(upickle.read[ChatMessage](ujson.read(args)))

			case "gameAction" =>
				GameActionMessage.fromJSON(ujson.read(args)) match {
					case None =>
						IO.println(s"${Console.RED}failed to read gameAction $args${Console.RESET}") *>
						IO.unit
					case Some(action) => handleAction(action)
				}

			case "gameActionList" =>
				val msg = GameActionListMessage.fromJSON(ujson.read(args))

				gameRef.get.flatMap {
					case None => throw new IllegalStateException("Game not initialized")
					case Some(g) =>
						gameRef.set(Some(g.copy(catchup = true))) *>
						msg.list.init.traverse_ { action =>
							handleAction(GameActionMessage(msg.tableID, action))
						} *>
						gameRef.get.flatMap {
							case Some(g2) => gameRef.set(Some(g2.copy(catchup = false)))
							case None => IO.unit
						} *>
						sendCmd("loaded", ujson.write(ujson.Obj("tableID" -> msg.tableID))) *>
						IO.sleep(1000.millis) *>
						handleAction(GameActionMessage(msg.tableID, msg.list.last))
				}

			case "joined" =>
				tableID = Some(ujson.read(args)("tableID").num.toInt)
				IO { gameStarted = false }

			case "init" =>
				handleInit(upickle.read[InitMessage](args))

			case "left" =>
				IO {
					tableID = None
					gameStarted = false
				}

			case "table" =>
				val table = upickle.read[Table](args)
				IO { tables = tables + (table.id -> table) }

			case "tableGone" =>
				val id = ujson.read(args)("tableID").num.toInt
				IO { tables = tables - id }

			case "tableList" =>
				val list = upickle.read[List[Table]](args)
				IO { tables = tables ++ list.map(t => t.id -> t) }

			case "tableStart" =>
				val id = ujson.read(args)("tableID").num.toInt
				sendCmd("getGameInfo1", ujson.write(ujson.Obj("tableID" -> id)))

			case "warning" =>
				IO { println(s"warn: $args") }

			case "welcome" =>
				IO { info = Some(upickle.read[SelfData](args)) }

			case _ => IO.unit
		}

	def leaveRoom(): IO[Unit] =
		val cmd = if (gameStarted) "tableUnattend" else "tableLeave"
		sendCmd(cmd, ujson.write(ujson.Obj("tableID" -> tableID.get))) *>
		IO {
			tableID = None
			gameStarted = false
		} *>
		gameRef.set(None)

	def handleInit(data: InitMessage) =
		val InitMessage(tID, playerNames, ourPlayerIndex, _, _, options) = data
		val variant = Variant.getVariant(options.variantName)
		val state = State(playerNames, ourPlayerIndex, variant)

		IO { tableID = Some(tID) } *>
		gameRef.set(Some(Game(tID, state, Reactor, inProgress = true))) *>
		sendCmd("getGameInfo2", ujson.write(ujson.Obj("tableID" -> tID)))

	def handleChat(data: ChatMessage): IO[Unit] =
		val ChatMessage(msg, who, room, recipient) = data
		val withinRoom = recipient.isEmpty && room.startsWith("table")

		if (withinRoom)
			if (msg.startsWith("/setall"))
				assignSettings(data, false)
			else if (msg.startsWith("/leaveall"))
				leaveRoom()
			else
				IO.unit

		else if (recipient != info.get.username)
			return IO.unit

		else if (msg.startsWith("/join"))
			val table = tables.values.filter(t =>
				(t.players.contains(who) && !t.sharedReplay) ||
				t.spectators.exists(_.name == who))
				.maxByOption(_.id)

			table match {
				case Some(t) if t.passwordProtected =>
					msg.split(" ").lift(1) match {
						case Some(password) =>
							sendCmd("tableJoin", ujson.write(ujson.Obj("tableID" -> t.id, "password" -> password)))
						case None =>
							sendPM(who, "Room is password protected, please provide a password.")
					}
				case Some(t) => sendCmd("tableJoin", ujson.write(ujson.Obj("tableID" -> t.id)))
				case None => sendPM(who, "Could not join, as you are not in a room.")
			}

		else if (msg.startsWith("/rejoin"))
			gameRef.get.flatMap {
				case Some(_) => sendPM(who, "Could not rejoin, as the bot is already in a game.")
				case None =>
					val table = tables.values
						.filter(_.players.contains(info.get.username))
						.maxByOption(_.id)

					table match {
						case Some(t) => sendCmd("tableReattend", ujson.write(ujson.Obj("tableID" -> t.id)))
						case None => sendPM(who, "Could not rejoin, as the bot is not a player in any open room.")
					}
			}

		else if (msg.startsWith("/version"))
			sendPM(who, BOT_VERSION)

		else
			IO.unit

	def handleAction(data: GameActionMessage): IO[Unit] =
		val action = data.action
		gameRef.get.flatMap {
			case None => throw new IllegalStateException("Game not initialized")
			case Some(g) =>
				IO {
					g.handleAction(action)
				}
				.flatMap { newGame =>
					val state = newGame.state
					val queuedCmds = newGame.queuedCmds
					val perform = !newGame.catchup &&
						state.currentPlayerIndex == state.ourPlayerIndex &&
						!state.ended &&
						(action match {
							case TurnAction(_, _) => true
							case DrawAction(_, _, _, _) => state.turnCount == 1
							case _ => false
						})

					val turn1IO = action match {
						case TurnAction(num, _) if num == 1 && newGame.notes.contains(0) =>
							val note = s"[INFO: v$BOT_VERSION]"
							sendCmd("note", ujson.write(ujson.Obj("order" -> 0, "note" -> note)))
						case _ => IO.unit
					}

					val actIO = IO.whenA(perform) {
						val suggestedAction = newGame.takeAction
						Log.highlight(Console.BLUE, s"Suggested action: ${suggestedAction.fmt(newGame)}")
						val arg = suggestedAction.json(tableID.get)

						IO.whenA(newGame.inProgress) {
							IO.sleep(2.seconds) *> sendCmd("action", ujson.write(arg))
						}
					}

					gameRef.set(Some(newGame.copy(queuedCmds = List()))) *>
					turn1IO *>
					queuedCmds.traverse_(sendCmd(_, _)) *>
					actIO
				}
		}

	def assignSettings(data: ChatMessage, pm: Boolean) =
		val reply = pm match {
			case true => (msg: String) => sendPM(data.who, msg)
			case false => (msg: String) => sendChat(msg)
		}
		reply(s"Currently playing with Reactor 1.0 conventions.")

	def sendPM(recipient: String, msg: String) =
		queue.offer(s"chatPM ${ujson.Obj(
			"msg" -> msg,
			"recipient" -> recipient,
			"room" -> "lobby"
		).toString}")

	def sendChat(msg: String) =
		queue.offer(s"chat ${ujson.Obj(
			"msg" -> msg,
			"recipient" -> "",
			"room" -> s"table${tableID.get}"
		).toString}")

	def sendCmd(cmd: String, args: String) =
		println(s"${Console.RED}Sending command: $cmd $args${Console.RESET}")
		queue.offer(s"$cmd $args")
