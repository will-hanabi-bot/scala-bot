package scala_bot

import cats.effect.{kernel, IO, std, unsafe}, kernel.Ref, std.Queue, unsafe.IORuntime
import cats.syntax.all._
import upickle.default._

import scala.concurrent.duration._

import scala_bot.basics._
import scala_bot.console.{ConsoleCmd, NavArg}
import scala_bot.reactor.Reactor
import scala_bot.logger._
import scala_bot.refSieve.RefSieve
import scala_bot.hgroup.HGroup

val BOT_VERSION = "v0.10.0 (scala-bot)"

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
		Action.fromJSON(json.obj("action")).map:
			GameActionMessage(json.obj("tableID").num.toInt, _)

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
	variantName: String,
	startingPlayer: Int = 0,
	deckPlays: Boolean = false,
	detrimentalCharacters: Boolean = false,
	emptyClues: Boolean = false,
	oneExtraCard: Boolean = false,
	oneLessCard: Boolean = false,
	speedrun: Boolean = false
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

enum Convention:
	case Reactor, RefSieve, HGroup

object Convention:
	def from(s: String) = s match
		case "Reactor" => Some(Convention.Reactor)
		case "RefSieve" => Some(Convention.RefSieve)
		case "HGroup" => Some(Convention.HGroup)
		case _ => None

case class Settings(convention: Convention, level: Int = 1):
	def str = convention match
		case Convention.Reactor => "Reactor 1.0"
		case Convention.RefSieve => "Ref Sieve"
		case Convention.HGroup => s"H-Group $level"

class BotClient(queue: Queue[IO, String], gameRef: Ref[IO, Option[Game]])(using runtime: IORuntime):
	private var info: Option[SelfData] = None
	private var tableID: Option[Int] = None
	private var gameStarted = false
	private var tables: Map[Int, Table] = Map()
	private var settings: Settings = Settings(Convention.Reactor)

	def debug(cmd: ConsoleCmd) = gameRef.get.flatMap:
		case None => IO.println("no active game")
		case Some(game) =>
			val state = game.state
			cmd match
				case ConsoleCmd.Hand(name, from) => state.names.indexOf(name) match
					case -1 =>
						Log.error(s"Player $name not found.")
						IO.unit
					case i =>
						val player = from.fold(game.common): name =>
							state.names.indexOf(name) match
								case -1 =>
									println(s"Player $from not found.")
									null
								case index =>
									game.players(index)

						if player != null then
							val output = List(
								s"viewing from ${player.name}",
								s"===================="
							).concat:
								state.hands(i).flatMap: order =>
									val meta = game.meta(order)
									val flags = List(
										Option.when(meta.focused)("focused"),
										Option.when(meta.hidden)("hidden"),
										Option.when(meta.trash)("trash"),
										Option.when(meta.urgent)("urgent"),
										Option.when(player.thoughts(order).reset)("reset")
									).flatten

									List(
										Some(s"$order: ${state.logId(order)} ${meta.status}"),
										Some(s"inferred: [${player.strInfs(state, order)}]"),
										player.thoughts(order).infoLock.mapA(info => s"info lock: [${info.fmt(state)}]"),
										Some(s"possible: [${player.strPoss(state, order)}]"),
										Some(s"reasoning: ${meta.reasoning}"),
										Option.when(!flags.isEmpty)(s"flags: $flags"),
										Some("====================")
									).flatten

							IO.println(output.mkString("\n"))
						else
							IO.unit
				case ConsoleCmd.Navigate(arg) =>
					val turn = arg match
						case NavArg.Turn(turn) => turn
						case NavArg.NextRound => state.turnCount + state.numPlayers
						case NavArg.Next => state.turnCount + 1
						case NavArg.Prev => state.turnCount - 1
						case NavArg.PrevRound => state.turnCount - state.numPlayers

					if turn < 1 || turn >= state.actionList.length then
						Log.error(s"Turn $turn does not exist.")
						IO.unit
					else
						game match
							case r: Reactor => gameRef.set(Some(r.navigate(turn)))
							case r: RefSieve => gameRef.set(Some(r.navigate(turn)))
							case r: HGroup => gameRef.set(Some(r.navigate(turn)))
							case _ => IO.unit

	def handleMsg(data: String): IO[Unit] =
		val (command, args) = data.splitAt(data.indexOf(' '))

		command match
			case "chat" =>
				handleChat(upickle.read[ChatMessage](ujson.read(args)))

			case "gameAction" =>
				GameActionMessage.fromJSON(ujson.read(args)) match
					case None =>
						IO.println(s"${Console.RED}failed to read gameAction $args${Console.RESET}") *>
						IO.unit
					case Some(action) => handleAction(action)

			case "gameActionList" =>
				val msg = GameActionListMessage.fromJSON(ujson.read(args))

				gameRef.get.flatMap:
					case None => throw new IllegalStateException("Game not initialized")
					case Some(g) => g match
						case r: Reactor => gameRef.set(Some(r.copy(catchup = true)))
						case r: RefSieve => gameRef.set(Some(r.copy(catchup = true)))
						case h: HGroup => gameRef.set(Some(h.copy(catchup = true)))
				*>
				msg.list.init.traverse_ : action =>
					handleAction(GameActionMessage(msg.tableID, action))
				*>
				gameRef.update:
					case Some(g2: Reactor) => Some(g2.copy(catchup = false))
					case Some(g2: RefSieve) => Some(g2.copy(catchup = false))
					case Some(g2: HGroup) => Some(g2.copy(catchup = false))
					case other => other
				*>
				sendCmd("loaded", ujson.write(ujson.Obj("tableID" -> msg.tableID))) *>
				IO.sleep(1000.millis) *>
				handleAction(GameActionMessage(msg.tableID, msg.list.last))

			case "joined" =>
				tableID = Some(ujson.read(args)("tableID").num.toInt)
				IO { gameStarted = false }

			case "init" =>
				handleInit(upickle.read[InitMessage](args))

			case "left" =>
				IO:
					tableID = None
					gameStarted = false

			case "table" =>
				val table = upickle.read[Table](args)

				gameRef.get.flatMap:
					// Only bots left in the replay
					case Some(g) if table.id == g.tableID &&
						table.sharedReplay &&
						table.spectators.forall(_.name.startsWith("will-bot")) =>
						leaveRoom()
					case _ => IO.unit
				*>
				IO { tables = tables + (table.id -> table) }

			case "tableGone" =>
				val id = ujson.read(args)("tableID").num.toInt
				IO { tables = tables - id }

			case "tableList" =>
				val list = upickle.read[List[Table]](args)
				IO { tables = tables ++ list.map(t => t.id -> t) } *>
				info.fold(IO.unit): self =>
					val myTable = tables.values
						.filter(t => t.players.contains(self.username))
						.maxByOption(_.id)
					myTable.fold(IO.unit): t =>
						Log.info(s"Trying to re-attend table ${t.id}")
						sendCmd("tableReattend", ujson.write(ujson.Obj("tableID" -> t.id)))

			case "tableStart" =>
				val id = ujson.read(args)("tableID").num.toInt

				IO { gameStarted = true } *>
				sendCmd("getGameInfo1", ujson.write(ujson.Obj("tableID" -> id)))

			case "warning" =>
				IO { println(s"warn: $args") }

			case "welcome" =>
				IO { info = Some(upickle.read[SelfData](args)) }

			case _ => IO.unit

	def leaveRoom(): IO[Unit] =
		val cmd = if gameStarted then "tableUnattend" else "tableLeave"

		IO.whenA(tableID.isDefined):
			sendCmd(cmd, ujson.write(ujson.Obj("tableID" -> tableID.get)))
		*>
		IO:
			tableID = None
			gameStarted = false
		*>
		gameRef.set(None)

	def handleInit(data: InitMessage) =
		val InitMessage(tID, playerNames, ourPlayerIndex, _, _, options) = data
		val variant = Variant.getVariant(options.variantName)
		val state = State(playerNames, ourPlayerIndex, variant, options)

		val game = settings.convention match
			case Convention.Reactor  => Reactor(tID, state, inProgress = true)
			case Convention.RefSieve => RefSieve(tID, state, inProgress = true)
			case Convention.HGroup   => HGroup(tID, state, inProgress = true, settings.level)

		IO { tableID = Some(tID) } *>
		gameRef.set(Some(game)) *>
		sendCmd("getGameInfo2", ujson.write(ujson.Obj("tableID" -> tID)))

	def handleChat(data: ChatMessage): IO[Unit] =
		val ChatMessage(msg, who, room, recipient) = data
		val withinRoom = recipient.isEmpty && room.startsWith("table")

		if withinRoom then
			if msg.startsWith("/setall") then
				assignSettings(data, false)
			else if msg.startsWith("/leaveall") then
				leaveRoom()
			else if msg.startsWith("/help") then
				sendPM(data.who, "See https://github.com/will-hanabi-bot/scala-bot?tab=readme-ov-file#supported-commands for help.")
			else
				IO.unit

		else if recipient != info.get.username then
			return IO.unit

		else if msg.startsWith("/join") then
			val table = tables.values.filter: t =>
				(t.players.contains(who) && !t.sharedReplay) ||
				t.spectators.exists(_.name == who)
			.maxByOption(_.id)

			table match
				case Some(t) if t.passwordProtected =>
					msg.split(" ").lift(1) match
						case Some(password) =>
							sendCmd("tableJoin", ujson.write(ujson.Obj("tableID" -> t.id, "password" -> password)))
						case None =>
							sendPM(who, "Room is password protected, please provide a password.")
				case Some(t) => sendCmd("tableJoin", ujson.write(ujson.Obj("tableID" -> t.id)))
				case None => sendPM(who, "Could not join, as you are not in a room.")

		else if msg.startsWith("/rejoin") then
			gameRef.get.flatMap:
				case Some(_) => sendPM(who, "Could not rejoin, as the bot is already in a game.")
				case None =>
					val table = tables.values
						.filter(_.players.contains(info.get.username))
						.maxByOption(_.id)

					table match
						case Some(t) => sendCmd("tableReattend", ujson.write(ujson.Obj("tableID" -> t.id)))
						case None => sendPM(who, "Could not rejoin, as the bot is not a player in any open room.")

		else if msg.startsWith("/settings") then
			assignSettings(data, true)

		else if msg.startsWith("/version") then
			sendPM(who, BOT_VERSION)

		else if msg.startsWith("/help") then
			sendPM(data.who, "See https://github.com/will-hanabi-bot/scala-bot?tab=readme-ov-file#supported-commands for help.")

		else if msg.startsWith("/analyze") then
			val pattern = """/analyze (\d+) (\w+) ?(\d+)?""".r

			msg match
				case pattern(id, conv, level) if Convention.from(conv).isDefined =>
					val args = List(
						Some(s"id=$id"),
						Some(s"convention=$conv"),
						Option.when(level != null)(s"level=$level")
					).flatten

					val analyzeIO = IO.blocking(fetchAnalyzeGame(args))
						.flatMap: comments =>
							comments.toList.traverse_(sendPM(who, _))
						.handleErrorWith: reason =>
							sendPM(who, s"Failed to analyze: $reason.")

						sendPM(who, "Analyzing... (may take up to 60s)") *> analyzeIO.start.void
				case _ =>
					sendPM(who, s"Couldn't parse command. Format is /analyze <id> <convention> [level].")

		else
			IO.unit

	def handleAction(data: GameActionMessage): IO[Unit] =
		val action = data.action
		gameRef.get.flatMap:
			case None => throw new IllegalStateException("Game not initialized")
			case Some(g) =>
				IO:
					g match
						case r: Reactor  => r.handleAction(action)
						case r: RefSieve => r.handleAction(action)
						case h: HGroup   => h.handleAction(action)
				.flatMap: newGame =>
					val state = newGame.state
					val queuedCmds = newGame.queuedCmds
					val perform = !newGame.catchup &&
						state.currentPlayerIndex == state.ourPlayerIndex &&
						!state.ended &&
						(action match
							case _: TurnAction => true
							case _: DrawAction => state.turnCount == 1
							case _ => false)

					val (turn1IO, nextGame) = action match
						case TurnAction(num, _) if num == 1 && !newGame.notes.contains(0) =>
							val note = s"[INFO: $BOT_VERSION, ${settings.str}]"
							val noteIO = sendCmd("note", ujson.write(ujson.Obj("tableID" -> newGame.tableID, "order" -> 0, "note" -> note)))
							val nextGame = newGame match
								case r: Reactor  => r.copy(notes = r.notes + (0 -> Note(0, note, note)))
								case r: RefSieve => r.copy(notes = r.notes + (0 -> Note(0, note, note)))
								case h: HGroup   => h.copy(notes = h.notes + (0 -> Note(0, note, note)))
							(noteIO, nextGame)

						case _ => (IO.unit, newGame)

					val actIO = IO.whenA(perform):
						val suggestedActionIO = nextGame match
							case r: Reactor  => r.takeAction
							case r: RefSieve => r.takeAction
							case h: HGroup   => h.takeAction

						suggestedActionIO.flatMap: suggestedAction =>
							Log.highlight(Console.BLUE, s"Suggested action: ${suggestedAction.fmt(newGame, accordingTo = Some(newGame.me))}")
							val arg = suggestedAction.json(tableID.get)

							IO.whenA(nextGame.inProgress):
								IO.sleep(2.seconds) *> sendCmd("action", ujson.write(arg))

					val x = nextGame match
						case r: Reactor  => r.copy(queuedCmds = Nil)
						case r: RefSieve => r.copy(queuedCmds = Nil)
						case h: HGroup   => h.copy(queuedCmds = Nil)

					gameRef.set(Some(x)) *>
					turn1IO *>
					queuedCmds.traverse_(sendCmd(_, _)) *>
					actIO

	def assignSettings(data: ChatMessage, pm: Boolean) =
		val reply = pm match
			case true => (msg: String) => sendPM(data.who, msg)
			case false => (msg: String) => sendChat(msg)

		data.msg.split(" ") match
			case Array(_) =>
				reply(s"Currently playing with ${settings.str} conventions.")

			case Array(_, level) if level.toIntOption.isDefined =>
				level.toIntOption match
					case None => reply(s"Unrecognized level $level.")
					case Some(l) if l < 1 || l > 11 =>
						reply(s"scala-bot can only play HGroup between levels 1-11.")
					case Some(l) =>
						settings = settings.copy(convention = Convention.HGroup, level = l)
						reply(s"Currently playing with ${settings.str} conventions.")

			case Array(_, conv) => Convention.from(conv) match
				case None => reply(s"Unrecognized convention $conv. Supported: HGroup, RefSieve, Reactor.")
				case Some(c) =>
					settings = settings.copy(convention = c)
					reply(s"Currently playing with ${settings.str} conventions.")

			case Array(_, conv, level) => Convention.from(conv) match
				case None => reply(s"Unrecognized convention $conv. Supported: HGroup, RefSieve, Reactor.")
				case Some(c) if conv == "HGroup" =>
					level.toIntOption match
						case None => reply(s"Unrecognized level $level.")
						case Some(l) if l < 1 || l > 11 =>
							reply(s"scala-bot can only play HGroup between levels 1-11.")
						case Some(l) =>
							settings = settings.copy(convention = c, level = l)
							reply(s"Currently playing with ${settings.str} conventions.")
				case Some(c) =>
					settings = settings.copy(convention = c)
					reply(s"Currently playing with ${settings.str} conventions ($conv doesn't support levels).")

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
