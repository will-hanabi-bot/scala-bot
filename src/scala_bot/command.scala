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
import scala_bot.hgroup.{HGroup, Level}

case class ChatAuthor(username: String = "") derives Reader

case class ChatKind(
	`type`: String,
	author: ChatAuthor = ChatAuthor(),
	recipient: String = ""
) derives Reader

case class ChatMessage(
	text: String,
	tableId: Int = -1,
	kind: ChatKind
) derives Reader:
	def username = kind.author.username
	def recipient = kind.recipient

/** The table of a table's chat list. The lobby's chat list has none, which reads as the table -1. */
case class ChatListTable(tableId: Int = -1) derives Reader

case class ChatList(messages: Vector[ChatMessage], table: ChatListTable = ChatListTable()) derives Reader

case class Spectator(username: String) derives Reader

case class TableSettings(options: TableOptions = TableOptions()) derives Reader

case class TablePhase(`type`: String = "pregame") derives Reader

case class Table(
	tableId: Int,
	passwordProtected: Boolean = false,
	phase: TablePhase = TablePhase(),
	settings: TableSettings = TableSettings(),
	playerNames: Vector[String],
	spectators: List[Spectator] = Nil
) derives Reader:
	def running = phase.`type` == "playing"
	def sharedReplay = phase.`type` == "sharedReplay"

case class TableList(tables: List[Table]) derives Reader

case class PregamePlayer(userId: Int) derives Reader

case class Pregame(tableId: Int, players: Vector[PregamePlayer]) derives Reader

case class TableJoined(tableId: Int, stage: String) derives Reader

case class TableOptions(
	variantId: Int = 0,
	deckPlays: Boolean = false,
	detrimentalCharacters: Boolean = false,
	emptyClues: Boolean = false,
	oneExtraCard: Boolean = false,
	oneLessCard: Boolean = false
) derives Reader

case class ViewMode(ourPlayerIndex: Int = 0) derives Reader

case class InitMessage(
	playerNames: Vector[String],
	mode: ViewMode,
	options: TableOptions = TableOptions()
) derives Reader

case class SelfData(
	username: String
) derives Reader

case class PlayerNotes(username: String = "", notes: Vector[String]) derives Reader

case class GameStateMessage(
	tableId: Int,
	events: Seq[Action],
	notes: Vector[PlayerNotes]
)

object GameStateMessage:
	def fromJSON(json: ujson.Value): GameStateMessage =
		GameStateMessage(
			json("tableId").num.toInt,
			json("events").arr.flatMap(Action.fromJSON).toSeq,
			upickle.read[Vector[PlayerNotes]](json("notes"))
		)

case class BotConfig(
	leaveReplayIfOnlyBots: Boolean = true,
	botNamePrefixes: List[String] = Nil
):
	def isBotName(name: String): Boolean =
		botNamePrefixes.exists(name.startsWith)

object BotConfig:
	def fromEnv(env: Map[String, String]): BotConfig = BotConfig(
		leaveReplayIfOnlyBots = env.getOrElse("HANABI_LEAVE_REPLAY_IF_ONLY_BOTS", "1") == "1",
		botNamePrefixes = env.getOrElse("HANABI_BOT_NAME_PREFIXES", "")
			.split(",").map(_.trim).filter(_.nonEmpty).toList
	)

class BotClient(queue: Queue[IO, String], gameRef: Ref[IO, Option[Game]], config: BotConfig = BotConfig())(using runtime: IORuntime):
	private var info: Option[SelfData] = None
	private var tableID: Option[Int] = None
	private var gameStarted = false
	private var tables: Map[Int, Table] = Map()
	private var pregamePlayers: Map[Int, Vector[Int]] = Map()
	private var nextRequestId: Int = 1
	private var pendingInitLoads: Map[Int, Int] = Map()
	private var convention: Convention = Convention.Reactor
	private var fastMode: Boolean = false
	private var leavePregameIfOnlyBots: Boolean = true
	private var lastRequester: Option[String] = None

	private def newGameFromSettings(tID: Int, state: State, convention: Convention): Game =
		convention match
			case Convention.Reactor => Reactor(tID, state, inProgress = true)
			case Convention.RefSieve => RefSieve(tID, state, inProgress = true)
			case Convention.HGroup(level) => HGroup(tID, state, inProgress = true, level = level)

	private def writeInfoNote(tableId: Int, notes: Vector[String], game: Game, convention: Convention): (IO[Unit], Game) =
		def addInfoNote(game: Game, note: String): Game =
			game match
				case r: Reactor => r.copy(notes = r.notes + (0 -> Note(0, note, note)))
				case r: RefSieve => r.copy(notes = r.notes + (0 -> Note(0, note, note)))
				case h: HGroup => h.copy(notes = h.notes + (0 -> Note(0, note, note)))

		val noteStr = infoNote(convention)
		if !game.notes.contains(0) && game.inProgress && notes.headOption.getOrElse("").isEmpty then
			val io = sendCmd("note", ujson.Obj("tableId" -> tableId, "order" -> 0, "text" -> noteStr))
			(io, addInfoNote(game, noteStr))
		else
			(IO.unit, game)

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
						val player = from.fold(Some(game.common)): name =>
							state.names.indexOf(name) match
								case -1 =>
									println(s"Player $from not found.")
									None
								case index =>
									Some(game.players(index))

						player.fold(IO.unit): p =>
							val output = List(
								s"viewing from ${p.name}",
								s"===================="
							).concat:
								state.hands(i).flatMap: order =>
									val meta = game.meta(order)
									val flags = List(
										Option.when(meta.focused)("focused"),
										Option.when(meta.hidden)("hidden"),
										Option.when(meta.trash)("trash"),
										Option.when(meta.urgent)("urgent"),
										Option.when(p.thoughts(order).reset)("reset")
									).flatten

									List(
										Some(s"$order: ${state.logId(order)} ${meta.status}"),
										Some(s"inferred: [${p.strInfs(state, order)}]"),
										p.thoughts(order).infoLock.mapA(info => s"info lock: [${info.fmt(state)}]"),
										Some(s"possible: [${p.strPoss(state, order)}]"),
										Some(s"reasoning: ${meta.reasoning}"),
										Option.when(!flags.isEmpty)(s"flags: $flags"),
										Some("====================")
									).flatten

							IO.println(output.mkString("\n"))
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
				case ConsoleCmd.State =>
					IO.println(s"clue tokens: ${state.clueTokens}")

	private def handleWarning(notice: ujson.Value): IO[Unit] =
		val msg = notice("message").str

		lastRequester.fold(IO.unit): who =>
			sendPM(who, msg) *>
			IO { lastRequester = None }
		*>
		IO { println(s"warn: $notice") }

	def handleMsg(raw: String): IO[Unit] =
		val frame = ujson.read(raw)
		if frame.obj.contains("reply") then
			val id = frame("reply").num.toInt
			IO:
				val tID = pendingInitLoads.get(id)
				pendingInitLoads = pendingInitLoads - id
				tID
			.flatMap: maybeTableId =>
				frame.obj.get("error") match
					case Some(err) =>
						val ignored = err.obj.get("code").flatMap(_.strOpt).contains("commandIgnored")
						IO.whenA(!ignored)(handleWarning(err))
					case None =>
						maybeTableId.fold(IO.unit): tID =>
							handleInit(tID, upickle.read[InitMessage](frame("data")))
		else
			val data = frame("data")
			frame("event").str match
				case "chat" =>
					handleChat(upickle.read[ChatMessage](data))

				case "chatList" =>
					val chatList = upickle.read[ChatList](data)
					val helpMsg = "PM /help for help."
					val sendHelp = tableID.contains(chatList.table.tableId) &&
						!chatList.messages.exists(_.text == helpMsg)

					IO.whenA(sendHelp):
						sendChat(helpMsg)

				case "gameEvent" =>
					Action.fromJSON(data("event")).fold(IO.unit)(handleAction)

				case "gameState" =>
					val msg = GameStateMessage.fromJSON(data)

					gameRef.get.flatMap:
						case None => IO.unit
						case Some(g) =>
							// Restore bot level from the note on the first card after rejoin
							val notes = msg.notes.find(n => info.exists(_.username == n.username))
								.orElse(msg.notes.headOption)
								.fold(Vector.empty)(_.notes)
							val maybeNewConvention = notes.headOption.flatMap(parseConventionFromNote)
							val game: Game = maybeNewConvention.flatMap: c =>
									Option.when(c != convention):
										Log.info(s"restored settings from first card note: $c")
										convention = c
										newGameFromSettings(msg.tableId, g.base._1, c)
								.getOrElse(g)
							val (noteIO, gameWithNote) = writeInfoNote(msg.tableId, notes, game, convention)
							val setCatchup = gameWithNote match
								case r: Reactor => gameRef.set(Some(r.copy(catchup = true)))
								case r: RefSieve => gameRef.set(Some(r.copy(catchup = true)))
								case h: HGroup => gameRef.set(Some(h.copy(catchup = true)))

							setCatchup *>
							noteIO *>
							msg.events.dropRight(1).traverse_(handleAction) *>
							gameRef.update:
								case Some(g2: Reactor) => Some(g2.copy(catchup = false))
								case Some(g2: RefSieve) => Some(g2.copy(catchup = false))
								case Some(g2: HGroup) => Some(g2.copy(catchup = false))
								case other => other
							*>
							sendCmd("gameLoaded", ujson.Obj("tableId" -> msg.tableId)) *>
							IO.sleep(1000.millis).whenA(!fastMode) *>
							msg.events.lastOption.fold(IO.unit)(handleAction)

				case "pregame" =>
					val p = upickle.read[Pregame](data)
					IO { pregamePlayers = pregamePlayers + (p.tableId -> p.players.map(_.userId)) }

				case "tableJoined" =>
					val joined = upickle.read[TableJoined](data)
					joined.stage match
						case "pregame" =>
							IO:
								tableID = Some(joined.tableId)
								gameStarted = false
								lastRequester = None
								leavePregameIfOnlyBots = true		// reset to true on each table join
							.flatMap: _ =>
								tables.get(joined.tableId).fold(IO.unit): table =>
									checkSupportedSettings(table)
						case "justStarted" | "playing" =>
							IO:
								tableID = Some(joined.tableId)
								gameStarted = true
								lastRequester = None
							*>
							sendCmd("gameInitLoad", ujson.Obj("tableId" -> joined.tableId))
						case _ =>
							IO:
								tableID = Some(joined.tableId)
								gameStarted = false
								lastRequester = None

				case "gameFinished" =>
					IO { gameStarted = false }

				case "tableLeft" =>
					IO:
						tableID = None
						gameStarted = false

				case "table" =>
					val table = upickle.read[Table](data)

					IO.whenA(tableID.contains(table.tableId) && !gameStarted):
						if config.leaveReplayIfOnlyBots &&
							table.sharedReplay &&
							table.spectators.forall(s => config.isBotName(s.username)) then
							Log.info("Leaving game. Only bots left spectating.")
							leaveRoom()
						else if leavePregameIfOnlyBots &&
							!table.running &&
							table.playerNames.forall(config.isBotName) then
							Log.info("Leaving game. Only bots left in lobby.")
							leaveRoom()
						else
							checkSupportedSettings(table)
					*>
					IO { tables = tables + (table.tableId -> table) }

				case "tableRemoved" =>
					val id = data("tableId").num.toInt
					IO:
						tables = tables - id
						pregamePlayers = pregamePlayers - id

				case "tableList" =>
					val list = upickle.read[TableList](data).tables
					IO {
						tables = tables ++ list.map(t => t.tableId -> t)
						tables
					}.flatMap: updatedTables =>
						info.fold(IO.unit): self =>
							val myTable = updatedTables.values
								.find(t => t.playerNames.contains(self.username) && t.running)

							myTable.fold(IO.unit): t =>
								Log.info(s"automatically reattending table with id ${t.tableId}")
								sendCmd("tableReattend", ujson.Obj("tableId" -> t.tableId))

				case "warning" =>
					handleWarning(data)

				case "error" =>
					IO { println(s"error: $data") }

				case "welcome" =>
					IO { info = Some(upickle.read[SelfData](data)) }

				case _ => IO.unit

	def leaveRoom(): IO[Unit] =
		val cmd = if gameStarted then "tableUnattend" else "tableLeave"

		IO.whenA(tableID.isDefined):
			sendCmd(cmd, ujson.Obj("tableId" -> tableID.get))
		*>
		IO:
			tableID = None
			gameStarted = false
		*>
		gameRef.set(None)

	def handleInit(tID: Int, data: InitMessage) =
		val InitMessage(playerNames, ViewMode(ourPlayerIndex), options) = data
		val variant = Variant.getVariant(options.variantId)
		val state = State(playerNames, ourPlayerIndex, variant, options)
		val game = newGameFromSettings(tID, state, convention)

		IO { tableID = Some(tID) } *>
		gameRef.set(Some(game)) *>
		sendCmd("gameStateLoad", ujson.Obj("tableId" -> tID))

	def handleChat(data: ChatMessage): IO[Unit] =
		val ChatMessage(msg, msgTableId, kind) = data
		val who = data.username
		val withinRoom = kind.`type` == "public" && msgTableId != -1

		if withinRoom then
			if msg.startsWith("/setall") then
				assignSettings(data, false)
			else if msg.startsWith("/leaveall") then
				leaveRoom()
			else
				IO.unit

		else if kind.`type` != "private" || !info.exists(_.username == data.recipient) then
			IO.unit

		else if msg.startsWith("/join") then
			val table = tables.values.filter: t =>
				(t.playerNames.contains(who) && !t.sharedReplay) ||
				t.spectators.exists(_.username == who)
			.maxByOption(_.tableId)

			table match
				case Some(t) if t.passwordProtected =>
					msg.split(" ").lift(1) match
						case Some(password) =>
							IO { lastRequester = Some(who) } *>
							sendCmd("tableJoin", ujson.Obj("tableId" -> t.tableId, "password" -> password))
						case None =>
							sendPM(who, "Room is password protected, please provide a password.")
				case Some(t) => sendCmd("tableJoin", ujson.Obj("tableId" -> t.tableId))
				case None => sendPM(who, "Could not join, as you are not in a room.")

		else if msg.startsWith("/rejoin") then
			gameRef.get.flatMap:
				case Some(_) => sendPM(who, "Could not rejoin, as the bot is already in a game.")
				case None =>
					val table = tables.values
						.filter(_.playerNames.contains(info.get.username))
						.maxByOption(_.tableId)

					table match
						case Some(t) => sendCmd("tableReattend", ujson.Obj("tableId" -> t.tableId))
						case None => sendPM(who, "Could not rejoin, as the bot is not a player in any open room.")

		else if msg.startsWith("/start") then
			tableID match
				case Some(id) =>
					IO { lastRequester = Some(who) } *>
					sendCmd("tableStart", ujson.Obj(
						"tableId" -> id,
						"intendedPlayerIds" -> pregamePlayers.getOrElse(id, Vector.empty)
					))
				case None => sendPM(who, "Cannot start table, as the bot is not in a table.")

		else if msg.startsWith("/leave") then
			leaveRoom()

		else if msg.startsWith("/terminate") then
			gameRef.get.flatMap:
				case Some(game) if game.inProgress =>
					if game.state.names.contains(data.username) then
						sendCmd("tableTerminate", ujson.Obj("tableId" -> game.tableID))
					else
						sendPM(who, "Could not terminate, as you are not one of the players in the game.")
				case _ => sendPM(who, "Could not terminate, as the bot is not in a game.")

		else if msg.startsWith("/settings") then
			assignSettings(data, true)

		else if msg.startsWith("/version") then
			sendPM(who, BOT_VERSION)

		else if msg.startsWith("/help") then
			sendHelp(data.username)

		else if msg.startsWith("/analyze") then
			val pattern = """/analyze (\d+) (\w+) ?(\d+)?""".r

			msg match
				case pattern(id, conv, level) if conv != null && Convention.from(conv).isRight =>
					val args = List(
						Some(s"id=$id"),
						Some(s"convention=$conv"),
						Option.when(level != null)(s"level=$level")
					).flatten

					val analyzeIO = IO.blocking(fetchAnalyzeGame(args))
						.flatMap: comments =>
							comments.toList.traverse_(sendPM(who, _)) *>
							sendPM(who, "Analysis completed.")
						.handleErrorWith: reason =>
							sendPM(who, s"Failed to analyze: $reason.")

						sendPM(who, "Analyzing... (may take up to 60s)") *> analyzeIO.start.void
				case _ =>
					sendPM(who, s"Couldn't parse command. Format is /analyze <replayId> <convention>.")

		else if msg.startsWith("/fastmode") then
			sendPM(data.username, s"Fast mode is now ${if fastMode then "off" else "on"}.") *>
			IO { fastMode = !fastMode }

		else if msg.startsWith("/doc") then
			convention match
				case Convention.Reactor =>
					sendPM(data.username, "Reactor 1.0: https://hanabi.wiki/conventions/reactor-1")
				case Convention.RefSieve =>
					sendPM(data.username, "General RS: https://hackmd.io/Ui6LXAK3TdC7AKSDcN20PQ") *>
					sendPM(data.username, "2P RS: https://willflame14.github.io/rs-docs/intro")
				case Convention.HGroup(level) =>
					sendPM(data.username, "H-Group: https://hanabi.github.io/reference")

		else if msg.startsWith("/bugreport") then
			sendPM(data.username, s"The bots aren't perfect! Please report bugs on the issue tracker: https://github.com/will-hanabi-bot/scala-bot/issues")

		else if msg.startsWith("/stay") then
			tableID match
				case Some(_) =>
					sendPM(data.username, s"This bot will now remain in the lobby even if there are only bots left.") *>
					IO { leavePregameIfOnlyBots = false }
				case None =>
					sendPM(who, "Cannot set /stay, as the bot is not in a table.")

		else
			IO.unit

	def handleAction(action: Action): IO[Unit] =
		gameRef.get.flatMap:
			case None => IO.unit //throw new IllegalStateException("Game not initialized")
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

					val x = newGame match
						case r: Reactor  => r.copy(queuedCmds = Nil)
						case r: RefSieve => r.copy(queuedCmds = Nil)
						case h: HGroup   => h.copy(queuedCmds = Nil)

					gameRef.set(Some(x)) *>
					queuedCmds.traverse_((cmd, args) => sendCmd(cmd, ujson.read(args))) *>
					IO.whenA(perform):
						val suggestedActionIO = newGame match
							case r: Reactor  => r.takeAction
							case r: RefSieve => r.takeAction
							case h: HGroup   => h.takeAction

						val computeAndSend = IO.sleep(2.seconds).whenA(!fastMode).start.flatMap: sleepFiber =>
							suggestedActionIO.flatMap: action =>
								Log.highlight(Console.BLUE, s"Suggested action: ${action.fmt(newGame, accordingTo = Some(newGame.me))}")
								val arg = action.json(tableID.get)

								if newGame.inProgress then
									// Race against 2 second timer, then send.
									sleepFiber.join *> sendCmd("playerAction", arg)
								else
									sleepFiber.cancel

						computeAndSend.start.void

	def assignSettings(data: ChatMessage, pm: Boolean) =
		val reply =
			if pm then
				(msg: String) => sendPM(data.username, msg)
			else
				(msg: String) => sendChat(msg)

		data.text.split(" ", 2) match
			case Array(_) =>
				reply(s"Currently playing with ${convention} conventions.${if fastMode then " [fast mode]" else ""}")

			case Array(_, convStr) =>
				Convention.from(convStr) match
					case Left(msg) => reply(msg)
					case Right(c) =>
						convention = c

						reply(s"Currently playing with ${convention} conventions.${if fastMode then " [fast mode]" else ""}") *> {
							c match
								case Convention.HGroup(level) if level >= Level.Context =>
									reply("Above level 11, the bot plays with minimal context: only stale 1s after early game and explicit focus inversion.")
								case _ => IO.unit
						}

	def sendHelp(who: String) =
		sendPM(who, "Commands: /analyze, /bugreport, /doc, /fastmode, /help, /join, /leave, /settings, /start, /stay, /terminate, /version. See https://github.com/will-hanabi-bot/scala-bot#supported-commands for more info.")

	def checkSupportedSettings(table: Table): IO[Unit] =
		val options = table.settings.options
		val unsupportedOptions = List(
			Option.when(options.emptyClues)("emptyClues"),
			Option.when(options.detrimentalCharacters)("detrimentalChars"),
			Option.when(options.oneExtraCard)("oneExtraCard"),
			Option.when(options.oneLessCard)("oneLessCard"),
		).flatten

		val variantName = Variant.getVariant(options.variantId).name
		val unsupportedVarPattern = """Blind|Mute|Ambiguous|Dual-Color|Synesthesia|Mix|Funnels|Chimneys|Matryoshka|Alternating|Odds|Cow|Duck|Reversed|Throw|Up or Down|Sudoku""".r.unanchored
		val unsupportedVariants = unsupportedVarPattern.matches(variantName)

		IO.whenA(unsupportedOptions.nonEmpty):
			sendChat(s"This bot doesn't support the table ${if unsupportedOptions.length == 1 then "option" else "options"} ${unsupportedOptions.mkString(", ")} and may crash or behave nonsensically.")
		*>
		IO.whenA(unsupportedVariants):
			sendChat(s"This bot doesn't support the variant $variantName and may crash or behave nonsensically.")

	private def offerRequest(cmd: String, data: ujson.Value, log: Boolean): IO[Unit] =
		IO:
			val id = nextRequestId
			nextRequestId += 1
			if cmd == "gameInitLoad" then
				pendingInitLoads = pendingInitLoads + (id -> data("tableId").num.toInt)
			if log then
				println(s"${Console.RED}Sending command: $cmd $data${Console.RESET}")
			ujson.write(ujson.Obj("id" -> id, "request" -> cmd, "data" -> data))
		.flatMap(queue.offer)

	def sendPM(recipient: String, msg: String) =
		offerRequest("chatPm", ujson.Obj(
			"text" -> msg,
			"recipient" -> recipient
		), log = false)

	def sendChat(msg: String) =
		offerRequest("chat", ujson.Obj(
			"text" -> msg,
			"tableId" -> tableID.get
		), log = false)

	def sendCmd(cmd: String, data: ujson.Value) =
		offerRequest(cmd, data, log = true)
