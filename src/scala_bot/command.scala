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

enum ChatAuthor:
	case Server
	case User(username: String)

object ChatAuthor:
	given Reader[ChatAuthor] = reader[ujson.Value].map: json =>
		val obj = json.obj

		obj("type").str match
			case "server" | "discord" => ChatAuthor.Server
			case "user"   => ChatAuthor.User(obj("username").str)

enum ChatKind:
	case Public(author: ChatAuthor)
	case Private(author: ChatAuthor, recipient: String)

	def getAuthor = this match
		case Public(author) => author
		case Private(author, _) => author

object ChatKind:
	given Reader[ChatKind] = reader[ujson.Value].map: json =>
		val obj = json.obj
		val author = upickle.read[ChatAuthor](obj("author"))

		obj("type").str match
			case "public" | "discord"  => ChatKind.Public(author)
			case "private" => ChatKind.Private(author, obj("recipient").str)

case class ChatMessage(
	text: String,
	kind: ChatKind,
	tableId: Option[Int] = None
) derives Reader:
	def getUsername = kind.getAuthor match
		case ChatAuthor.Server => throw new Error("tried to get username of server!")
		case ChatAuthor.User(username) => username

case class ChatTable(tableId: Int) derives Reader

case class ChatList(
	messages: Vector[ChatMessage],
	table: Option[ChatTable] = None
) derives Reader

case class GameActionMessage(
	tableID: Int,
	action: Action
)

object GameActionMessage:
	def fromJSON(json: ujson.Value): Option[GameActionMessage] =
		Action.fromJSON(json.obj("action")).map:
			GameActionMessage(json.obj("tableId").num.toInt, _)

case class Spectator(
	username: String,
	shadowingPlayerIndex: Int = -1
) derives Reader

enum TablePhase:
	case Pregame
	case Playing
	case SharedReplay(gameId: Int)

object TablePhase:
	given Reader[TablePhase] = reader[ujson.Value].map: json =>
		val obj = json.obj

		obj("type").str match
			case "pregame" => TablePhase.Pregame
			case "playing" => TablePhase.Playing
			case "sharedReplay" =>
				val gameId = obj("gameId").num.toInt
				TablePhase.SharedReplay(gameId)

case class Table(
	id: Int,
	passwordProtected: Boolean,
	phase: TablePhase,
	settings: TableSettings,
	playerNames: Vector[String],
	spectators: List[Spectator]
)

object Table:
	given Reader[Table] = reader[ujson.Value].map: json =>
		val obj = json.obj

		Table(
			obj("tableId").num.toInt,
			obj("passwordProtected").bool,
			upickle.read[TablePhase](obj("phase")),
			upickle.read[TableSettings](obj("settings")),
			obj("playerNames").arr.toVector.map(_.str),
			obj("spectators").arr.toList.map(upickle.read[Spectator](_))
		)

case class TableSettings(
	name: String,
	maxPlayers: Int,
	options: TableOptions
) derives Reader

case class TableOptions(
	variantId: Int = 0,
	deckPlays: Boolean = false,
	emptyClues: Boolean = false,
	oneExtraCard: Boolean = false,
	oneLessCard: Boolean = false,
	speedrun: Boolean = false
) derives Reader:
	def variantName = Variant.getVariant(variantId).name

enum TableJoinedMessage:
	case Pregame
	case JustStarted
	case Playing

case class InitMode(ourPlayerIndex: Int) derives Reader

case class InitMessage(
	mode: InitMode,
	playerNames: Vector[String],
	seed: String,
	options: TableOptions
) derives Reader

case class SelfData(username: String) derives Reader

case class PlayerNotes(
	username: String,
	notes: Vector[String]
) derives Reader

case class GameStateMessage(
	tableID: Int,
	events: Vector[Action],
	notes: Vector[PlayerNotes]
)

object GameStateMessage:
	given Reader[GameStateMessage] = reader[ujson.Value].map: json =>
		val obj = json.obj

		GameStateMessage(
			obj("tableId").num.toInt,
			json("events").arr.flatMap(Action.fromJSON).toVector,
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
	private var nextRequestId: Int = 1
	private var lastInitIds: Option[(Int, Int)] = None
	private var gameStarted = false
	private var tables: Map[Int, Table] = Map()
	private var convention: Convention = Convention.Reactor
	private var fastMode: Boolean = false
	private var leavePregameIfOnlyBots: Boolean = true
	private var lastRequester: Option[String] = None

	private def newGameFromSettings(tID: Int, state: State, convention: Convention): Game =
		convention match
			case Convention.Reactor => Reactor(tID, state, inProgress = true)
			case Convention.RefSieve => RefSieve(tID, state, inProgress = true)
			case Convention.HGroup(level) => HGroup(tID, state, inProgress = true, level = level)

	private def writeInfoNote(tID: Int, notes: Vector[String], game: Game, convention: Convention): (IO[Unit], Game) =
		def addInfoNote(game: Game, note: String): Game =
			game match
				case r: Reactor => r.copy(notes = r.notes + (0 -> Note(0, note, note)))
				case r: RefSieve => r.copy(notes = r.notes + (0 -> Note(0, note, note)))
				case h: HGroup => h.copy(notes = h.notes + (0 -> Note(0, note, note)))

		val noteStr = infoNote(convention)
		if !game.notes.contains(0) && game.inProgress && notes.headOption.getOrElse("").isEmpty then
			val io = sendCmd("note", ujson.Obj("tableId" -> tID, "order" -> 0, "text" -> noteStr))
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

	def handleMsg(raw: String): IO[Unit] =
		val frame = ujson.read(raw)

		frame.obj.get("reply").map(_.num.toInt) match
			case Some(id) =>
				frame.obj.get("error") match
					case Some(err) => IO.println(s"reply error: ${err("message")}")
					case None =>
						lastInitIds match
							case Some(reqID, tableID) if reqID == id =>
								IO { lastInitIds = None } *>
								handleInit(tableID, upickle.read[InitMessage](frame("data")))
							case _ => IO.unit

			case None =>
				handleEvent(frame("event").str, frame("data"))

	def handleEvent(event: String, data: ujson.Value) =
		event match
			case "chat" =>
				handleChat(upickle.read[ChatMessage](data))

			case "chatList" =>
				val ChatList(messages, table) = upickle.read[ChatList](data)
				val helpMsg = "PM /help for help."

				val sendHelp = table.exists(t => tableID.contains(t.tableId)) &&
					!messages.exists(_.text == helpMsg)

				IO.whenA(sendHelp):
					sendChat(helpMsg)

			case "gameEvent" =>
				data("events").arr.toList.traverse_ : event =>
					Action.fromJSON(event) match
						case None =>
							IO.println(s"${Console.RED}failed to read gameEvent $event${Console.RESET}")
						case Some(action) =>
							handleAction(action)

			// Received at the beginning of the game, containing a list of all actions that have happened so far.
			case "gameState" =>
				val GameStateMessage(tID, events, allNotes) = upickle.read[GameStateMessage](data)

				gameRef.get.flatMap:
					case None => IO.unit
					case Some(g) =>
						// Restore bot level from the note on the first card after rejoin
						val notes = allNotes.find(n => info.exists(_.username == n.username))
							.orElse(allNotes.headOption)
							.fold(Vector.empty)(_.notes)
						val maybeNewConvention = notes.headOption.flatMap(parseConventionFromNote)
						val game: Game = maybeNewConvention.flatMap: c =>
								Option.when(c != convention):
									Log.info(s"restored settings from first card note: $c")
									convention = c
									newGameFromSettings(tID, g.base._1, c)
							.getOrElse(g)

						val (noteIO, gameWithNote) = writeInfoNote(tID, notes, game, convention)
						val setCatchup = gameWithNote match
							case r: Reactor => gameRef.set(Some(r.copy(catchup = true)))
							case r: RefSieve => gameRef.set(Some(r.copy(catchup = true)))
							case h: HGroup => gameRef.set(Some(h.copy(catchup = true)))

						setCatchup *>
						noteIO *>
						// Skip last action so we can react appropriately
						events.dropRight(1).traverse_(handleAction) *>
						gameRef.update:
							case Some(g2: Reactor) => Some(g2.copy(catchup = false))
							case Some(g2: RefSieve) => Some(g2.copy(catchup = false))
							case Some(g2: HGroup) => Some(g2.copy(catchup = false))
							case other => other
						*>
						sendCmd("gameLoaded", ujson.Obj("tableId" -> tID)) *>
						IO.sleep(1000.millis) *>
						events.lastOption.fold(IO.unit)(handleAction)

			case "tableJoined" =>
				val id = data("tableId").num.toInt

				data("stage").str match
					case "pregame" =>
						IO:
							tableID = Some(id)
							gameStarted = false
							lastRequester = None
							leavePregameIfOnlyBots = true		// reset to true on each table join
						.flatMap: _ =>
							tables.get(id).fold(IO.unit): table =>
								checkSupportedSettings(table)
					case "justStarted" | "playing" =>
						IO:
							tableID = Some(id)
							gameStarted = true
							lastRequester = None
							lastInitIds = Some((nextRequestId, id))
						*>
						sendCmd("gameInitLoad", ujson.Obj("tableId" -> id))
					case _ =>
						IO:
							tableID = Some(id)
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

				IO.whenA(tableID.contains(table.id) && !gameStarted):
					if config.leaveReplayIfOnlyBots &&
						table.phase == TablePhase.SharedReplay &&
						table.spectators.forall(s => config.isBotName(s.username)) then
						Log.info("Leaving game. Only bots left spectating.")
						leaveRoom()
					else if leavePregameIfOnlyBots &&
						table.phase != TablePhase.Playing &&
						table.playerNames.forall(config.isBotName) then
						Log.info("Leaving game. Only bots left in lobby.")
						leaveRoom()
					else
						checkSupportedSettings(table)
				*>
				IO { tables = tables + (table.id -> table) }

			case "tableRemoved" =>
				val id = data("tableId").num.toInt
				IO { tables = tables - id }

			case "tableList" =>
				val list = upickle.read[List[Table]](data("tables"))
				IO {
					tables = tables ++ list.map(t => t.id -> t)
					tables
				}.flatMap: updatedTables =>
					info.fold(IO.unit): self =>
						val myTable = updatedTables.values
							.find(t => t.playerNames.contains(self.username) && t.phase == TablePhase.Playing)

						myTable.fold(IO.unit): t =>
							Log.info(s"automatically reattending table with id ${t.id}")
							sendCmd("tableReattend", ujson.Obj("tableId" -> t.id))

			case "warning" =>
				val msg = data("message").str

				lastRequester.fold(IO.unit): who =>
					sendPM(who, msg) *>
					IO { lastRequester = None }
				*>
				IO { println(s"warn: $msg") }

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
		val InitMessage(InitMode(ourPlayerIndex), playerNames, seed, options) = data
		val variant = Variant.getVariant(options.variantId)
		val state = State(playerNames, ourPlayerIndex, variant, options)
		val game = newGameFromSettings(tID, state, convention)

		IO { tableID = Some(tID) } *>
		gameRef.set(Some(game)) *>
		sendCmd("gameStateLoad", ujson.Obj("tableId" -> tID))

	def handleChat(data: ChatMessage): IO[Unit] =
		val ChatMessage(text, kind, tableID) = data
		kind match
			case ChatKind.Public(author) =>
				IO.whenA(tableID.isDefined):
					if text.startsWith("/setall") then
						assignSettings(text)
					else if text.startsWith("/leaveall") then
						leaveRoom()
					else
						IO.unit
			case ChatKind.Private(ChatAuthor.User(username), recipient) if recipient == info.get.username =>
				handlePM(username, text)
			case _ => IO.unit

	def handlePM(who: String, msg: String) =
		if msg.startsWith("/join") then
			val table = tables.values.filter: t =>
				(t.playerNames.contains(who) && t.phase != TablePhase.SharedReplay) ||
				t.spectators.exists(_.username == who)
			.maxByOption(_.id)

			table match
				case Some(t) if t.passwordProtected =>
					msg.split(" ").lift(1) match
						case Some(password) =>
							IO { lastRequester = Some(who) } *>
							sendCmd("tableJoin", ujson.Obj("tableId" -> t.id, "password" -> password))
						case None =>
							sendPM(who, "Room is password protected, please provide a password.")
				case Some(t) => sendCmd("tableJoin", ujson.Obj("tableId" -> t.id))
				case None => sendPM(who, "Could not join, as you are not in a room.")

		else if msg.startsWith("/rejoin") then
			gameRef.get.flatMap:
				case Some(_) => sendPM(who, "Could not rejoin, as the bot is already in a game.")
				case None =>
					val table = tables.values
						.filter(_.playerNames.contains(info.get.username))
						.maxByOption(_.id)

					table match
						case Some(t) => sendCmd("tableReattend", ujson.Obj("tableId" -> t.id))
						case None => sendPM(who, "Could not rejoin, as the bot is not a player in any open room.")

		else if msg.startsWith("/start") then
			tableID match
				case Some(id) =>
					IO { lastRequester = Some(who) } *>
					sendCmd("tableStart", ujson.Obj("tableId" -> id))
				case None => sendPM(who, "Cannot start table, as the bot is not in a table.")

		else if msg.startsWith("/leave") then
			leaveRoom()

		else if msg.startsWith("/terminate") then
			gameRef.get.flatMap:
				case Some(game) if game.inProgress =>
					if game.state.names.contains(who) then
						sendCmd("tableTerminate", ujson.Obj("tableId" -> game.tableID))
					else
						sendPM(who, "Could not terminate, as you are not one of the players in the game.")
				case _ => sendPM(who, "Could not terminate, as the bot is not in a game.")

		else if msg.startsWith("/settings") then
			assignSettings(msg, Some(who))

		else if msg.startsWith("/version") then
			sendPM(who, BOT_VERSION)

		else if msg.startsWith("/help") then
			sendHelp(who)

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
			sendPM(who, s"Fast mode is now ${if fastMode then "off" else "on"}.") *>
			IO { fastMode = !fastMode }

		else if msg.startsWith("/doc") then
			convention match
				case Convention.Reactor =>
					sendPM(who, "Reactor 1.0: https://hanabi.wiki/conventions/reactor-1")
				case Convention.RefSieve =>
					sendPM(who, "General RS: https://hackmd.io/Ui6LXAK3TdC7AKSDcN20PQ") *>
					sendPM(who, "2P RS: https://willflame14.github.io/rs-docs/intro")
				case Convention.HGroup(level) =>
					sendPM(who, "H-Group: https://hanabi.github.io/reference")

		else if msg.startsWith("/bugreport") then
			sendPM(who, s"The bots aren't perfect! Please report bugs on the issue tracker: https://github.com/will-hanabi-bot/scala-bot/issues")

		else if msg.startsWith("/stay") then
			tableID match
				case Some(_) =>
					sendPM(who, s"This bot will now remain in the lobby even if there are only bots left.") *>
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
					queuedCmds.traverse_(sendCmd(_, _)) *>
					IO.whenA(perform):
						val suggestedActionIO = newGame match
							case r: Reactor  => r.takeAction
							case r: RefSieve => r.takeAction
							case h: HGroup   => h.takeAction

						val computeAndSend = IO.sleep(2.seconds).whenA(!fastMode).start.flatMap: sleepFiber =>
							suggestedActionIO.flatMap: action =>
								Log.highlight(Console.BLUE, s"Suggested action: ${action.fmt(newGame, accordingTo = Some(newGame.me))}")
								val arg = ujson.Obj("tableId" -> tableID.get, "action" -> action.json)

								if newGame.inProgress then
									// Race against 2 second timer, then send.
									sleepFiber.join *> sendCmd("playerAction", arg)
								else
									sleepFiber.cancel

						computeAndSend.start.void

	def assignSettings(text: String, from: Option[String] = None) =
		val reply = from match
			case Some(username) =>
				(msg: String) => sendPM(username, msg)
			case None =>
				(msg: String) => sendChat(msg)

		text.split(" ", 2) match
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
			Option.when(options.oneExtraCard)("oneExtraCard"),
			Option.when(options.oneLessCard)("oneLessCard"),
		).flatten

		val unsupportedVarPattern = """Blind|Mute|Ambiguous|Dual-Color|Synesthesia|Mix|Funnels|Chimneys|Matryoshka|Alternating|Odds|Cow|Duck|Reversed|Throw|Up or Down|Sudoku""".r.unanchored
		val unsupportedVariants = unsupportedVarPattern.matches(options.variantName)

		IO.whenA(unsupportedOptions.nonEmpty):
			sendChat(s"This bot doesn't support the table ${if unsupportedOptions.length == 1 then "option" else "options"} ${unsupportedOptions.mkString(", ")} and may crash or behave nonsensically.")
		*>
		IO.whenA(unsupportedVariants):
			sendChat(s"This bot doesn't support the variant ${options.variantName} and may crash or behave nonsensically.")

	def sendPM(recipient: String, msg: String) =
		sendCmd("chatPm", ujson.Obj("text" -> msg, "recipient" -> recipient))

	def sendChat(msg: String) =
		sendCmd("chat", ujson.Obj("text" -> msg, "tableId" -> tableID.get))

	def sendCmd(cmd: String, data: ujson.Value) =
		println(s"${Console.RED}Sending command: $cmd $data${Console.RESET}")
		IO:
			val requestId = nextRequestId
			nextRequestId += 1
			ujson.write(ujson.Obj("id" -> requestId, "request" -> cmd, "data" -> data))
		.flatMap(queue.offer)
