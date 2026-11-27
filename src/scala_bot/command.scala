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
	case SharedReplay(gameId: Option[Int])

	def isSharedReplay = this match
		case SharedReplay(_) => true
		case _ => false

object TablePhase:
	given Reader[TablePhase] = reader[ujson.Value].map: json =>
		val obj = json.obj

		obj("type").str match
			case "pregame" => TablePhase.Pregame
			case "playing" => TablePhase.Playing
			case "sharedReplay" =>
				val gameId = obj.get("gameId").map(_.num.toInt)
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
	seed: String = "",
	options: TableOptions
) derives Reader

case class SelfData(username: String) derives Reader

case class LobbyUser(
	username: String,
	bot: Boolean
) derives Reader

case class WelcomeMessage(
	username: String,
	users: List[LobbyUser],
	tables: List[Table],
	playingAtTables: List[Int]
) derives Reader

case class PregamePlayer(userId: Int) derives Reader

case class PregameMessage(
	tableId: Int,
	players: Vector[PregamePlayer]
) derives Reader

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
			obj("events").arr.flatMap(Action.fromJSON).toVector,
			upickle.read[Vector[PlayerNotes]](obj("notes"))
		)

case class SessionState(
	info: Option[SelfData] = None,
	tableID: Option[Int] = None,
	tables: Map[Int, Table] = Map(),
	pregamePlayerIds: Map[Int, Vector[Int]] = Map(),
	bots: Set[String] = Set(),
	gameStarted: Boolean = false,
	lastRequester: Option[String] = None,
	lastInitIds: Option[(Int, Int)] = None,
	nextRequestId: Int = 1
)

case class ConfigState(
	leaveReplayIfOnlyBots: Boolean = true,
	leavePregameIfOnlyBots: Boolean = true,
	fastMode: Boolean = false,
	convention: Convention = Convention.Reactor
)

case class BotClient(
	queue: Queue[IO, String],
	gameRef: Ref[IO, Option[Game]],
	sessionRef: Ref[IO, SessionState],
	configRef: Ref[IO, ConfigState]
)(using runtime: IORuntime):
	private def allocRequestID =
		sessionRef.modify: s =>
			(s.copy(nextRequestId = s.nextRequestId + 1), s.nextRequestId)

	private def newGameFromSettings(tID: Int, state: State, convention: Convention): Game =
		convention match
			case Convention.Reactor       => Reactor(tID, state, inProgress = true)
			case Convention.RefSieve      => RefSieve(tID, state, inProgress = true)
			case Convention.HGroup(level) => HGroup(tID, state, inProgress = true, level = level)

	private def writeInfoNote(tID: Int, notes: Vector[String], game: Game, convention: Convention): IO[Game] =
		val note = infoNote(convention)

		IO.whenA(!game.notes.contains(0) && game.inProgress && notes.headOption.getOrElse("").isEmpty):
			sendCmd("note", ujson.Obj("tableId" -> tID, "order" -> 0, "text" -> note))
		*>
		IO.pure:
			game match
				case r: Reactor  => r.copy(notes = r.notes + (0 -> Note(0, note, note)))
				case r: RefSieve => r.copy(notes = r.notes + (0 -> Note(0, note, note)))
				case h: HGroup   => h.copy(notes = h.notes + (0 -> Note(0, note, note)))

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
							case r: Reactor  => gameRef.set(Some(r.navigate(turn)))
							case r: RefSieve => gameRef.set(Some(r.navigate(turn)))
							case r: HGroup   => gameRef.set(Some(r.navigate(turn)))
							case _ => IO.unit
				case ConsoleCmd.State =>
					IO.println(s"clue tokens: ${state.clueTokens}")

	def handleMsg(raw: String): IO[Unit] =
		val frame = ujson.read(raw)

		frame.obj.get("reply").map(_.num.toInt) match
			case Some(id) =>
				frame.obj.get("error") match
					case Some(err) => handleWarning(err("message").str)
					case None =>
						sessionRef.get.flatMap: session =>
							session.lastInitIds match
								case Some(reqID, tableID) if reqID == id =>
									sessionRef.update(_.copy(lastInitIds = None)) *>
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

				sessionRef.get.flatMap: session =>
					val sendHelp = table.exists(t => session.tableID.contains(t.tableId)) &&
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
						sessionRef.get.flatMap: session =>
							// Restore bot level from the note on the first card after rejoin
							val notes = allNotes.find(n => session.info.exists(_.username == n.username))
								.orElse(allNotes.headOption)
								.fold(Vector.empty)(_.notes)

							notes.headOption.flatMap(parseConventionFromNote).fold(IO.pure(g)): conv =>
								configRef.modify: c =>
									(c.copy(convention = conv), c.convention != conv)
								.map: changed =>
									if changed then
										Log.info(s"restored settings from first card note: $conv")
										newGameFromSettings(tID, g.base._1, conv)
									else g
							.flatMap: game =>
								configRef.get.flatMap: config =>
									writeInfoNote(tID, notes, game, config.convention).flatMap:
										case r: Reactor  => gameRef.set(Some(r.copy(catchup = true)))
										case r: RefSieve => gameRef.set(Some(r.copy(catchup = true)))
										case h: HGroup   => gameRef.set(Some(h.copy(catchup = true)))
							*>
							// Skip last action so we can react appropriately
							events.dropRight(1).traverse_(handleAction) *>
							gameRef.update:
								case Some(g2: Reactor)  => Some(g2.copy(catchup = false))
								case Some(g2: RefSieve) => Some(g2.copy(catchup = false))
								case Some(g2: HGroup)   => Some(g2.copy(catchup = false))
								case other => other
							*>
							sendCmd("gameLoaded", ujson.Obj("tableId" -> tID)) *>
							IO.sleep(1000.millis) *>
							events.lastOption.traverse_(handleAction)

			case "tableJoined" =>
				val id = data("tableId").num.toInt

				data("stage").str match
					case "pregame" =>
						configRef.update(_.copy(leavePregameIfOnlyBots = true))	*>	// reset to true on each table join
						sessionRef.updateAndGet:
							_.copy(tableID = Some(id), gameStarted = false, lastRequester = None)
						.flatMap:
							_.tables.get(id).traverse_(checkSupportedSettings)

					case "justStarted" | "playing" =>
						allocRequestID.flatMap: reqID =>
							sessionRef.update(_.copy(
								tableID = Some(id),
								gameStarted = true,
								lastRequester = None,
								lastInitIds = Some((reqID, id))
							)) *>
							sendCmd("gameInitLoad", ujson.Obj("tableId" -> id), id = Some(reqID))

					case _ =>
						sessionRef.update(_.copy(
							tableID = Some(id),
							gameStarted = false,
							lastRequester = None
						))

			case "gameFinished" =>
				sessionRef.update(_.copy(gameStarted = false))

			case "tableLeft" =>
				val id = data("tableId").num.toInt
				sessionRef.update(s => s.copy(
					tableID = None,
					gameStarted = false,
					pregamePlayerIds = s.pregamePlayerIds - id
				))

			case "pregame" =>
				val pregame = upickle.read[PregameMessage](data)
				sessionRef.update(s => s.copy(
					pregamePlayerIds = s.pregamePlayerIds + (pregame.tableId -> pregame.players.map(_.userId))
				))

			case "table" =>
				val table = upickle.read[Table](data)

				sessionRef.get.flatMap: session =>
					IO.whenA(session.tableID.contains(table.id) && !session.gameStarted):
						configRef.get.flatMap: config =>
							if config.leaveReplayIfOnlyBots &&
								table.phase.isSharedReplay &&
								table.spectators.forall(s => session.bots.contains(s.username)) then
								Log.info("Leaving game. Only bots left spectating.")
								leaveRoom()
							else if config.leavePregameIfOnlyBots &&
								table.phase != TablePhase.Playing &&
								table.playerNames.forall(session.bots.contains) then
								Log.info("Leaving game. Only bots left in lobby.")
								leaveRoom()
							else
								checkSupportedSettings(table)
				*>
				sessionRef.update(s => s.copy(
					tables = s.tables + (table.id -> table)
				))

			case "tableRemoved" =>
				val id = data("tableId").num.toInt
				sessionRef.update(s => s.copy(
					tables = s.tables - id,
					pregamePlayerIds = s.pregamePlayerIds - id
				))

			case "user" =>
				val user = upickle.read[LobbyUser](data)
				IO.whenA(user.bot):
					sessionRef.update(s => s.copy(bots = s.bots + user.username))

			case "warning" =>
				handleWarning(data("message").str)

			case "error" =>
				IO.println(s"${Console.RED}error: ${data("message").str}${Console.RESET}")

			case "welcome" =>
				val welcome = upickle.read[WelcomeMessage](data)

				sessionRef.update(_.copy(
					info = Some(SelfData(welcome.username)),
					tables = welcome.tables.map(t => t.id -> t).toMap,
					bots = welcome.users.filter(_.bot).map(_.username).toSet
				)) *>
				welcome.playingAtTables.headOption.traverse_ : id =>
					Log.info(s"automatically reattending table with id $id")
					sendCmd("tableReattend", ujson.Obj("tableId" -> id))

			case _ => IO.unit

	def handleWarning(msg: String): IO[Unit] =
		sessionRef.modify(s => (s.copy(lastRequester = None), s.lastRequester)).flatMap: who =>
			who.traverse_(sendPM(_, msg))
		*>
		IO { println(s"warn: $msg") }

	def leaveRoom(): IO[Unit] =
		sessionRef.getAndUpdate:
			_.copy(tableID = None, gameStarted = false)
		.flatMap: session =>
			session.tableID.traverse_ : tableID =>
				val cmd = if session.gameStarted then "tableUnattend" else "tableLeave"
				sendCmd(cmd, ujson.Obj("tableId" -> tableID))
		*>
		gameRef.set(None)

	def handleInit(tID: Int, data: InitMessage) =
		val InitMessage(InitMode(ourPlayerIndex), playerNames, seed, options) = data
		val variant = Variant.getVariant(options.variantId)
		val state = State(playerNames, ourPlayerIndex, variant, options)

		sessionRef.update(_.copy(tableID = Some(tID))) *>
		configRef.get.flatMap: config =>
			val game = newGameFromSettings(tID, state, config.convention)
			gameRef.set(Some(game))
		*>
		sendCmd("gameStateLoad", ujson.Obj("tableId" -> tID))

	def handleChat(data: ChatMessage): IO[Unit] =
		val ChatMessage(text, kind, tableID) = data

		sessionRef.get.flatMap: session =>
			kind match
				case ChatKind.Public(author) =>
					IO.whenA(tableID.isDefined):
						if text.startsWith("!setall") then
							assignSettings(text)
						else if text.startsWith("!leaveall") then
							leaveRoom()
						else
							IO.unit
				case ChatKind.Private(ChatAuthor.User(username), recipient) if recipient == session.info.get.username =>
					handlePM(username, text)
				case _ => IO.unit

	def handlePM(who: String, msg: String) =
		if msg.startsWith("/join") then
			sessionRef.get.flatMap: session =>
				val table = session.tables.values.filter: t =>
					(t.playerNames.contains(who) && !t.phase.isSharedReplay) ||
					t.spectators.exists(_.username == who)
				.maxByOption(_.id)

				table match
					case Some(t) if t.passwordProtected =>
						msg.split(" ").lift(1) match
							case Some(password) =>
								sessionRef.update(_.copy(lastRequester = Some(who))) *>
								sendCmd("tableJoin", ujson.Obj("tableId" -> t.id, "password" -> password))
							case None =>
								sendPM(who, "Room is password protected, please provide a password.")
					case Some(t) => sendCmd("tableJoin", ujson.Obj("tableId" -> t.id))
					case None => sendPM(who, "Could not join, as you are not in a room.")

		else if msg.startsWith("/rejoin") then
			gameRef.get.flatMap:
				case Some(_) => sendPM(who, "Could not rejoin, as the bot is already in a game.")
				case None => sessionRef.get.flatMap: session =>
					val table = session.tables.values
						.filter(_.playerNames.contains(session.info.get.username))
						.maxByOption(_.id)

					table match
						case Some(t) => sendCmd("tableReattend", ujson.Obj("tableId" -> t.id))
						case None => sendPM(who, "Could not rejoin, as the bot is not a player in any open room.")

		else if msg.startsWith("/start") then
			sessionRef.get.flatMap: session =>
				session.tableID match
					case Some(id) =>
						val playerIds = session.pregamePlayerIds.getOrElse(id, Vector())
						sessionRef.update(_.copy(lastRequester = Some(who))) *>
						sendCmd("tableStart", ujson.Obj("tableId" -> id, "intendedPlayerIds" -> playerIds))
					case None =>
						sendPM(who, "Cannot start table, as the bot is not in a table.")

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
			configRef.modify: c =>
				(c.copy(fastMode = !c.fastMode), !c.fastMode)
			.flatMap: fastMode =>
				sendPM(who, s"Fast mode is now ${if fastMode then "on" else "off"}.")

		else if msg.startsWith("/doc") then
			configRef.get.flatMap: config =>
				config.convention match
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
			sessionRef.get.flatMap: session =>
				session.tableID match
					case Some(_) =>
						sendPM(who, s"This bot will now remain in the lobby even if there are only bots left.") *>
						configRef.update(_.copy(leavePregameIfOnlyBots = false))
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

						val computeAndSend =
							for
								config     <- configRef.get
								sleepFiber <- IO.sleep(2.seconds).whenA(!config.fastMode).start
								action     <- suggestedActionIO
								session    <- sessionRef.get
							yield
								Log.highlight(Console.BLUE, s"Suggested action: ${action.fmt(newGame, accordingTo = Some(newGame.me))}")
								val arg = ujson.Obj("tableId" -> session.tableID.get, "action" -> action.json)

								if newGame.inProgress then
									// Race against 2 second timer, then send.
									sleepFiber.join *> sendCmd("playerAction", arg)
								else
									sleepFiber.cancel

						computeAndSend.flatten.start.void

	def assignSettings(text: String, from: Option[String] = None) =
		val reply = from match
			case Some(username) =>
				(msg: String) => sendPM(username, msg)
			case None =>
				(msg: String) => sendChat(msg)

		text.split(" ", 2) match
			case Array(_) =>
				configRef.get.flatMap: config =>
					reply(s"Currently playing with ${config.convention} conventions.${if config.fastMode then " [fast mode]" else ""}")

			case Array(_, convStr) =>
				Convention.from(convStr) match
					case Left(msg) => reply(msg)
					case Right(c) => configRef.updateAndGet(_.copy(convention = c)).flatMap: config =>
						reply(s"Currently playing with $c conventions.${if config.fastMode then " [fast mode]" else ""}") *> {
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
		sessionRef.get.flatMap: session =>
			sendCmd("chat", ujson.Obj("text" -> msg, "tableId" -> session.tableID.get))

	def sendCmd(cmd: String, data: ujson.Value, id: Option[Int] = None) =
		for
			reqID <- id.fold(allocRequestID)(IO.pure)
			_ <- IO.println(s"${Console.RED}Sending command: $reqID $cmd $data${Console.RESET}")
			_ <- queue.offer(ujson.write(ujson.Obj("id" -> reqID, "request" -> cmd, "data" -> data)))
		yield ()
