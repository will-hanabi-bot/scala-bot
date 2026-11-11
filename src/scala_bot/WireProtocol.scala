package scala_bot

/** Adapt hanab.live's typed wire protocol to the existing strategy model.
	* Contract: Hanabi-Live/hanabi-live@7e4dd3325e8acc68332a10ad7e11aa21ced1b9a1,
	* server/protocol/src/{lobby,chat,game,requests}.rs. Keep unknown card identities unknown.
	*/
object WireProtocol:
	// One decoder per WebSocket. Only the server's welcome establishes note ownership.
	final class Decoder:
		private var username: Option[String] = None

		def incoming(frame: String): String = transform(frame): (command, data) =>
			if command == "welcome" then username = Some(data("username").str)
			incomingMessage(command, data, username)

	private def rename(data: ujson.Value, from: String, to: String): Unit =
		data.objOpt.flatMap(_.get(from)).foreach: value =>
			data(to) = value
			val _ = data.obj.remove(from)

	private def legacyIds(data: ujson.Value): Unit =
		rename(data, "tableId", "tableID")
		rename(data, "userId", "userID")

	private def options(data: ujson.Value, players: ujson.Value): ujson.Value =
		// The current protocol makes the roster authoritative for player counts.
		if !data.obj.contains("numPlayers") then data("numPlayers") = players.arr.size
		if !data.obj.contains("variantName") then data("variantName") = "No Variant"
		data

	private def transform(frame: String)(f: (String, ujson.Value) => (String, ujson.Value)): String =
		val split = frame.indexOf(' ')
		require(split > 0, "Expected a command and JSON payload")
		val (command, data) = f(frame.take(split), ujson.read(frame.drop(split + 1)))
		s"$command ${ujson.write(data)}"

	private def table(data: ujson.Value, username: Option[String]): ujson.Value =
		legacyIds(data)
		if data.obj.contains("phase") then
			val phase = data("phase").str
			require(Set("pregame", "playing", "replay", "sharedReplay").contains(phase), "Unknown table phase")
			data("id") = data("tableID")
			data("players") = data("playerNames")
			if !data.obj.contains("joined") then
				val self = username.getOrElse(throw new IllegalArgumentException("Table arrived before welcome"))
				data("joined") = data("playerNames").arr.exists(_.str == self)
			val settings = data.obj.getOrElse("settings", data)
			data("name") = settings("name")
			data("maxPlayers") = settings.obj.getOrElse("maxPlayers", ujson.Num(5))
			data("options") = options(settings.obj.getOrElse("options", ujson.Obj()), data("playerNames"))
			data("variant") = data("options")("variantName")
			data("running") = phase == "playing"
			data("sharedReplay") = phase == "sharedReplay"
			data("spectators") = ujson.Arr.from(data("spectators").arr.map: spectator =>
				ujson.Obj(
					"name" -> spectator("username"),
					"shadowingPlayerIndex" -> spectator.obj.getOrElse("shadowingPlayerIndex", ujson.Num(-1))))
		data

	private def chat(data: ujson.Value): ujson.Value =
		legacyIds(data)
		if data.obj.contains("text") then
			ujson.Obj(
				"msg" -> data("text"),
				"who" -> data.obj.getOrElse("username", ujson.Str("")),
				"room" -> data.obj.get("tableID").map(id => s"table${id.num.toInt}").getOrElse("lobby"),
				"recipient" -> data.obj.getOrElse("recipient", ujson.Str("")))
		else data

	private def ownNotes(data: ujson.Value, username: Option[String]): ujson.Value =
		val self = username.getOrElse(throw new IllegalArgumentException("Notes arrived before welcome"))
		val own = data("notes").arr.filter(_("username").str == self)
		val player = own.filter(!_("spectator").bool)
		val selected = if player.nonEmpty then player else own
		require(selected.size == 1, "Expected exactly one applicable note list for the authenticated bot")
		selected.head("notes")

	private def action(data: ujson.Value): ujson.Value =
		data("type").str match
			case kind @ ("draw" | "play" | "discard" | "misplay") =>
				data.obj.get("identity").foreach: identity =>
					data("suitIndex") = identity("suitIndex")
					data("rank") = identity("rank")
				for key <- List("suitIndex", "rank") do
					if !data.obj.contains(key) then data(key) = -1
				if kind == "misplay" then
					data("type") = "discard"
					data("failed") = true
				else if kind == "discard" && !data.obj.contains("failed") then data("failed") = false
			case "clue" if data.obj.contains("orders") =>
				require(!data.obj.get("ignoreNegative").exists(_.bool), "Hypothetical clues are unsupported")
				rename(data, "playerIndex", "giver")
				rename(data, "receiverIndex", "target")
				data("list") = data("orders")
				// Blind clues are unsupported by the strategy; never fabricate a clue.
				val clue = data("clue")
				clue("type") = clue("type").str match
					case "color" => ujson.Num(0)
					case "rank" => ujson.Num(1)
					case _ => throw new IllegalArgumentException("Unknown clue kind")
			case "turn" =>
				rename(data, "turn", "num")
				if !data.obj.contains("currentPlayerIndex") then data("currentPlayerIndex") = -1
			case "gameOver" =>
				data("endCondition").strOpt.foreach: name =>
					val endings = Vector("inProgress", "normal", "strikeout", "timeout", "terminatedByPlayer",
						"speedrunFail", "idleTimeout", "characterSoftlock", "allOrNothingFail", "allOrNothingSoftlock", "terminatedByVote")
					val index = endings.indexOf(name)
					require(index >= 0, "Unknown end condition")
					data("endCondition") = index
				if !data.obj.contains("playerIndex") then data("playerIndex") = -1
			case _ => ()
		data

	private def gameEvent(data: ujson.Value, event: ujson.Value): (String, ujson.Value) =
		if event("type").str == "playerTimes" then ("playerTimes", data)
		else
			data("action") = action(event)
			("gameAction", data)

	def incoming(frame: String): String = transform(frame): (command, data) =>
		incomingMessage(command, data, None)

	private def incomingMessage(command: String, data: ujson.Value, username: Option[String]): (String, ujson.Value) =
		legacyIds(data)
		command match
			case "table" => (command, table(data, username))
			case "tableList" =>
				val tables = data.objOpt.flatMap(_.get("tables")).getOrElse(data)
				(command, ujson.Arr.from(tables.arr.map(value => table(value, username))))
			case "chat" => (command, chat(data))
			case "chatList" if data.obj.contains("messages") =>
				(command, ujson.Obj("list" -> ujson.Arr.from(data("messages").arr.map(chat))))
			case "tableStarted" => ("tableStart", data)
			case "tableJoined" => ("joined", data)
			case "tableLeft" | "tableBoot" | "tableBooted" => ("left", data)
			case "tableRemoved" => ("tableGone", data)
			case "noteList" =>
				("noteListPlayer", ujson.Obj("tableID" -> data("tableID"), "notes" -> ownNotes(data, username)))
			case "gameState" =>
				data("list") = ujson.Arr.from(data("events").arr.map(action))
				data("notes") = ownNotes(data, username)
				(command, data)
			case "init" if data.obj.contains("mode") =>
				val view = data("mode")
				val mode = view.strOpt.getOrElse(view("type").str)
				require(Set("playing", "spectating", "shadowing", "replay", "sharedReplay").contains(mode), "Unknown game mode")
				if view.objOpt.nonEmpty then
					require(view.obj.contains("ourPlayerIndex"), "Unseated game views are unsupported")
					data("ourPlayerIndex") = view("ourPlayerIndex")
				// The strategy requires a real seat; never invent one for an unseated viewer.
				require(data.obj.contains("ourPlayerIndex"), "Unseated game views are unsupported")
				data("replay") = mode == "replay" || mode == "sharedReplay"
				data("options") = options(data("options"), data("playerNames"))
				// The strategy ignores this field; current custom-deck replays have no seed.
				if !data.obj.contains("seed") then data("seed") = ""
				(command, data)
			case "gameEventList" =>
				data("list") = ujson.Arr.from(data("events").arr.map(action))
				("gameActionList", data)
			case "gameActionList" if data.obj.contains("actions") =>
				data("list") = ujson.Arr.from(data("actions").arr.map(action))
				(command, data)
			// Timing metadata has no strategy action; malformed events still fail.
			case "gameEvent" => gameEvent(data, data("event"))
			case "gameAction" => gameEvent(data, data("action"))
			case "warning" if data.obj.contains("message") =>
				(command, ujson.Obj("warning" -> data("message")))
			case _ => (command, data)

	def outgoing(frame: String): String = transform(frame): (command, data) =>
		rename(data, "tableID", "tableId")
		command match
			case "getGameInfo1" => ("gameInitLoad", data)
			case "getGameInfo2" => ("gameStateLoad", data)
			case "chatPM" =>
				("chatPm", ujson.Obj("text" -> data("msg"), "recipient" -> data("recipient")))
			case "chat" =>
				val result = ujson.Obj("text" -> data("msg"))
				val room = data("room").str
				if room != "lobby" then
					require(room.startsWith("table"), "Unknown chat room")
					result("tableId") = room.stripPrefix("table").toInt
				(command, result)
			case "note" =>
				(command, ujson.Obj("tableId" -> data("tableId"), "order" -> data("order"), "text" -> data("note")))
			case "action" =>
				val result = ujson.Obj("tableId" -> data("tableId"))
				data("type").num.toInt match
					case kind @ (0 | 1) =>
						result("action") = ujson.Obj(
							"type" -> (if kind == 0 then "play" else "discard"),
							"order" -> data("target"))
						("playerAction", result)
					case kind @ (2 | 3) =>
						result("action") = ujson.Obj(
							"type" -> "clue", "receiverIndex" -> data("target"),
							"clue" -> ujson.Obj("type" -> (if kind == 2 then "color" else "rank"), "value" -> data("value")))
						("playerAction", result)
					case 4 => ("tableTerminate", result)
					case _ => throw new IllegalArgumentException("Unknown action kind")
			case _ => (command, data)
