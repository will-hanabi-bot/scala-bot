package tests.protocol

import scala_bot.*

import cats.effect.{IO, Ref, std}
import cats.effect.unsafe.implicits.global
import scala.concurrent.duration.*
import scala_bot.basics.*
import upickle.default.read

class CurrentWireSuite extends munit.FunSuite:
	// Outgoing contract: Hanabi-Live/hanabi-live@7e4dd3325e8acc68332a10ad7e11aa21ced1b9a1.
	// These incoming fixtures retain the earlier wire layouts; LiveWireSuite covers the current server.
	private val table = ujson.read("""{"tableID":3,"joined":true,"maxPlayers":5,"name":"Alice's game","numPlayers":1,"options":{"numPlayers":2,"variantName":"No Variant"},"ownerID":7,"passwordProtected":false,"playerNames":["Alice"],"progress":0,"phase":"pregame","spectators":[{"username":"Carol","shadowingPlayerIndex":1},{"username":"Observer"}]}""")
	private def incoming(command: String, data: ujson.Value): ujson.Value =
		ujson.read(WireProtocol.incoming(s"$command $data").split(" ", 2)(1))
	private def outgoing(frame: String): (String, ujson.Value) =
		val parts = WireProtocol.outgoing(frame).split(" ", 2)
		(parts(0), ujson.read(parts(1)))

	// hanabi-live@5573e494 moved these fields into TableSettings.
	private def nestedTable(): ujson.Value =
		val copy = ujson.read(table.toString)
		copy("settings") = ujson.Obj.from(List("name", "maxPlayers", "options").map: key =>
			key -> copy.obj.remove(key).get)
		copy("settings")("options")("numPlayers") = 1
		copy("settings")("options")("deckPlays") = true
		copy

	test("nested table settings survive single updates and lobby lists in every phase"):
		for (phase, running, replay) <- List(("pregame", false, false), ("playing", true, false), ("replay", false, false), ("sharedReplay", false, true)) do
			val wire = nestedTable()
			wire("phase") = phase
			val decoded = read[Table](incoming("table", wire))
			assertEquals(decoded.id, 3)
			assertEquals(decoded.name, "Alice's game")
			assertEquals(decoded.maxPlayers, 5)
			assertEquals(decoded.variant, "No Variant")
			assertEquals(decoded.options, Some(TableOptions(1, "No Variant", deckPlays = true)))
			assertEquals(decoded.players, Vector("Alice"))
			assertEquals(decoded.spectators, List(Spectator("Carol", 1), Spectator("Observer", -1)))
			assertEquals((decoded.running, decoded.sharedReplay), (running, replay))
			assertEquals(read[List[Table]](incoming("tableList", ujson.Arr(wire))), List(decoded))

	test("BotClient joins and reattends from rosters with nested table settings"):
		val queue = std.Queue.unbounded[IO, String].unsafeRunSync()
		val ref = Ref.of[IO, Option[Game]](None).unsafeRunSync()
		val client = new BotClient(queue, ref)
		def receive(frame: String): Unit = client.handleMsg(WireProtocol.incoming(frame)).unsafeRunSync()
		receive("welcome {\"userID\":9,\"username\":\"TestBot\",\"playingAtTables\":[]}")
		receive(s"tableList ${ujson.Arr(nestedTable())}")
		receive("chat {\"text\":\"/join\",\"source\":\"user\",\"username\":\"Alice\",\"recipient\":\"TestBot\"}")
		assertEquals(WireProtocol.outgoing(queue.take.unsafeRunSync()), "tableJoin {\"tableId\":3}")
		val playing = nestedTable()
		playing("phase") = "playing"
		playing("playerNames") = ujson.Arr("Alice", "TestBot")
		playing("numPlayers") = 2
		playing("settings")("options")("numPlayers") = 2
		receive(s"tableList ${ujson.Arr(playing)}")
		assertEquals(WireProtocol.outgoing(queue.take.unsafeRunSync()), "tableReattend {\"tableId\":3}")


	test("current table and roster preserve IDs, options, players and spectators"):
		val decoded = read[Table](incoming("table", table))
		assertEquals(decoded.id, 3)
		assertEquals(decoded.players, Vector("Alice"))
		assertEquals(decoded.spectators, List(Spectator("Carol", 1), Spectator("Observer", -1)))
		assert(!decoded.running && !decoded.sharedReplay)
		assertEquals(read[List[Table]](incoming("tableList", ujson.Arr(table))), List(decoded))
		for (phase, running, replay) <- List(("playing", true, false), ("replay", false, false), ("sharedReplay", false, true)) do
			val copy = ujson.read(table.toString)
			copy("phase") = phase
			val t = read[Table](incoming("table", copy))
			assertEquals((t.running, t.sharedReplay), (running, replay))

	test("missing required table IDs fail instead of inventing a target"):
		val copy = ujson.read(table.toString)
		val _ = copy.obj.remove("tableID")
		intercept[Exception](read[Table](incoming("table", copy)))

	test("chat preserves private recipient, table scope and escaped text"):
		val pm = incoming("chat", ujson.read("""{"text":"/join","source":"user","username":"Alice","recipient":"TestBot","datetime":"2026-01-01T12:00:00.000Z"}"""))
		assertEquals(read[ChatMessage](pm), ChatMessage("/join", "Alice", "lobby", "TestBot"))
		val history = incoming("chatList", ujson.read("""{"messages":[{"text":"&amp;","source":"server","tableID":3,"datetime":"2026-01-01T12:00:00.000Z"}],"unread":false}"""))
		assertEquals(read[ChatList](history).list, Vector(ChatMessage("&amp;", "", "table3", "")))

	test("game startup and resume use the current command names"):
		assertEquals(WireProtocol.incoming("tableStarted {\"tableID\":3,\"stage\":\"inProgress\"}").takeWhile(_ != ' '), "tableStart")
		assertEquals(outgoing("getGameInfo1 {\"tableID\":3}"), ("gameInitLoad", ujson.Obj("tableId" -> 3)))
		assertEquals(outgoing("getGameInfo2 {\"tableID\":3}"), ("gameStateLoad", ujson.Obj("tableId" -> 3)))
		val init = incoming("init", ujson.read("""{"tableID":3,"playerNames":["Alice","TestBot"],"ourPlayerIndex":1,"mode":"playing","seed":"test","options":{"numPlayers":2,"variantName":"No Variant"}}"""))
		assert(!read[InitMessage](init).replay)

	test("hidden draws stay unknown and optional game-over player stays absent"):
		val msg = GameActionListMessage.fromJSON(incoming("gameActionList", ujson.read("""{"tableID":3,"actions":[{"type":"draw","playerIndex":0,"order":0},{"type":"draw","playerIndex":1,"order":1,"suitIndex":2,"rank":3},{"type":"turn","num":1,"currentPlayerIndex":0},{"type":"turn","num":2},{"type":"gameOver","endCondition":1}]}""")))
		assertEquals(msg.list, Seq(DrawAction(0, 0, -1, -1), DrawAction(1, 1, 2, 3), TurnAction(1, 0), TurnAction(2, -1), GameOverAction(1, -1)))

	test("rank and color clues reach the strategy with the touched cards intact"):
		for (kind, expected) <- List(("rank", ClueKind.Rank), ("color", ClueKind.Colour)) do
			val event = ujson.Obj("tableID" -> 3, "action" -> ujson.Obj("type" -> "clue", "clue" -> ujson.Obj("type" -> kind, "value" -> 1), "giver" -> 0, "target" -> 1, "orders" -> ujson.Arr(3, 4), "turn" -> 2))
			val action = GameActionMessage.fromJSON(incoming("gameAction", event)).get.action
			assertEquals(action, ClueAction(0, 1, Seq(3, 4), BaseClue(expected, 1)))

	test("all bot actions emit only fields accepted by the current server"):
		val cases = List(
			(PerformAction.Play(7), "playerAction", ujson.Obj("tableId" -> 3, "action" -> ujson.Obj("type" -> "play", "order" -> 7))),
			(PerformAction.Discard(8), "playerAction", ujson.Obj("tableId" -> 3, "action" -> ujson.Obj("type" -> "discard", "order" -> 8))),
			(PerformAction.Colour(1, 2), "playerAction", ujson.Obj("tableId" -> 3, "action" -> ujson.Obj("type" -> "clue", "receiverIndex" -> 1, "clue" -> ujson.Obj("type" -> "color", "value" -> 2)))),
			(PerformAction.Rank(1, 5), "playerAction", ujson.Obj("tableId" -> 3, "action" -> ujson.Obj("type" -> "clue", "receiverIndex" -> 1, "clue" -> ujson.Obj("type" -> "rank", "value" -> 5)))),
			(PerformAction.Terminate(0, 0), "tableTerminate", ujson.Obj("tableId" -> 3)))
		for (action, command, expected) <- cases do
			assertEquals(outgoing(s"action ${action.json(3)}"), (command, expected))

	test("the previously rejected play uses the current playerAction request"):
		assertEquals(outgoing("action {\"tableID\":8732,\"type\":0,\"target\":8}"),
			("playerAction", ujson.read("""{"tableId":8732,"action":{"type":"play","order":8}}""")))

	test("color clue matches the current server compatibility fixture"):
		// hanabi-live@1bd43329, server/protocol/fixtures/compatibility.json:
		// "color clues name their target and color" (valid client playerAction).
		val expected = ujson.read("""{"tableId":7,"action":{"type":"clue","receiverIndex":1,"clue":{"type":"color","value":2}}}""")
		assertEquals(outgoing(s"action ${PerformAction.Colour(1, 2).json(7)}"), ("playerAction", expected))

	test("a real BotClient turn emits a nested move after processing the deal"):
		import scala_bot.reactor.Reactor
		val variant = Variant(0, "No Variant", Vector("Red", "Yellow", "Green", "Blue", "Purple"))
		val queue = std.Queue.unbounded[IO, String].unsafeRunSync()
		val ref = Ref.of[IO, Option[Game]](None).unsafeRunSync()
		val client = new BotClient(queue, ref)
		def receive(frame: String): Unit = client.handleMsg(WireProtocol.incoming(frame)).unsafeRunSync()
		receive("welcome {\"userID\":9,\"username\":\"TestBot\",\"playingAtTables\":[]}")
		receive("joined {\"tableID\":3}")
		ref.set(Some(Reactor(3,
			State(Vector("TestBot", "Alice"), 0, variant, TableOptions(2, "No Variant")), inProgress = true).copy(catchup = true))).unsafeRunSync()
		for order <- 0 until 10 do
			val draw = ujson.Obj("type" -> "draw", "playerIndex" -> (order / 5), "order" -> order)
			if order >= 5 then
				draw("suitIndex") = order - 5
				draw("rank") = 1
			receive(s"gameAction ${ujson.Obj("tableID" -> 3, "action" -> draw)}")
		ref.update {
			case Some(game: Reactor) => Some(game.copy(catchup = false))
			case other => other
		}.unsafeRunSync()
		receive("gameAction {\"tableID\":3,\"action\":{\"type\":\"turn\",\"num\":0,\"currentPlayerIndex\":0}}")
		def takeMove: IO[String] = queue.take.flatMap(frame => if frame.startsWith("action ") then IO.pure(frame) else takeMove)
		val (command, data) = outgoing(takeMove.timeout(5.seconds).unsafeRunSync())
		assertEquals(command, "playerAction")
		assertEquals(data.obj.keySet.toSet, Set("tableId", "action"))
		assertEquals(data("tableId").num.toInt, 3)
		assert(Set("play", "discard", "clue").contains(data("action")("type").str))


	test("outgoing chat cannot leak old room or action fields"):
		assertEquals(outgoing("chatPM {\"msg\":\"hello\",\"recipient\":\"Alice\",\"room\":\"lobby\"}"), ("chatPm", ujson.Obj("text" -> "hello", "recipient" -> "Alice")))
		assertEquals(outgoing("chat {\"msg\":\"hello\",\"room\":\"table3\"}"), ("chat", ujson.Obj("text" -> "hello", "tableId" -> 3)))
		assertEquals(outgoing("note {\"tableID\":3,\"order\":0,\"note\":\"settings\"}"), ("note", ujson.Obj("tableId" -> 3, "order" -> 0, "text" -> "settings")))

	test("actual BotClient joins from a PM, starts and reattends using modern frames"):
		val queue = std.Queue.unbounded[IO, String].unsafeRunSync()
		val ref = Ref.of[IO, Option[Game]](None).unsafeRunSync()
		val client = new BotClient(queue, ref)
		def receive(frame: String): Unit = client.handleMsg(WireProtocol.incoming(frame)).unsafeRunSync()
		def take(): (String, ujson.Value) = outgoing(queue.take.timeout(2.seconds).unsafeRunSync())
		receive("welcome {\"userID\":9,\"username\":\"TestBot\",\"playingAtTables\":[]}")
		receive(s"tableList ${ujson.Arr(table)}")
		receive("chat {\"text\":\"/join\",\"source\":\"user\",\"username\":\"Alice\",\"recipient\":\"SomeoneElse\"}")
		assertEquals(queue.tryTake.unsafeRunSync(), None)
		receive("chat {\"text\":\"/join\",\"source\":\"user\",\"username\":\"Alice\",\"recipient\":\"TestBot\"}")
		assertEquals(take(), ("tableJoin", ujson.Obj("tableId" -> 3)))
		receive("joined {\"tableID\":3}")
		receive("tableStarted {\"tableID\":3,\"stage\":\"justStarted\"}")
		assertEquals(take(), ("gameInitLoad", ujson.Obj("tableId" -> 3)))
		val playing = ujson.read(table.toString)
		playing("phase") = "playing"
		playing("playerNames") = ujson.Arr("Alice", "TestBot")
		receive(s"tableList ${ujson.Arr(playing)}")
		assertEquals(take(), ("tableReattend", ujson.Obj("tableId" -> 3)))

	test("current game events update real strategy state through a full deal and ending"):
		import scala_bot.reactor.Reactor
		val variant = Variant(0, "No Variant", Vector("Red", "Yellow", "Green", "Blue", "Purple"))
		val options = TableOptions(2, "No Variant")
		var game = Reactor(3, State(Vector("TestBot", "Alice"), 0, variant, options), inProgress = true)
		def event(data: ujson.Value): Unit =
			val decoded = GameActionMessage.fromJSON(incoming("gameAction", ujson.Obj("tableID" -> 3, "action" -> data))).get
			game = game.handleAction(decoded.action)
		for order <- 0 until 10 do
			val draw = ujson.Obj("type" -> "draw", "playerIndex" -> (order / 5), "order" -> order)
			if order >= 5 then
				draw("suitIndex") = order - 5
				draw("rank") = 1
			event(draw)
		assertEquals(game.state.hands.map(_.size), Vector(5, 5))
		assertEquals(game.deckIds.take(5), Vector.fill(5)(None))
		event(ujson.Obj("type" -> "turn", "num" -> 0, "currentPlayerIndex" -> 0))
		assertEquals(game.state.turnCount, 1)
		event(ujson.Obj("type" -> "discard", "playerIndex" -> 0, "order" -> 0, "suitIndex" -> 0, "rank" -> 3, "failed" -> true))
		assertEquals(game.state.strikes, 1)
		assertEquals(game.state.hands.head.size, 4)
		event(ujson.Obj("type" -> "draw", "playerIndex" -> 0, "order" -> 10))
		event(ujson.Obj("type" -> "turn", "num" -> 1, "currentPlayerIndex" -> 1))
		assertEquals(game.state.currentPlayerIndex, 1)
		event(ujson.Obj("type" -> "play", "playerIndex" -> 1, "order" -> 5, "suitIndex" -> 0, "rank" -> 1))
		assertEquals(game.state.playStacks.head, 1)
		event(ujson.Obj("type" -> "turn", "num" -> 2))
		event(ujson.Obj("type" -> "gameOver", "endCondition" -> 1))
		assertEquals(game.state.currentPlayerIndex, -1)
		assert(!game.inProgress)

	test("empty rosters and unrelated messages pass through without invented state"):
		assertEquals(incoming("tableList", ujson.Arr()), ujson.Arr())
		assertEquals(incoming("user", ujson.Obj("userID" -> 5, "username" -> "Alice")), ujson.Obj("userID" -> 5, "username" -> "Alice"))
		assertEquals(outgoing("tableLeave {\"tableID\":3}"), ("tableLeave", ujson.Obj("tableId" -> 3)))

	test("replay modes are translated without changing players or options"):
		for (mode, replay) <- List(("playing", false), ("spectating", false), ("shadowing", false), ("replay", true), ("sharedReplay", true)) do
			val data = ujson.Obj("tableID" -> 3, "playerNames" -> ujson.Arr("Alice", "TestBot"), "ourPlayerIndex" -> 1, "mode" -> mode, "seed" -> "test", "options" -> ujson.Obj("numPlayers" -> 2, "variantName" -> "No Variant"))
			val init = read[InitMessage](incoming("init", data))
			assertEquals(init.replay, replay)
			assertEquals(init.playerNames, Vector("Alice", "TestBot"))
			assertEquals(init.options, TableOptions(2, "No Variant"))

	test("server warnings keep the original message"):
		assertEquals(incoming("warning", ujson.Obj("message" -> "Table is full")), ujson.Obj("warning" -> "Table is full"))

	test("hidden clues and unknown protocol modes fail rather than guessing"):
		val _ = intercept[Exception]:
			incoming("gameAction", ujson.read("""{"tableID":3,"action":{"type":"clue","giver":0,"target":1,"orders":[3,4],"turn":2}}"""))
		val unknown = ujson.read(table.toString)
		unknown("phase") = "unknown"
		val _ = intercept[IllegalArgumentException](incoming("table", unknown))
		val _ = intercept[IllegalArgumentException](incoming("init", ujson.Obj("mode" -> "unknown")))

	test("player timing events do not remove the final game-over action"):
		val data = incoming("gameActionList", ujson.read("""{"tableID":3,"actions":[{"type":"turn","num":7},{"type":"gameOver","endCondition":1},{"type":"playerTimes","playerTimesMs":[1000,2000],"durationMs":3000}]}"""))
		assertEquals(GameActionListMessage.fromJSON(data).list, Seq(TurnAction(7, -1), GameOverAction(1, -1)))

	test("live timing notifications are not decoded as player actions"):
		val frame = WireProtocol.incoming("gameAction {\"tableID\":3,\"action\":{\"type\":\"playerTimes\",\"playerTimesMs\":[1000,2000],\"durationMs\":3000}}")
		assertEquals(frame.takeWhile(_ != ' '), "playerTimes")
		val queue = std.Queue.unbounded[IO, String].unsafeRunSync()
		val ref = Ref.of[IO, Option[Game]](None).unsafeRunSync()
		new BotClient(queue, ref).handleMsg(frame).unsafeRunSync()
		assertEquals(queue.tryTake.unsafeRunSync(), None)
		assertEquals(ref.get.unsafeRunSync(), None)
