//> using test.resourceDir fixtures
package tests.protocol

import scala_bot.*

import cats.effect.{IO, Ref, std}
import cats.effect.unsafe.implicits.global
import scala.concurrent.duration.*
import scala_bot.basics.*
import upickle.default.read

class LiveWireSuite extends munit.FunSuite:
	// Every canonical frame in this snapshot was checked against the live revision's
	// generated server/client validators; its revision and provenance are in the file.
	private val fixtures = scala.util.Using.resource(scala.io.Source.fromResource("live-protocol.json"))(s => ujson.read(s.mkString))
	private def data(name: String): ujson.Value = ujson.read(fixtures("server")(name)("data").toString)
	private def frame(name: String): String = s"${fixtures("server")(name)("command").str} ${data(name)}"
	private def parts(frame: String): (String, ujson.Value) =
		val split = frame.split(" ", 2)
		(split(0), ujson.read(split(1)))
	private def decoder(): WireProtocol.Decoder =
		val wire = new WireProtocol.Decoder
		val _ = wire.incoming(frame("welcome"))
		wire
	private def incoming(name: String): (String, ujson.Value) = parts(decoder().incoming(frame(name)))
	private def event(name: String): Action = GameActionMessage.fromJSON(incoming(name)._2).get.action

	test("the live welcome identifies the bot without a legacy userID"):
		assertEquals(read[SelfData](incoming("welcome")._2), SelfData(9, "TestBot", Nil))

	test("current tables derive player counts from their rosters in all phases"):
		val decoded = read[Table](incoming("table")._2)
		assertEquals(decoded.id, 3)
		assertEquals(decoded.name, "Alice's game")
		assertEquals(decoded.maxPlayers, 5)
		assertEquals(decoded.options, Some(TableOptions(1, "No Variant")))
		assertEquals(decoded.spectators, List(Spectator("Observer", -1)))
		assertEquals(read[List[Table]](incoming("tableList")._2), List(decoded))
		for (phase, running, replay) <- List(("pregame", false, false), ("playing", true, false), ("replay", false, false), ("sharedReplay", false, true)) do
			val table = read[Table](incoming(phase)._2)
			assertEquals(table.options.map(_.numPlayers), Some(2))
			assertEquals((table.running, table.sharedReplay), (running, replay))

	test("init derives its player count and accepts an absent unused seed"):
		val init = read[InitMessage](incoming("init")._2)
		assertEquals(init.tableID, 3)
		assertEquals(init.options, TableOptions(2, "No Variant"))
		assertEquals(init.seed, "p2v0s1")
		assert(!init.replay)
		val replay = read[InitMessage](incoming("initNoSeed")._2)
		assert(replay.replay)
		assertEquals(replay.seed, "")

	test("nested visible identities are decoded while hidden identities stay unknown"):
		assertEquals(event("hiddenDraw"), DrawAction(0, 0, -1, -1))
		assertEquals(event("visibleDraw"), DrawAction(1, 5, 0, 1))
		assertEquals(event("play"), PlayAction(1, 5, 0, 1))
		assertEquals(event("discard"), DiscardAction(1, 5, 0, 1, false))
		assertEquals(event("misplay"), DiscardAction(0, 0, 0, 3, true))
		assertEquals(event("hiddenMisplay"), DiscardAction(0, 0, -1, -1, true))

	test("current clue events retain their kind and touched orders"):
		assertEquals(event("colorClue"), ClueAction(1, 0, Seq(0, 1), BaseClue(ClueKind.Colour, 1)))
		assertEquals(event("rankClue"), ClueAction(1, 0, Seq(0, 1), BaseClue(ClueKind.Rank, 1)))

	test("named ending conditions map exactly to the strategy's existing enum"):
		val endings = List("inProgress", "normal", "strikeout", "timeout", "terminatedByPlayer", "speedrunFail", "idleTimeout", "characterSoftlock", "allOrNothingFail", "allOrNothingSoftlock", "terminatedByVote")
		for (name, index) <- endings.zipWithIndex do
			val value = data("gameOver")
			value("event")("endCondition") = name
			val decoded = GameActionMessage.fromJSON(parts(WireProtocol.incoming(s"gameEvent $value"))._2).get.action
			assertEquals(decoded, GameOverAction(index, -1))
		val invalid = data("gameOver")
		invalid("event")("endCondition") = "unknown"
		intercept[IllegalArgumentException](WireProtocol.incoming(s"gameEvent $invalid"))

	test("current turn events keep their turn number at the legacy boundary"):
		assertEquals(event("turn"), TurnAction(0, 0))
		assertEquals(event("endedTurn"), TurnAction(5, -1))

	test("unseated game views do not fabricate a player seat"):
		intercept[IllegalArgumentException](WireProtocol.incoming(frame("unseated")))

	test("hypothetical clues that ignore negative information are rejected"):
		val clue = data("colorClue")
		clue("event")("ignoreNegative") = true
		intercept[IllegalArgumentException](WireProtocol.incoming(s"gameEvent $clue"))

	test("event histories preserve the deal and ending while timing stays metadata"):
		val deal = GameActionListMessage.fromJSON(incoming("deal")._2)
		assertEquals(deal.tableID, 3)
		assertEquals(deal.list.size, 11)
		assertEquals(deal.list.last, TurnAction(0, 0))
		assertEquals(GameActionListMessage.fromJSON(incoming("ending")._2).list, Seq(TurnAction(5, -1), GameOverAction(1, -1)))
		assertEquals(incoming("timing")._1, "playerTimes")

	test("chat scope, notes, lifecycle IDs and warnings reach the existing readers"):
		assertEquals(read[ChatMessage](incoming("chat")._2), ChatMessage("/join", "Alice", "lobby", "TestBot"))
		assertEquals(read[ChatList](incoming("chatList")._2).list, Vector(ChatMessage("&amp;", "", "table3", "")))
		assertEquals(read[NoteListPlayerMessage](incoming("notes")._2), NoteListPlayerMessage(3, Vector.empty))
		for name <- List("joined", "left", "boot", "tableGone", "started") do
			assertEquals(incoming(name)._2("tableID").num.toInt, 3)
		assertEquals(incoming("joined")._1, "joined")
		assertEquals(incoming("left")._1, "left")
		assertEquals(incoming("boot")._1, "left")
		assertEquals(incoming("tableGone")._1, "tableGone")
		assertEquals(incoming("started")._1, "tableStart")
		assertEquals(incoming("warning")._2, ujson.Obj("warning" -> "Table is full"))

	test("every supported outgoing frame matches a validated current-server fixture"):
		for example <- fixtures("client").arr do
			assertEquals(parts(WireProtocol.outgoing(example("input").str)), (example("command").str, example("data")))

	test("missing IDs and malformed hidden clues fail instead of inventing values"):
		val welcome = data("welcome")
		val _ = welcome.obj.remove("userId")
		intercept[Exception](read[SelfData](parts(WireProtocol.incoming(s"welcome $welcome"))._2))
		val clue = data("colorClue")
		val _ = clue("event").obj.remove("clue")
		intercept[Exception](WireProtocol.incoming(s"gameEvent $clue"))
		val draw = data("visibleDraw")
		val _ = draw("event")("identity").obj.remove("rank")
		intercept[Exception](WireProtocol.incoming(s"gameEvent $draw"))

	test("BotClient joins, loads notes, catches up, takes a turn and reattends current frames"):
		import scala_bot.reactor.Reactor
		val queue = std.Queue.unbounded[IO, String].unsafeRunSync()
		val ref = Ref.of[IO, Option[Game]](None).unsafeRunSync()
		val client = new BotClient(queue, ref)
		val wire = new WireProtocol.Decoder
		def receive(name: String): Unit = client.handleMsg(wire.incoming(frame(name))).unsafeRunSync()
		def take(): (String, ujson.Value) = parts(WireProtocol.outgoing(queue.take.timeout(6.seconds).unsafeRunSync()))
		receive("welcome")
		receive("tableList")
		receive("chat")
		assertEquals(take(), ("tableJoin", ujson.Obj("tableId" -> 3)))
		receive("joined")
		receive("started")
		assertEquals(take(), ("gameInitLoad", ujson.Obj("tableId" -> 3)))
		// Inject the decoded initial state with a local variant; no catalog HTTP request.
		val init = read[InitMessage](incoming("init")._2)
		val variant = Variant(0, "No Variant", Vector("Red", "Yellow", "Green", "Blue", "Purple"))
		ref.set(Some(Reactor(init.tableID, State(init.playerNames, init.ourPlayerIndex, variant, init.options), inProgress = true))).unsafeRunSync()
		// A game history can arrive before notes. It must not trigger a move yet.
		val legacyHistory = ujson.Obj("tableId" -> 3, "events" -> data("deal")("events"))
		client.handleMsg(wire.incoming(s"gameEventList $legacyHistory")).unsafeRunSync()
		assertEquals(queue.tryTake.unsafeRunSync(), None)
		receive("notes")
		val (noteCommand, note) = take()
		assertEquals(noteCommand, "note")
		assert(note.obj.contains("text") && !note.obj.contains("note"))
		assertEquals(take(), ("gameStateLoad", ujson.Obj("tableId" -> 3)))
		client.handleMsg(wire.incoming(s"gameEventList $legacyHistory")).unsafeRunSync()
		assertEquals(take(), ("loaded", ujson.Obj("tableId" -> 3)))
		def takeMove: IO[String] = queue.take.flatMap(value => if value.startsWith("action ") then IO.pure(value) else takeMove)
		val (command, move) = parts(WireProtocol.outgoing(takeMove.timeout(6.seconds).unsafeRunSync()))
		assertEquals(command, "playerAction")
		assertEquals(move.obj.keySet.toSet, Set("tableId", "action"))
		val action = move("action")
		assert(Set("play", "discard", "clue").contains(action("type").str))
		if action("type").str == "clue" then
			assert(Set("color", "rank").contains(action("clue")("type").str))
			assert(action.obj.contains("receiverIndex") && !action.obj.contains("target"))
		assertEquals(ref.get.unsafeRunSync().get.state.hands.map(_.size), Vector(5, 5))
		receive("timing")
		receive("playingList")
		def takeReattend: IO[String] = queue.take.flatMap(value => if value.startsWith("tableReattend ") then IO.pure(value) else takeReattend)
		assertEquals(parts(WireProtocol.outgoing(takeReattend.timeout(6.seconds).unsafeRunSync())), ("tableReattend", ujson.Obj("tableId" -> 3)))

	test("wrapped table lists handle an empty lobby and reject a malformed roster"):
		assertEquals(read[List[Table]](incoming("emptyTableList")._2), Nil)
		intercept[Exception](WireProtocol.incoming("tableList {\"tables\":{}}"))

	test("tagged game views preserve real seats and replay status"):
		for mode <- List("playing", "shadowing", "replay", "sharedReplay") do
			val init = read[InitMessage](incoming(s"init$mode")._2)
			assertEquals(init.ourPlayerIndex, 1)
			assertEquals(init.replay, mode == "replay" || mode == "sharedReplay")
		for name <- List("unseated", "unseatedreplay", "unseatedsharedReplay") do
			intercept[IllegalArgumentException](WireProtocol.incoming(frame(name)))
		val unknown = data("init")
		unknown("mode") = ujson.Obj("type" -> "unknown", "ourPlayerIndex" -> 0)
		intercept[IllegalArgumentException](WireProtocol.incoming(s"init $unknown"))

	test("current clue histories preserve the giving player as live events do"):
		val history = GameActionListMessage.fromJSON(incoming("clueHistory")._2)
		assertEquals(history.list, Seq(
			ClueAction(1, 0, Seq(0, 1), BaseClue(ClueKind.Colour, 1)),
			ClueAction(1, 0, Seq(0, 1), BaseClue(ClueKind.Rank, 1)),
			TurnAction(0, 0)))

	test("tableLeft and tableBoot clear the current table before leaving again"):
		for notification <- List("left", "boot") do
			val queue = std.Queue.unbounded[IO, String].unsafeRunSync()
			val ref = Ref.of[IO, Option[Game]](None).unsafeRunSync()
			val client = new BotClient(queue, ref)
			client.handleMsg(WireProtocol.incoming(frame("joined"))).unsafeRunSync()
			client.sendChat("before leaving").unsafeRunSync()
			assertEquals(parts(WireProtocol.outgoing(queue.take.unsafeRunSync())), ("chat", ujson.Obj("text" -> "before leaving", "tableId" -> 3)))
			client.handleMsg(WireProtocol.incoming(frame(notification))).unsafeRunSync()
			client.leaveRoom().unsafeRunSync()
			assertEquals(queue.tryTake.unsafeRunSync(), None)

	test("note lists restore the authenticated bot's own settings, not the first author's"):
		assertEquals(incoming("mixedNotes")._1, "noteListPlayer")
		assertEquals(read[NoteListPlayerMessage](incoming("mixedNotes")._2),
			NoteListPlayerMessage(3, Vector("[INFO: v1.4.1 (scala-bot), HGroup5]")))

	test("current discard and misplay histories preserve strikes and card removal"):
		import scala_bot.reactor.Reactor
		val variant = Variant(0, "No Variant", Vector("Red", "Yellow", "Green", "Blue", "Purple"))
		val initial = Reactor(3, State(Vector("TestBot", "Alice"), 0, variant, TableOptions(2, "No Variant")), inProgress = true)
		for (name, strikes) <- List(("discard", 0), ("misplay", 1)) do
			val game = GameActionListMessage.fromJSON(incoming("deal")._2).list.foldLeft(initial)((game, action) => game.handleAction(action))
			val value = data(name)
			val history = ujson.Obj("tableId" -> 3, "events" -> ujson.Arr(value("event")))
			val decoded = GameActionListMessage.fromJSON(parts(WireProtocol.incoming(s"gameEventList $history"))._2).list
			val seat = if strikes == 1 then 0 else 1
			assertEquals(decoded, Seq(DiscardAction(seat, if seat == 0 then 0 else 5, 0, if seat == 0 then 3 else 1, strikes == 1)))
			val updated = game.handleAction(decoded.head)
			assertEquals(updated.state.strikes, strikes)
			assertEquals(updated.state.hands(seat).size, 4)

	test("note ownership comes from welcome and is isolated between connections"):
		val alice = new WireProtocol.Decoder
		val welcome = data("welcome")
		welcome("username") = "Alice"
		val _ = alice.incoming(s"welcome $welcome")
		val bot = decoder()
		assertEquals(read[NoteListPlayerMessage](parts(alice.incoming(frame("mixedNotes")))._2).notes, Vector("other settings"))
		assertEquals(read[NoteListPlayerMessage](parts(bot.incoming(frame("mixedNotes")))._2).notes,
			Vector("[INFO: v1.4.1 (scala-bot), HGroup5]"))
		intercept[IllegalArgumentException](new WireProtocol.Decoder().incoming(frame("notes")))
		intercept[IllegalArgumentException](bot.incoming(frame("missingOwnNotes")))
		val duplicate = data("notes")
		duplicate("notes").arr += ujson.read(duplicate("notes")(0).toString)
		intercept[IllegalArgumentException](bot.incoming(s"noteList $duplicate"))

	test("restored notes select the bot's strategy before catch-up"):
		import scala_bot.reactor.Reactor
		import scala_bot.hgroup.HGroup
		val queue = std.Queue.unbounded[IO, String].unsafeRunSync()
		val variant = Variant(0, "No Variant", Vector("Red", "Yellow", "Green", "Blue", "Purple"))
		val game = Reactor(3, State(Vector("TestBot", "Alice"), 0, variant, TableOptions(2, "No Variant")), inProgress = true)
		val ref = Ref.of[IO, Option[Game]](Some(game)).unsafeRunSync()
		val client = new BotClient(queue, ref)
		client.handleMsg(decoder().incoming(frame("mixedNotes"))).unsafeRunSync()
		assert(ref.get.unsafeRunSync().get.isInstanceOf[HGroup])
		assertEquals(parts(WireProtocol.outgoing(queue.take.timeout(3.seconds).unsafeRunSync())),
			("gameStateLoad", ujson.Obj("tableId" -> 3)))

	test("tableRemoved removes stale join targets from the actual BotClient"):
		val queue = std.Queue.unbounded[IO, String].unsafeRunSync()
		val ref = Ref.of[IO, Option[Game]](None).unsafeRunSync()
		val client = new BotClient(queue, ref)
		val wire = new WireProtocol.Decoder
		for name <- List("welcome", "tableList", "tableGone", "chat") do
			client.handleMsg(wire.incoming(frame(name))).unsafeRunSync()
		val result = parts(WireProtocol.outgoing(queue.take.timeout(3.seconds).unsafeRunSync()))
		assertEquals(result._1, "chatPm")
		assertEquals(result._2("text").str, "Could not join, as you are not in a room.")

	test("replay notes prefer the bot's player settings over its spectator notes"):
		assertEquals(read[NoteListPlayerMessage](incoming("replayNotes")._2).notes,
			Vector("[INFO: v1.4.1 (scala-bot), HGroup5]"))
		assertEquals(read[NoteListPlayerMessage](incoming("spectatorNotes")._2).notes, Vector("spectator settings"))

	test("current lobby derives joined from the authenticated roster"):
		assert(!data("table").obj.contains("joined"))
		assert(!read[Table](incoming("table")._2).joined)
		assert(read[Table](incoming("playing")._2).joined)

	test("one bundled gameState restores settings and completes catch-up without another fetch"):
		import scala_bot.reactor.Reactor
		val queue = std.Queue.unbounded[IO, String].unsafeRunSync()
		val ref = Ref.of[IO, Option[Game]](None).unsafeRunSync()
		val client = new BotClient(queue, ref)
		val wire = new WireProtocol.Decoder
		client.handleFrame(frame("welcome"), wire.incoming).unsafeRunSync()
		client.handleFrame(frame("joined"), wire.incoming).unsafeRunSync()
		val variant = Variant(0, "No Variant", Vector("Red", "Yellow", "Green", "Blue", "Purple"))
		val game = Reactor(3, State(Vector("TestBot", "Alice"), 0, variant, TableOptions(2, "No Variant")), inProgress = true)
		ref.set(Some(game)).unsafeRunSync()
		client.handleFrame(frame("deal"), wire.incoming).unsafeRunSync()
		assertEquals(ref.get.unsafeRunSync().get.state.hands.map(_.size), Vector(5, 5))
		val first = queue.take.timeout(3.seconds).unsafeRunSync()
		val second = queue.take.timeout(3.seconds).unsafeRunSync()
		assert(first.startsWith("note "))
		assertEquals(parts(WireProtocol.outgoing(second)), ("loaded", ujson.Obj("tableId" -> 3)))
		def takeMove: IO[String] = queue.take.flatMap(frame => if frame.startsWith("action ") then IO.pure(frame) else takeMove)
		val move = WireProtocol.outgoing(takeMove.timeout(5.seconds).unsafeRunSync())
		val (command, payload) = parts(move)
		assertEquals(command, "playerAction")
		assertEquals(payload("tableId").num.toInt, 3)
		assert(Set("play", "discard", "clue").contains(payload("action")("type").str))
		if payload("action")("type").str == "clue" then
			assert(payload("action").obj.contains("receiverIndex") && !payload("action").obj.contains("target"))
		println(s"PROTOCOL_OUTGOING: $move")
		// Repeated snapshots rebuild from the initial state instead of applying the deal twice.
		client.handleFrame(frame("deal"), wire.incoming).unsafeRunSync()
		assertEquals(ref.get.unsafeRunSync().get.state.hands.map(_.size), Vector(5, 5))
		assertEquals(ref.get.unsafeRunSync().get.state.turnCount, 1)
		client.close.unsafeRunSync()

	test("current gameplay works both as a snapshot and as live events after rejoining"):
		import scala_bot.reactor.Reactor
		for bundled <- List(true, false) do
			val queue = std.Queue.unbounded[IO, String].unsafeRunSync()
			val ref = Ref.of[IO, Option[Game]](None).unsafeRunSync()
			val client = new BotClient(queue, ref)
			val wire = new WireProtocol.Decoder
			client.handleFrame(frame("welcome"), wire.incoming).unsafeRunSync()
			client.handleFrame(frame("joined"), wire.incoming).unsafeRunSync()
			val variant = Variant(0, "No Variant", Vector("Red", "Yellow", "Green", "Blue", "Purple"))
			ref.set(Some(Reactor(3, State(Vector("TestBot", "Alice"), 0, variant, TableOptions(2, "No Variant")), inProgress = true))).unsafeRunSync()
			val snapshot = data("fullGame")
			val events = snapshot("events").arr.toVector
			val split = events.indexWhere(e => e("type").str == "turn" && e("turn").num.toInt == 3) + 1
			if !bundled then snapshot("events") = ujson.Arr.from(events.take(split))
			client.handleFrame(s"gameState $snapshot", wire.incoming).unsafeRunSync()
			if !bundled then
				for event <- events.drop(split) do
					client.handleFrame(s"gameEvent ${ujson.Obj("tableId" -> 3, "event" -> event)}", wire.incoming).unsafeRunSync()
			val completed = ref.get.unsafeRunSync().get
			assertEquals(completed.state.playStacks.head, 2)
			assertEquals(completed.state.strikes, 1)
			assertEquals(completed.state.hands.map(_.size), Vector(5, 5))
			assert(!completed.inProgress)
			def drain: IO[List[String]] = queue.tryTake.flatMap:
				case Some(frame) => drain.map(frame :: _)
				case None => IO.pure(Nil)
			val messages = drain.unsafeRunSync()
			assertEquals(messages.count(_.startsWith("loaded ")), 1)
			assert(!messages.exists(_.startsWith("getGameInfo2 ")))
			assert(!messages.exists(_.startsWith("action ")))
			client.close.unsafeRunSync()

	test("a valid empty gameState acknowledges loading without fabricating an action"):
		import scala_bot.reactor.Reactor
		val queue = std.Queue.unbounded[IO, String].unsafeRunSync()
		val ref = Ref.of[IO, Option[Game]](None).unsafeRunSync()
		val client = new BotClient(queue, ref)
		val wire = new WireProtocol.Decoder
		client.handleFrame(frame("welcome"), wire.incoming).unsafeRunSync()
		client.handleFrame(frame("joined"), wire.incoming).unsafeRunSync()
		val variant = Variant(0, "No Variant", Vector("Red", "Yellow", "Green", "Blue", "Purple"))
		ref.set(Some(Reactor(3, State(Vector("TestBot", "Alice"), 0, variant, TableOptions(2, "No Variant")), inProgress = true))).unsafeRunSync()
		client.handleFrame(frame("emptyState"), wire.incoming).unsafeRunSync()
		assert(queue.take.timeout(3.seconds).unsafeRunSync().startsWith("note "))
		assertEquals(queue.take.timeout(3.seconds).unsafeRunSync(), "loaded {\"tableID\":3}")
		assertEquals(queue.tryTake.unsafeRunSync(), None)
		assertEquals(ref.get.unsafeRunSync().get.state.hands.map(_.size), Vector(0, 0))
		client.close.unsafeRunSync()
