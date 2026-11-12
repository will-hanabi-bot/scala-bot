package tests.protocol

import scala_bot.*

import cats.effect.{IO, Ref, std}
import cats.effect.unsafe.implicits.global
import scala.concurrent.duration.*
import scala_bot.basics.*
import scala_bot.reactor.Reactor

class GameIsolationSuite extends munit.FunSuite:
	private def game(id: Int): Reactor =
		val variant = Variant(0, "No Variant", Vector("Red", "Yellow", "Green", "Blue", "Purple"))
		val initial = Reactor(id, State(Vector("TestBot", "Alice"), 0, variant, TableOptions(2, "No Variant")), inProgress = true)
		(0 until 10).foldLeft(initial): (game, order) =>
			game.handleAction(if order < 5 then DrawAction(0, order, -1, -1) else DrawAction(1, order, order - 5, 1))

	private case class Harness(queue: std.Queue[IO, String], ref: Ref[IO, Option[Game]], client: BotClient):
		def receive(command: String, id: Int): Unit = client.handleMsg(s"$command {\"tableID\":$id}").unsafeRunSync()
		def seed(id: Int): Unit =
			receive("joined", id)
			ref.set(Some(game(id))).unsafeRunSync()
		def drain(): List[String] =
			def loop: IO[List[String]] = queue.tryTake.flatMap:
				case Some(frame) => loop.map(frame :: _)
				case None => IO.pure(Nil)
			loop.unsafeRunSync()
		def ownTurn(id: Int): Unit = client.handleAction(GameActionMessage(id, TurnAction(0, 0))).unsafeRunSync()

	private def harness(): Harness =
		val queue = std.Queue.unbounded[IO, String].unsafeRunSync()
		val ref = Ref.of[IO, Option[Game]](None).unsafeRunSync()
		Harness(queue, ref, new BotClient(queue, ref))

	test("foreign live events cannot alter the active game's turn count"):
		val h = harness()
		h.seed(9216)
		val before = h.ref.get.unsafeRunSync()
		h.client.handleAction(GameActionMessage(9208, TurnAction(49, -1))).unsafeRunSync()
		assertEquals(h.ref.get.unsafeRunSync(), before)
		assertEquals(h.drain(), Nil)

	test("foreign histories cannot replace active state or send loaded for another table"):
		val h = harness()
		h.seed(9216)
		h.client.handleMsg("""noteListPlayer {"tableID":9216,"notes":[]}""").unsafeRunSync()
		h.drain()
		val before = h.ref.get.unsafeRunSync()
		h.client.handleMsg("""gameActionList {"tableID":9208,"list":[{"type":"turn","num":49}]}""").unsafeRunSync()
		assertEquals(h.ref.get.unsafeRunSync(), before)
		assertEquals(h.drain(), Nil)

	test("foreign notes cannot restore settings into the active game"):
		val h = harness()
		h.seed(9216)
		val before = h.ref.get.unsafeRunSync()
		h.client.handleMsg("""noteListPlayer {"tableID":9208,"notes":["[INFO: v1.4.1 (scala-bot), HGroup5]"]}""").unsafeRunSync()
		assertEquals(h.ref.get.unsafeRunSync(), before)
		assertEquals(h.drain(), Nil)

	test("joining another table clears the previous game immediately"):
		val h = harness()
		h.seed(9208)
		h.client.sendCmd("tableJoin", """{"tableID":9216}""").unsafeRunSync()
		h.drain()
		h.receive("joined", 9216)
		assertEquals(h.ref.get.unsafeRunSync(), None)

	test("late lifecycle messages from another table do not clear or restart the current game"):
		val h = harness()
		h.seed(9216)
		val before = h.ref.get.unsafeRunSync()
		h.receive("left", 9208)
		h.receive("tableStart", 9208)
		h.client.handleMsg("""init {"tableID":9208}""").unsafeRunSync()
		assertEquals(h.ref.get.unsafeRunSync(), before)
		assertEquals(h.drain(), Nil)
		h.client.sendChat("still here").unsafeRunSync()
		assert(h.drain().head.contains("table9216"))

	test("a delayed move cannot be retargeted when the bot joins another table"):
		val h = harness()
		h.seed(9208)
		h.ownTurn(9208)
		h.client.sendCmd("tableJoin", """{"tableID":9216}""").unsafeRunSync()
		h.receive("joined", 9216)
		IO.sleep(2300.millis).unsafeRunSync()
		assert(!h.drain().exists(_.startsWith("action ")))

	test("leaving and rejoining the same table invalidates the old move"):
		val h = harness()
		h.seed(9216)
		h.ownTurn(9216)
		h.receive("left", 9216)
		h.seed(9216)
		IO.sleep(2300.millis).unsafeRunSync()
		assert(!h.drain().exists(_.startsWith("action ")))

	test("a completed turn cancels the previously computed move"):
		val h = harness()
		h.seed(9216)
		h.ownTurn(9216)
		h.client.handleAction(GameActionMessage(9216, TurnAction(1, 1))).unsafeRunSync()
		IO.sleep(2300.millis).unsafeRunSync()
		assert(!h.drain().exists(_.startsWith("action ")))

	test("a current game still computes and queues a move with its own table ID"):
		val h = harness()
		h.seed(9216)
		h.ownTurn(9216)
		def takeAction: IO[String] = h.queue.take.flatMap(frame => if frame.startsWith("action ") then IO.pure(frame) else takeAction)
		val frame = takeAction.timeout(5.seconds).unsafeRunSync()
		assertEquals(ujson.read(frame.split(" ", 2)(1))("tableID").num.toInt, 9216)

	test("foreign wire frames are ignored before their game payload is decoded"):
		val h = harness()
		h.seed(9216)
		h.client.handleFrame("""gameEvent {"tableId":9208,"event":{}}""", _ => throw new IllegalStateException("foreign decoder ran")).unsafeRunSync()
		assertEquals(h.ref.get.unsafeRunSync().get.tableID, 9216)

	test("already queued commands are checked again at the actual send boundary"):
		val outbox = std.Queue.unbounded[IO, BotClient.Outgoing].unsafeRunSync()
		val sent = Ref.of[IO, List[String]](Nil).unsafeRunSync()
		val ref = Ref.of[IO, Option[Game]](None).unsafeRunSync()
		val client = new BotClient(outbox.offer, ref)
		client.handleMsg("""joined {"tableID":9216}""").unsafeRunSync()
		client.sendCmd("note", """{"tableID":9216,"order":0,"note":"old"}""").unsafeRunSync()
		val queued = outbox.take.unsafeRunSync()
		client.handleMsg("""left {"tableID":9216}""").unsafeRunSync()
		client.handleMsg("""joined {"tableID":9216}""").unsafeRunSync()
		queued.sendIfCurrent(frame => sent.update(_ :+ frame)).unsafeRunSync()
		assertEquals(sent.get.unsafeRunSync(), Nil)
		client.sendCmd("note", """{"tableID":9216,"order":0,"note":"new"}""").unsafeRunSync()
		outbox.take.flatMap(_.sendIfCurrent(frame => sent.update(_ :+ frame))).unsafeRunSync()
		assertEquals(sent.get.unsafeRunSync().size, 1)

	test("connection shutdown invalidates queued game commands"):
		val outbox = std.Queue.unbounded[IO, BotClient.Outgoing].unsafeRunSync()
		val ref = Ref.of[IO, Option[Game]](None).unsafeRunSync()
		val client = new BotClient(outbox.offer, ref)
		client.handleMsg("""joined {"tableID":9216}""").unsafeRunSync()
		client.sendCmd("loaded", """{"tableID":9216}""").unsafeRunSync()
		val queued = outbox.take.unsafeRunSync()
		client.close.unsafeRunSync()
		queued.sendIfCurrent(_ => IO.raiseError(new IllegalStateException("stale send"))).unsafeRunSync()
		assertEquals(ref.get.unsafeRunSync(), None)

	test("a late join confirmation cannot replace a newer requested table"):
		val h = harness()
		h.seed(9208)
		h.client.sendCmd("tableJoin", """{"tableID":9216}""").unsafeRunSync()
		h.receive("joined", 9216)
		h.ref.set(Some(game(9216))).unsafeRunSync()
		h.receive("joined", 9208)
		assertEquals(h.ref.get.unsafeRunSync().get.tableID, 9216)

	test("a duplicate join confirmation does not discard the current game"):
		val h = harness()
		h.seed(9216)
		val before = h.ref.get.unsafeRunSync()
		h.receive("joined", 9216)
		assertEquals(h.ref.get.unsafeRunSync(), before)

	test("leaving cancels an invitation that has not reached the wire yet"):
		val outbox = std.Queue.unbounded[IO, BotClient.Outgoing].unsafeRunSync()
		val ref = Ref.of[IO, Option[Game]](None).unsafeRunSync()
		val client = new BotClient(outbox.offer, ref)
		client.sendCmd("tableJoin", """{"tableID":9216}""").unsafeRunSync()
		val queued = outbox.take.unsafeRunSync()
		client.leaveRoom().unsafeRunSync()
		queued.sendIfCurrent(_ => IO.raiseError(new IllegalStateException("cancelled invitation sent"))).unsafeRunSync()

	test("a newer invitation supersedes an older queued join"):
		val outbox = std.Queue.unbounded[IO, BotClient.Outgoing].unsafeRunSync()
		val sent = Ref.of[IO, List[String]](Nil).unsafeRunSync()
		val ref = Ref.of[IO, Option[Game]](None).unsafeRunSync()
		val client = new BotClient(outbox.offer, ref)
		client.sendCmd("tableJoin", """{"tableID":9208}""").unsafeRunSync()
		client.sendCmd("tableJoin", """{"tableID":9216}""").unsafeRunSync()
		for _ <- 0 until 2 do
			outbox.take.flatMap(_.sendIfCurrent(frame => sent.update(_ :+ frame))).unsafeRunSync()
		assertEquals(sent.get.unsafeRunSync(), List("""tableJoin {"tableID":9216}"""))

	test("commands from another table's chat cannot leave the current game"):
		val h = harness()
		h.seed(9216)
		val before = h.ref.get.unsafeRunSync()
		h.client.handleMsg("""chat {"msg":"/leaveall","who":"Alice","room":"table9208"}""").unsafeRunSync()
		assertEquals(h.ref.get.unsafeRunSync(), before)
		assertEquals(h.drain(), Nil)

	test("an already queued move is discarded if the turn has advanced"):
		val outbox = std.Queue.unbounded[IO, BotClient.Outgoing].unsafeRunSync()
		val ref = Ref.of[IO, Option[Game]](None).unsafeRunSync()
		val client = new BotClient(outbox.offer, ref)
		client.handleMsg("""joined {"tableID":9216}""").unsafeRunSync()
		ref.set(Some(game(9216))).unsafeRunSync()
		client.handleAction(GameActionMessage(9216, TurnAction(0, 0))).unsafeRunSync()
		def takeAction: IO[BotClient.Outgoing] = outbox.take.flatMap(command => if command.frame.startsWith("action ") then IO.pure(command) else takeAction)
		val queued = takeAction.timeout(5.seconds).unsafeRunSync()
		client.handleAction(GameActionMessage(9216, TurnAction(1, 1))).unsafeRunSync()
		queued.sendIfCurrent(_ => IO.raiseError(new IllegalStateException("stale move sent"))).unsafeRunSync()

	private def invite(h: Harness): Unit =
		h.client.handleMsg("""welcome {"userID":9,"username":"TestBot","playingAtTables":[]}""").unsafeRunSync()
		h.client.handleMsg("""table {"id":9216,"name":"New game","passwordProtected":false,"joined":false,"running":false,"variant":"No Variant","sharedReplay":false,"progress":0,"players":["Alice"],"spectators":[],"maxPlayers":5}""").unsafeRunSync()
		h.client.handleChat(ChatMessage("/join", "Alice", "lobby", "TestBot")).unsafeRunSync()

	test("an invitation to another table cannot steal a bot from an ongoing game"):
		val h = harness()
		h.seed(9208)
		invite(h)
		val messages = h.drain()
		assert(!messages.exists(_.startsWith("tableJoin ")))
		assert(messages.exists(frame => frame.startsWith("chatPM ") && frame.contains("Already playing")))
		assertEquals(h.ref.get.unsafeRunSync().get.tableID, 9208)

	test("switching pregame tables leaves the old lobby before joining the new one"):
		val h = harness()
		h.receive("joined", 9208)
		invite(h)
		assertEquals(h.drain(), List("""tableLeave {"tableID":9208}""", """tableJoin {"tableID":9216}"""))
