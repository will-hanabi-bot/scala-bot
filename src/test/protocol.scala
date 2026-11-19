package tests

import scala_bot.{BotClient, ConfigState, SessionState, Table, TablePhase}
import scala_bot.basics.Game
import scala_bot.logger.{Logger, LogLevel}

import cats.effect.IO
import cats.effect.kernel.Ref
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global

class Protocol extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	def table(id: Int, phase: String, players: String) =
		s"""{"tableId": $id, "owner": {"userId": 7, "username": "Alice"}, "passwordProtected": false,
			"playerNames": $players, "phase": $phase, "spectators": [{"userId": 9, "username": "Carol"}],
			"settings": {"name": "Alice's game", "maxPlayers": 5, "options": {}}}"""

	def welcome(tables: String, playingAtTables: String) =
		s"""{"userId": 5, "username": "bot1", "muted": false, "firstTimeUser": false, "friends": [], "blocked": [],
			"playingAtTables": $playingAtTables, "spectatingAtTables": [],
			"users": [
				{"userId": 5, "username": "bot1", "activity": {"type": "lobby"}, "hGroupConventions": false, "bot": true, "inactive": false},
				{"userId": 7, "username": "Alice", "activity": {"type": "lobby"}, "hGroupConventions": false, "bot": false, "inactive": false}
			],
			"tables": $tables, "settings": {}}"""

	/** Runs the client on the given events, and returns the requests it sent. */
	def run(events: List[(String, String)], pms: List[(String, String)] = Nil) =
		val io = for
			queue      <- Queue.unbounded[IO, String]
			gameRef    <- Ref.of[IO, Option[Game]](None)
			sessionRef <- Ref.of[IO, SessionState](SessionState())
			configRef  <- Ref.of[IO, ConfigState](ConfigState())
			client = BotClient(queue, gameRef, sessionRef, configRef)(using global)
			_          <- events.foldLeft(IO.unit): (acc, ev) =>
				acc *> client.handleMsg(s"""{"event": "${ev._1}", "data": ${ev._2}}""")
			_          <- pms.foldLeft(IO.unit): (acc, pm) =>
				acc *> client.handlePM(pm._1, pm._2)
			sent       <- queue.tryTakeN(None)
			session    <- sessionRef.get
		yield (sent.map(ujson.read(_)), session)

		io.unsafeRunSync()

	test("reads a shared replay of a game that is not saved"):
		val t = upickle.read[Table](table(4, """{"type": "sharedReplay", "progress": 50}""", """["Alice", "Bob"]"""))
		assertEquals(t.phase, TablePhase.SharedReplay(None))
		assert(t.phase.isSharedReplay)

	test("welcome replaces the tables and the bots, and reattends a game"):
		val tables = s"[${table(3, """{"type": "playing", "progress": 10}""", """["Alice", "bot1"]""")}]"
		val (sent, session) = run(List("welcome" -> welcome(tables, "[3]")))

		assertEquals(session.info.map(_.username), Some("bot1"))
		assertEquals(session.tables.keySet, Set(3))
		assertEquals(session.bots, Set("bot1"))
		assertEquals(sent.map(_("request").str), List("tableReattend"))
		assertEquals(sent.head("data")("tableId").num.toInt, 3)

	test("starts a table with the seated players"):
		val pregame = """{"tableId": 3, "owner": {"userId": 5, "username": "bot1"}, "passwordProtected": false,
			"players": [
				{"userId": 7, "username": "Alice", "present": true, "stats": {"numGames": 0, "variant": {"numGames": 0, "bestScores": []}}},
				{"userId": 5, "username": "bot1", "present": true, "stats": {"numGames": 0, "variant": {"numGames": 0, "bestScores": []}}}
			],
			"settings": {"name": "Alice's game", "maxPlayers": 5, "options": {}}}"""

		val (sent, _) = run(
			List(
				"welcome" -> welcome("[]", "[]"),
				"tableJoined" -> """{"tableId": 3, "stage": "pregame"}""",
				"pregame" -> pregame
			),
			pms = List("Alice" -> "/start")
		)

		assertEquals(sent.map(_("request").str), List("tableStart"))
		assertEquals(sent.head("data"), ujson.Obj("tableId" -> 3, "intendedPlayerIds" -> ujson.Arr(7, 5)))
