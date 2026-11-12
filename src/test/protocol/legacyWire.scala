package tests.protocol

import scala_bot.*

import upickle.default.read

class LegacyWireSuite extends munit.FunSuite:
	// Synthetic wire payloads exercise the same readers used by BotClient.
	test("welcome without the removed randomTableName field still identifies the bot"):
		val welcome = read[SelfData](
			"""{"userID":123,"username":"TestBot","playingAtTables":[]}"""
		)
		assertEquals(welcome.userID, 123)
		assertEquals(welcome.username, "TestBot")
		assertEquals(welcome.playingAtTables, List.empty[Int])

	test("legacy welcome with randomTableName remains readable"):
		val welcome = read[SelfData](
			"""{"userID":123,"username":"TestBot","playingAtTables":[42],"randomTableName":"Example table"}"""
		)
		assertEquals(welcome.playingAtTables, List(42))

	test("public chat history can omit recipient"):
		val history = read[ChatList](
			"""{"list":[{"msg":"hello","who":"Player","room":"lobby"}]}"""
		)
		assertEquals(history.list, Vector(ChatMessage("hello", "Player", "lobby", "")))

	test("system chat history can omit sender and room as well as recipient"):
		val history = read[ChatList]("""{"list":[{"msg":"Server restarted"}]}""")
		assertEquals(history.list, Vector(ChatMessage("Server restarted", "", "", "")))

	test("private chat history preserves an explicit recipient"):
		val history = read[ChatList](
			"""{"list":[{"msg":"ready","who":"Player","room":"lobby","recipient":"TestBot"}]}"""
		)
		assertEquals(history.list, Vector(ChatMessage("ready", "Player", "lobby", "TestBot")))

	test("legacy lobby table accepts absent options, spectator names and null maxPlayers"):
		val table = read[Table](
			"""{
				|  "id":42,"name":"Test table","passwordProtected":false,
				|  "joined":false,"running":false,"variant":"No Variant",
				|  "sharedReplay":false,"progress":0,"players":["Player"],
				|  "spectators":["Observer"],"maxPlayers":null
				|}""".stripMargin
		)
		assertEquals(table.id, 42)
		assertEquals(table.players, Vector("Player"))
		assertEquals(table.spectators, List(Spectator("Observer", -1, "")))
		assertEquals(table.maxPlayers, 6)

	test("lobby table list accepts spectators without a shadowing username"):
		val tables = read[Vector[Table]](
			"""[{
				|  "id":42,"name":"Test table","passwordProtected":false,
				|  "joined":false,"running":true,"variant":"No Variant",
				|  "sharedReplay":false,"progress":10,"players":["Player"],
				|  "spectators":[
				|    {"name":"Observer","shadowingPlayerIndex":-1},
				|    {"name":"Shadow","shadowingPlayerIndex":0,"shadowingPlayerUsername":"Player"}
				|  ],"maxPlayers":5
				|}]""".stripMargin
		)
		assertEquals(tables.head.spectators, List(
			Spectator("Observer", -1, ""),
			Spectator("Shadow", 0, "Player")
		))

	test("legacy lobby table preserves structured spectator data"):
		val table = read[Table](
			"""{
				|  "id":42,"name":"Test table","passwordProtected":false,
				|  "joined":true,"running":true,"variant":"No Variant",
				|  "options":{"numPlayers":2,"variantName":"No Variant"},
				|  "sharedReplay":false,"progress":10,"players":["Player","TestBot"],
				|  "spectators":[{"name":"Observer","shadowingPlayerIndex":0,"shadowingPlayerUsername":"Player"}],
				|  "maxPlayers":5
				|}""".stripMargin
		)
		assertEquals(table.players, Vector("Player", "TestBot"))
		assertEquals(table.spectators, List(Spectator("Observer", 0, "Player")))
		assertEquals(table.maxPlayers, 5)
