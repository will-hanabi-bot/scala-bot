package tests.hgroup

import cats.effect.unsafe.implicits.global

import scala_bot.{GameData, processGame}
import scala_bot.basics._
import scala_bot.hgroup.HGroup
import scala_bot.logger.{Logger, LogLevel}

class WaitingConnections extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	// HGroup4, 4 players, before michiel-bot-2's 12th action (hanab.live game 2063608). Its clued 1 (order 20) is r1 or g1, and the 3 clue
	// on order 13 reads as r3 (blind-playing r1 and r2) or as b3 (prompting b2). Playing the 1 rules out the b3 reading, which is not a mistake.
	test("playing a clued 1 that rules out another reading of a clue is not a mistake"):
		val deck = ujson.read("""[[1,4],[1,1],[1,4],[3,3],[3,1],[4,4],[3,2],[4,1],[1,1],[4,1],[2,4],[3,4],[4,4],[0,3],[4,3],[1,2],[0,3],[3,5],[0,2],[2,5],[2,1],[0,1],[4,5],[0,1],[4,2],[2,2],[0,2],[1,5],[0,4],[1,2],[4,3],[3,2],[4,2],[0,5],[2,1],[4,1],[3,4],[3,1],[0,4],[1,1],[0,1],[2,4],[1,3],[1,3],[2,3],[2,1],[3,3],[3,1],[2,3],[2,2]]""").arr.map(c => ujson.Obj("suitIndex" -> c(0), "rank" -> c(1)))
		val actions = ujson.read("""[[3,2,1],[2,3,1],[0,9,0],[2,1,3],[1,0,0],[0,4,0],[0,8,0],[0,15,0],[3,3,1],[1,5,0],[3,3,3]]""").arr.map(a => ujson.Obj("type" -> a(0), "target" -> a(1), "value" -> a(2)))
		val data = GameData.parse(ujson.Obj(
			"players" -> ujson.Arr("MichielBaird", "michiel-bot-1", "michiel-bot-3", "michiel-bot-2"),
			"deck" -> ujson.Arr.from(deck),
			"actions" -> ujson.Arr.from(actions)
		))

		Variant.init()
		val state = State(data.players, 3, Variant.getVariant(data.options.variantName), data.options)
		val game = processGame(HGroup(0, state, false, 4).copy(catchup = true), data, 3)

		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Play(20))
