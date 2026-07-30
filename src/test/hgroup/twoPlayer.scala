package tests.hgroup

import cats.effect.unsafe.implicits.global

import scala_bot.basics._
import scala_bot.test.{Player, preClue, setup}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.logger.{Logger,LogLevel}

class TwoPlayer extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("gives a double save"):
		val game = setup(HGroup.atLevel(1), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "r1", "b3", "b2", "y5")
		),
			clueTokens = 4,
			init = preClue(Bob, 2, Seq("1"))
		)

		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Rank(Bob.ordinal, 5))
