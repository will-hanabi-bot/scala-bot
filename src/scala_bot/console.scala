package scala_bot.console

import cats.effect.std.{Console, Queue}
import cats.effect.IO
import scala_bot.BotClient

enum ConsoleCmd:
	case Hand(name: String, from: Option[String])
	case Navigate(arg: NavArg)

enum NavArg:
	case PrevRound, Prev, Next, NextRound
	case Turn(turn: Int)

def spawnConsole(queue: Queue[IO, ConsoleCmd], client: BotClient) =
	(spawnConsoleInput(queue), spawnConsoleHandler(queue, client)).parTupled.start

def spawnConsoleInput(queue: Queue[IO, ConsoleCmd]) =
	def loop: IO[Unit] = for {
		input <- Console[IO].readLine
		cmd <- input.split(" ") match {
			case Array("hand", name) => IO.pure(Some(ConsoleCmd.Hand(name, None)))
			case Array("hand", name, from) => IO.pure(Some(ConsoleCmd.Hand(name, Some(from))))
			case Array(nav, arg) if nav == "navigate" || nav == "nav" =>
				val navArg = arg match {
					case "++" => NavArg.NextRound
					case "+" => NavArg.Next
					case "--" => NavArg.PrevRound
					case "-" => NavArg.Prev
					case x => NavArg.Turn(x.toInt)
				}
				IO.pure(Some(ConsoleCmd.Navigate(navArg)))
			case _ =>
				IO.println("unknown command") *>
				IO.pure(None)
		}
		_ <- IO.whenA(cmd.nonEmpty)(queue.offer(cmd.get))
		_ <- loop
	} yield ()

	loop.handleErrorWith { err =>
		IO { err.printStackTrace() } *> IO.raiseError(err)
	}

def spawnConsoleHandler(queue: Queue[IO, ConsoleCmd], client: BotClient) =
	def loop: IO[Unit] = for {
		cmd <- queue.take
		_ <- client.debug(cmd)
		_ <- loop
	} yield ()

	loop.handleErrorWith { err =>
		IO { err.printStackTrace() } *> IO.raiseError(err)
	}
