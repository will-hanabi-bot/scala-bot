//> using scala 3.7.2
//> using jvm 25
//> using options -Wall -Wconf:msg=toString:s -feature -language:implicitConversions
//> using dep com.softwaremill.sttp.client4::core:4.0.13
//> using dep com.softwaremill.sttp.client4::cats:4.0.13
//> using dep org.typelevel::cats-effect:3.6.3
//> using dep com.lihaoyi::upickle:4.4.1
//> using dep com.lihaoyi::requests:0.9.0
// > using dep org.scala-lang.modules::scala-parallel-collections:1.2.0
//> using test.dep org.scalameta::munit:1.2.1

package scala_bot

import cats.effect.{IO, FiberIO}
import cats.effect.kernel.Ref
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import sttp.client4.*
import sttp.client4.httpclient.cats.HttpClientCatsBackend
import sttp.client4.ws.async.*
import sttp.ws.{WebSocket, WebSocketFrame}

import scala.io.Source._
import scala.concurrent.duration._
import scala_bot.basics.Variant
import scala_bot.basics.Game
import scala_bot.console.{ConsoleCmd, spawnConsole}
import scala.util.Try

def parseArgs(args: Seq[String]) =
	args.foldLeft(Map[String, String]()): (acc, arg) =>
		val parts = arg.split("=")

		if parts.length != 2 then
			throw new IllegalArgumentException(s"Invalid argument $arg")

		acc.updated(parts(0), parts(1))

def readEnv(args: Map[String, String]) =
	Try:
		val lines = fromFile("./.env").getLines

		lines.foldLeft(args): (acc, line) =>
			line.split("=") match
				case Array(key, value) => acc.updated(key, value)
				case Array("") => acc
				case _ =>
					println(s"malformed line in .env: $line")
					acc
	.getOrElse:
		println(s"Failed to read .env file (maybe it doesn't exist?)")
		args

@main
def main(args: String*): Unit =
	def useWebSocket(ws: WebSocket[IO]): IO[Unit] =
		def receive(client: BotClient): IO[Unit] =
			Ref.of[IO, String]("").flatMap: buffer =>
				def loop: IO[Unit] =
					ws.receive().flatMap:
						case WebSocketFrame.Text(text, finalFrame, _) =>
							if !finalFrame then
								buffer.update(_ + text) *> loop
							else
								for
									acc <- buffer.getAndUpdate(_ => "")
									full = acc + text
									_   <- client.handleMsg(full)
									_   <- loop
								yield ()

						case WebSocketFrame.Close(_, _) => IO.println("Connection closed")

						case _ => loop
					.handleErrorWith: err =>
						IO(err.printStackTrace()) *> IO.raiseError(err)

				loop

		def startSender(ws: WebSocket[IO], queue: Queue[IO, String]): IO[FiberIO[Unit]] =
			def loop: IO[Unit] =
				for
					msg <- queue.take
					_   <- ws.send(WebSocketFrame.text(msg))
					_   <- IO.sleep(500.millis)
					_   <- loop
				yield ()
			loop.start

		for
			_        <- IO.println("connected!")
			wsQueue  <- Queue.unbounded[IO, String]
			consoleQ <- Queue.unbounded[IO, ConsoleCmd]
			gameRef  <- Ref.of[IO, Option[Game]](None)
			client   = new BotClient(wsQueue, gameRef)
			console  <- spawnConsole(consoleQ, client)
			_        <- startSender(ws, wsQueue)
			_        <- IO { Variant.init() }
			_        <- (console.join, receive(client)).parTupled
		yield ()

	val parsedArgs = parseArgs(args)
	val env = readEnv(parsedArgs)

	val index = parsedArgs.getOrElse("index", "0").toInt
	val username = env.lift(s"HANABI_USERNAME$index")
		.getOrElse(throw new IllegalStateException(s"Missing HANABI_USERNAME$index env variable!"))
	val password = env.lift(s"HANABI_PASSWORD$index")
		.getOrElse(throw new IllegalStateException(s"Missing HANABI_USERNAME$index env variable!"))

	val program = HttpClientCatsBackend.resource[IO]().use: backend =>
		for
			response <- basicRequest.post(uri"https://hanab.live:443/login")
				.header("Content-Type", "application/x-www-form-urlencoded")
				.body(Map("username" -> username, "password" -> password, "version" -> "bot"))
				.send(backend)
			cookie = response.headers.find(_.name == "set-cookie").map(_.value).getOrElse(throw new Exception(s"No cookie found ${response.headers}"))
			_ <- basicRequest
				.get(uri"wss://hanab.live/ws")
				.response(asWebSocket(useWebSocket))
				.header("Cookie", cookie)
				.send(backend)
		yield ()
	.handleErrorWith: err =>
		IO(err.printStackTrace()) *> IO.raiseError(err)

	program.unsafeRunSync()
