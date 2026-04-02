//> using scala 3.8.3
//> using jvm 25
//> using options -opt -Wall -Wconf:msg=toString:s -feature
//> using javaOpt -Xms128m -Xmx192m -Xss256k -XX:MaxMetaspaceSize=96m -XX:ReservedCodeCacheSize=64m -XX:+UseSerialGC
//> using dep com.softwaremill.sttp.client4::core:4.0.21
//> using dep com.softwaremill.sttp.client4::cats:4.0.21
//> using dep org.typelevel::cats-effect:3.7.0
//> using dep com.lihaoyi::upickle:4.4.3
//> using dep com.lihaoyi::requests:0.9.3
//> using dep org.scala-lang.modules::scala-parallel-collections:1.2.0
//> using test.dep org.scalameta::munit:1.2.4

package scala_bot

import cats.effect.{ExitCode, IO, IOApp, FiberIO}
import cats.effect.kernel.Ref
import cats.effect.std.Queue
import sttp.client4.*
import sttp.client4.httpclient.cats.HttpClientCatsBackend
import sttp.client4.ws.async.*
import sttp.ws.{WebSocket, WebSocketFrame}

import scala.io.Source._
import scala.concurrent.duration._
import scala_bot.basics.{Game,Variant}
import scala_bot.console.{ConsoleCmd, spawnConsole}
import scala.util.Try

case class WebSocketClosedException(code: Int, reason: String) extends Exception(s"WebSocket closed ($code): $reason")

def parseArgs(args: Seq[String]) =
	args.foldLeft(Map[String, String]()): (acc, arg) =>
		val parts = arg.split("=")

		if parts.length != 2 then
			throw new IllegalArgumentException(s"Invalid argument $arg")

		acc.updated(parts(0), parts(1))

def readEnv(args: Map[String, String]) =
	Try:
		val lines = fromFile("./.env").getLines()

		lines.foldLeft(args): (acc, line) =>
			val stripped = line.stripLeading()
			val withoutComment = stripped.takeWhile(_ != '#').stripTrailing()
			val withoutExport = withoutComment.stripPrefix("export ")
			withoutExport.split("=", 2) match
				case Array(key, value) => acc.updated(key.strip(), value.strip())
				case Array(rest) if rest.isBlank => acc
				case _ =>
					println(s"malformed line in .env: $line")
					acc
	.getOrElse:
		println(s"Failed to read .env file (maybe it doesn't exist?)")
		args

object main extends IOApp:
	def run(args: List[String]) =
		val parsedArgs = parseArgs(args)
		val env = sys.env ++ readEnv(parsedArgs)
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

							case WebSocketFrame.Close(code, reason) =>
								IO.raiseError(WebSocketClosedException(code, reason))

							case _ => loop

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
				client   = new BotClient(wsQueue, gameRef, BotConfig.fromEnv(env))(using runtime)
				console  <- spawnConsole(consoleQ, client)
				sender   <- startSender(ws, wsQueue)
				_        <- IO { Variant.init() }
				_        <- (console.join, receive(client)).parTupled.guaranteeCase(_ => sender.cancel)
			yield ()

		val index = parsedArgs.getOrElse("index", "0").toInt
		val username = env.lift(s"HANABI_USERNAME$index")
			.getOrElse(throw new IllegalStateException(s"Missing HANABI_USERNAME$index env variable!"))
		val password = env.lift(s"HANABI_PASSWORD$index")
			.getOrElse(throw new IllegalStateException(s"Missing HANABI_USERNAME$index env variable!"))

		val maxRetries = 5

		HttpClientCatsBackend.resource[IO]().use: backend =>
			Ref.of[IO, Boolean](false).flatMap: connectedRef =>
				def connect(attemptNum: Int = 0): IO[Unit] =
					def useWebSocketWithReset(ws: WebSocket[IO]): IO[Unit] =
						connectedRef.set(true) *> useWebSocket(ws)

					val attempt = for
						response <- basicRequest.post(uri"https://hanab.live:443/login")
							.header("Content-Type", "application/x-www-form-urlencoded")
							.body(Map("username" -> username, "password" -> password, "version" -> "bot"))
							.send(backend)
						cookie = response.headers.find(_.name == "set-cookie").map(_.value).getOrElse(throw new Exception(s"No cookie found ${response.headers}"))
						_ <- basicRequest
							.get(uri"wss://hanab.live/ws")
							.response(asWebSocket(useWebSocketWithReset))
							.header("Cookie", cookie)
							.send(backend)
					yield ()

					attempt.handleErrorWith:
						case err @ (_: sttp.client4.SttpClientException.ReadException | _: WebSocketClosedException) if attemptNum < maxRetries =>
							connectedRef.getAndSet(false).flatMap: wasConnected =>
								val nextAttemptNum = if wasConnected then 0 else attemptNum + 1
								IO.println(s"Connection lost (attempt $attemptNum/$maxRetries): ${err.getMessage}") *>
								IO.sleep((2 << attemptNum).seconds) *>
								connect(nextAttemptNum)
						case err =>
							IO(err.printStackTrace()) *> IO.raiseError(err)
				connect()
		.as(ExitCode.Success)
