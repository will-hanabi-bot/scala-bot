//> using scala 3.10.0
//> using jvm 25
//> using options -opt -Wall -Wconf:msg=toString:s -Yexplicit-nulls -feature
//> using javaOpt -Xms128m -Xmx192m -Xss256k -XX:MaxMetaspaceSize=96m -XX:ReservedCodeCacheSize=64m -XX:+UseSerialGC
//> using dep com.softwaremill.sttp.client4::core:4.1.1
//> using dep com.softwaremill.sttp.client4::cats:4.1.1
//> using dep org.typelevel::cats-effect:3.7.1
//> using dep com.lihaoyi::upickle:4.4.3
//> using dep com.lihaoyi::requests:0.9.3
//> using dep org.scala-lang.modules::scala-parallel-collections:1.3.0
//> using test.dep org.scalameta::munit:1.3.6

package scala_bot

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.kernel.Ref
import cats.effect.std.Queue
import sttp.client4.*
import sttp.client4.httpclient.cats.HttpClientCatsBackend
import sttp.client4.SttpClientException.{ConnectException, ReadException}
import sttp.client4.ws.async.*
import sttp.ws.{WebSocket, WebSocketFrame}

import scala.io.Source._
import scala.concurrent.duration._
import scala_bot.basics.{Game,Variant}
import scala_bot.console.{ConsoleCmd, spawnConsole}
import scala.util.Try
import java.io.IOException
import java.util.concurrent.TimeoutException

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
			if stripped.startsWith("#") then acc else
			stripped.split("=", 2) match
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

		val index = parsedArgs.getOrElse("index", "0").toInt
		val username = env.lift(s"HANABI_USERNAME$index")
			.getOrElse(throw new IllegalStateException(s"Missing HANABI_USERNAME$index env variable!"))
		val password = env.lift(s"HANABI_PASSWORD$index")
			.getOrElse(throw new IllegalStateException(s"Missing HANABI_USERNAME$index env variable!"))

		def useWebSocket(ws: WebSocket[IO]): IO[Unit] =
			def heartbeat: IO[Unit] =
				(ws.send(WebSocketFrame.Ping(Array.emptyByteArray)) *> IO.sleep(30.seconds)).foreverM

			def receive(client: BotClient, wire: WireProtocol.Decoder): IO[Unit] =
				Ref.of[IO, String]("").flatMap: buffer =>
					def loop: IO[Unit] =
						ws.receive().timeout(2.minutes).flatMap:
							case WebSocketFrame.Text(text, finalFrame, _) =>
								if !finalFrame then
									buffer.update(_ + text) *> loop
								else
									for
										acc <- buffer.getAndUpdate(_ => "")
										full = acc + text
										_   <- client.handleFrame(full, wire.incoming)
										_   <- loop
									yield ()

							case WebSocketFrame.Close(code, reason) =>
								IO.raiseError(WebSocketClosedException(code, reason))

							case WebSocketFrame.Pong(_) => loop

							case _ => loop

					loop

			def senderLoop(queue: Queue[IO, BotClient.Outgoing]): IO[Unit] =
				def loop: IO[Unit] =
					for
						msg <- queue.take
						_   <- msg.sendIfCurrent(frame => ws.send(WebSocketFrame.text(WireProtocol.outgoing(frame))))
						_   <- IO.sleep(500.millis)
						_   <- loop
					yield ()
				loop

			for
				_        <- IO.println("connected!")
				wsQueue  <- Queue.unbounded[IO, BotClient.Outgoing]
				consoleQ <- Queue.unbounded[IO, ConsoleCmd]
				gameRef  <- Ref.of[IO, Option[Game]](None)
				wire     = new WireProtocol.Decoder
				client   = new BotClient(wsQueue.offer, gameRef, BotConfig.fromEnv(env))(using runtime)
				console  <- spawnConsole(consoleQ, client, username)
				_        <- IO { Variant.init() }
				_        <- (console.join, receive(client, wire), senderLoop(wsQueue), heartbeat).parTupled.guarantee(client.close *> console.cancel)
			yield ()

		val maxRetries = 8

		val useSSL = env.getOrElse("SSL_DISABLED", "1") == "1"
		val HOSTNAME = env.getOrElse("HANABI_HOSTNAME", "hanab.live")
		val PORT = env.lift("HANABI_PORT") match
			case Some(port) => port.toInt
			case None => if useSSL then 443 else 80
		val CUSTOM_PORT = PORT != 443 && PORT != 80

		HttpClientCatsBackend.resource[IO]().use: backend =>
			Ref.of[IO, Boolean](false).flatMap: connectedRef =>
				def connect(attemptNum: Int = 0): IO[Unit] =
					def useWebSocketWithReset(ws: WebSocket[IO]): IO[Unit] =
						connectedRef.set(true) *> useWebSocket(ws)

					val attempt = for
						response <- LoginRetry.run:
							basicRequest.post(uri"${if useSSL then "https" else "http"}://${HOSTNAME}:${PORT}/login")
								.header("Content-Type", "application/x-www-form-urlencoded")
								.body(Map("username" -> username, "password" -> password, "client" -> "bot"))
								.send(backend)
								.flatMap(response => LoginRetry.check(response.code.code, response.header("Retry-After")).as(response))
						_ <- IO.whenA(response.cookies.forall(_.isLeft)):
								IO.raiseError(new Exception(s"No cookie found ${response.headers}"))
						_ <- basicRequest
							.get(uri"${if useSSL then "wss" else "ws"}://${HOSTNAME}${if CUSTOM_PORT then s":${PORT}" else ""}/ws?client=bot")
							.response(asWebSocket(useWebSocketWithReset))
							.cookies(response)
							.send(backend)
						_ <- response.body match
							case Left(body) => IO.raiseError(new IOException(s"WebSocket upgrade failed (${response.code}): $body"))
							case Right(_) => IO.unit
					yield ()

					attempt.handleErrorWith:
						case err @ (_: IOException | _: ReadException | _: WebSocketClosedException | _: ConnectException | _: TimeoutException | _: InternalError) if attemptNum < maxRetries =>
							connectedRef.getAndSet(false).flatMap: wasConnected =>
								val nextAttemptNum = if wasConnected then 0 else attemptNum + 1
								IO.println(s"Connection lost (attempt $attemptNum/$maxRetries): ${err.getMessage}") *>
								IO.sleep((2 << attemptNum).seconds) *>
								connect(nextAttemptNum)
						case err =>
							IO(err.printStackTrace()) *> IO.raiseError(err)
				connect()
		.onCancel(IO.print("\n"))
		.as(ExitCode.Success)
