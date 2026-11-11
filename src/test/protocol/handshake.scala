package tests.protocol

import java.io.{BufferedReader, EOFException, File, InputStreamReader}
import java.net.{InetAddress, ServerSocket, SocketException, URI, URLDecoder}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.concurrent.{CompletableFuture, TimeUnit}
import scala.util.control.NonFatal

class WebSocketHandshakeSuite extends munit.FunSuite:
	private class LoginServer:
		private val listener = new ServerSocket(0, 50, InetAddress.getByName("127.0.0.1"))
		val port = listener.getLocalPort
		val handshake = new CompletableFuture[String]()
		val login = new CompletableFuture[Map[String, String]]()
		@volatile var cookieHeader = ""
		private val worker = new Thread(() =>
			try
				while !listener.isClosed && !handshake.isDone do
					val socket = listener.accept().nn
					socket.setSoTimeout(2000)
					try
						val reader = new BufferedReader(new InputStreamReader(socket.getInputStream, StandardCharsets.US_ASCII))
						val request = Option(reader.readLine()).getOrElse(throw new EOFException("Missing request line"))
						val parts = request.split(" ")
						var line = reader.readLine()
						var contentLength = 0
						while line != null && line != "" do
							if line.nn.toLowerCase(java.util.Locale.ROOT).startsWith("content-length:") then
								contentLength = line.nn.substring("content-length:".length).trim.toInt
							if line.nn.toLowerCase(java.util.Locale.ROOT).startsWith("cookie:") then
								cookieHeader = line.nn.substring("cookie:".length).trim
							line = reader.readLine()
						val body = new StringBuilder
						for _ <- 0 until contentLength do
							val char = reader.read()
							if char < 0 then throw new EOFException("Incomplete request body")
							body.append(char.toChar)

						val response =
							if parts(0) == "POST" && parts(1) == "/login" then
								val form = body.toString.split("&").map: field =>
									val pair = field.split("=", 2)
									URLDecoder.decode(pair(0), StandardCharsets.UTF_8).nn -> URLDecoder.decode(pair(1), StandardCharsets.UTF_8).nn
								.toMap
								val _ = login.complete(form)
								if form.get("client").contains("bot") && form.keySet == Set("username", "password", "client") then
									"HTTP/1.1 200 OK\r\nSet-Cookie: session=test-only; Path=/; HttpOnly; SameSite=Lax\r\nSet-Cookie: second=other; Path=/\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
								else
									"HTTP/1.1 401 Unauthorized\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
							else
								// Refuse the upgrade: no bot session, variant fetch or game action runs.
								val _ = handshake.complete(parts(1))
								"HTTP/1.1 400 Bad Request\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"
						socket.getOutputStream.nn.write(response.getBytes(StandardCharsets.US_ASCII))
					finally socket.close()
			catch
				case _: SocketException if listener.isClosed => ()
				case NonFatal(error) =>
					val _ = handshake.completeExceptionally(error)
		, "hanabi-handshake-test")
		worker.setDaemon(true)
		worker.start()

		def close(): Unit =
			listener.close()
			worker.join(2500)

	test("bot identifies itself on the login form and actual WebSocket upgrade"):
		val server = new LoginServer
		// A fresh working directory prevents a real .env from overriding loopback credentials.
		val directory = Files.createTempDirectory("hanabi-handshake-").nn
		val classPath = System.getProperty("java.class.path").nn.split(File.pathSeparator)
			.map(path => new File(path).getAbsolutePath.nn).mkString(File.pathSeparator)
		val args = List(
			new File(System.getProperty("java.home"), "bin/java").getAbsolutePath.nn,
			"-Xms128m", "-Xmx192m", "-Xss256k", "-XX:MaxMetaspaceSize=96m",
			"-XX:ReservedCodeCacheSize=64m", "-XX:+UseSerialGC",
			"-cp", classPath, "scala_bot.main", "index=1",
			"HANABI_USERNAME1=protocol-test-only", "HANABI_PASSWORD1=not-a-real-password",
			"HANABI_HOSTNAME=127.0.0.1", s"HANABI_PORT=${server.port}",
			"SSL_DISABLED=0"
		)
		var process: Option[Process] = None
		try
			val builder = new ProcessBuilder(args*)
				.directory(directory.toFile)
				.redirectOutput(ProcessBuilder.Redirect.DISCARD)
				.redirectError(ProcessBuilder.Redirect.DISCARD)
			// A larger test-runner metadata limit must not change the bot's limits.
			val _ = builder.environment().nn.remove("_JAVA_OPTIONS")
			process = Some(builder.start().nn)
			val form = server.login.get(7, TimeUnit.SECONDS).nn
			assertEquals(form, Map("username" -> "protocol-test-only", "password" -> "not-a-real-password", "client" -> "bot"))
			val uri = new URI(server.handshake.get(7, TimeUnit.SECONDS).nn)
			assertEquals(uri.getPath, "/ws")
			assertEquals(server.cookieHeader.split("; ").toSet, Set("session=test-only", "second=other"))
			assertEquals(Option(uri.getRawQuery).map(_.split("&").toSet), Some(Set("client=bot")))
		finally
			process.foreach: child =>
				child.destroy()
				if !child.waitFor(2, TimeUnit.SECONDS) then
					val _ = child.destroyForcibly()
					val _ = child.waitFor(2, TimeUnit.SECONDS)
			server.close()
			val _ = Files.deleteIfExists(directory)
