package tests.protocol

import scala_bot.*

import cats.effect.{Deferred, IO, Ref}
import cats.effect.unsafe.implicits.global
import scala.concurrent.duration.*
import java.time.Instant

class LoginRetrySuite extends munit.FunSuite:
	private val now = Instant.parse("2026-11-12T07:00:00Z").nn

	test("Retry-After seconds are honored with a small safety margin"):
		assertEquals(LoginRetry.delayFor(Some("37"), now), 38.seconds)
		assertEquals(LoginRetry.delayFor(Some("0"), now), 31.seconds)

	test("Retry-After dates and missing headers have safe delays"):
		assertEquals(LoginRetry.delayFor(Some("Thu, 12 Nov 2026 07:01:00 GMT"), now), 61.seconds)
		assertEquals(LoginRetry.delayFor(None, now), 61.seconds)
		assertEquals(LoginRetry.delayFor(Some("invalid"), now), 61.seconds)

	test("429 waits and retries the login instead of escaping into a container restart"):
		val count = Ref.of[IO, Int](0).unsafeRunSync()
		val delays = Ref.of[IO, List[FiniteDuration]](Nil).unsafeRunSync()
		val attempt = count.getAndUpdate(_ + 1).flatMap: index =>
			LoginRetry.check(if index < 2 then 429 else 200, Some("37"), now).as("session-cookie")
		assertEquals(LoginRetry.run(attempt, delay => delays.update(_ :+ delay)).unsafeRunSync(), "session-cookie")
		assertEquals(count.get.unsafeRunSync(), 3)
		assertEquals(delays.get.unsafeRunSync(), List(38.seconds, 38.seconds))

	test("other login errors are returned rather than retried indefinitely"):
		val error = new IllegalStateException("authentication failed")
		assertEquals(LoginRetry.run(IO.raiseError[String](error)).attempt.unsafeRunSync(), Left(error))

	test("waiting for a rate limit is cancellable"):
		(for
			paused <- Deferred[IO, Unit]
			task <- LoginRetry.run(LoginRetry.check(429, None, now), _ => paused.complete(()).void *> IO.never).start
			_ <- paused.get.timeout(2.seconds)
			_ <- task.cancel.timeout(2.seconds)
		yield ()).unsafeRunSync()
