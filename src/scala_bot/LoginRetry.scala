package scala_bot

import cats.effect.IO
import java.io.IOException
import java.time.{Duration as JavaDuration, Instant, ZonedDateTime}
import java.time.format.DateTimeFormatter
import scala.concurrent.duration.*
import scala.util.Try

object LoginRetry:
	final case class RateLimited(delay: FiniteDuration) extends IOException("Login rate limited")

	def delayFor(header: Option[String], now: Instant): FiniteDuration =
		val parsed = header.flatMap: value =>
			Try(value.trim.toLong.seconds).toOption.filter(_ >= Duration.Zero).orElse:
				Try(JavaDuration.between(now, ZonedDateTime.parse(value, DateTimeFormatter.RFC_1123_DATE_TIME).nn.toInstant).nn.toMillis.millis).toOption
		parsed.getOrElse(60.seconds).max(30.seconds) + 1.second

	def check(code: Int, header: Option[String], now: Instant = Instant.now().nn): IO[Unit] =
		IO.raiseWhen(code == 429)(RateLimited(delayFor(header, now)))

	def run[A](attempt: IO[A], pause: FiniteDuration => IO[Unit] = delay => IO.sleep(delay)): IO[A] =
		attempt.handleErrorWith:
			case RateLimited(delay) =>
				IO.println(s"Login rate limited; retrying in ${delay.toSeconds}s.") *> pause(delay) *> run(attempt, pause)
			case error => IO.raiseError(error)
