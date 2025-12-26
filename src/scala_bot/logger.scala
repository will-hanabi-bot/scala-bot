package scala_bot.logger

import scala.Console._

object LogLevel:
	inline val Off = 0
	inline val Error = 1
	inline val Warn  = 2
	inline val Info  = 3

object Logger:
	@volatile var runtimeLevel: Int = LogLevel.Info

	def setLevel(level: Int): Unit =
		runtimeLevel = level

	def level = runtimeLevel

object Log:
	inline val compileTimePriority = 3

	inline def log(inline level: Int, inline msg: => String, inline colour: String = WHITE): Unit =
		inline if level <= compileTimePriority then
			if level <= Logger.runtimeLevel then
				println(s"$colour$msg$RESET")
		else
			()

	inline def error(inline msg: => String): Unit = log(LogLevel.Error, msg, MAGENTA)
	inline def warn(inline msg: => String): Unit = log(LogLevel.Warn,  msg, CYAN)
	inline def info(inline msg: => String): Unit = log(LogLevel.Info,  msg)
	inline def highlight(inline colour: String, inline msg: => String): Unit = log(LogLevel.Info,  msg, colour)
