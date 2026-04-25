package scala_bot

val BOT_VERSION = "v0.10.4 (scala-bot)"
val MAX_H_LEVEL = 11

enum Convention:
	case Reactor
	case RefSieve
	case HGroup(level: Int)

	override def toString: String = this match
		case Reactor       => "Reactor1"
		case RefSieve      => "RefSieve"
		case HGroup(level) => s"HGroup$level"

val reactorPattern = """(?i)Reactor1?""".r.unanchored
val refSievePattern = """(?i)RefSieve|rs""".r.unanchored
val HGroupPattern = """(?i)HGroup(\d+)""".r.unanchored
val levelOnlyPattern = """(\d+)""".r.unanchored

object Convention:
	def makeH(level: Int): Either[String, Convention] =
		if level < 1 || level > MAX_H_LEVEL then
			Left(s"scala-bot can only play HGroup between levels 1-$MAX_H_LEVEL.")
		else
			Right(Convention.HGroup(level))

	def from(s: String): Either[String, Convention] =
		s match
			case reactorPattern()        => Right(Convention.Reactor)
			case refSievePattern()       => Right(Convention.RefSieve)
			case HGroupPattern(level)    => makeH(if level == null then 1 else level.toInt)
			case levelOnlyPattern(level) => makeH(level.toInt)
			case _ => Left(s"Unrecognized convention $s. Supported: HGroup[1-11], RefSieve, Reactor1.")

def infoNote(convention: Convention): String =
	s"[INFO: $BOT_VERSION, ${convention}]"

val infoNotePattern = """\[INFO: .*?, (\w+)\]""".r.unanchored

def parseConventionFromNote(note: String): Option[Convention] =
	note match
		case infoNotePattern(conv) => Convention.from(conv).toOption
		case _ => None
