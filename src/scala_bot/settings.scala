package scala_bot

val BOT_VERSION = "v0.10.0 (scala-bot)"
val MAX_H_LEVEL = 11

enum Convention:
	case Reactor
	case RefSieve
	case HGroup(level: Int)

	override def toString: String = this match
		case Reactor	=> "Reactor 1.0"
		case RefSieve => "Ref Sieve"
		case HGroup(level)	 => s"H-Group $level"

val reactorPattern = "Reactor".r.unanchored
val refSievePattern = "RefSieve".r.unanchored
val HGroupPattern = "HGroup (\\d+)".r.unanchored
val HGroupPatternWithoutLevel = "HGroup".r.unanchored
val levelOnlyPattern = "(\\d+)".r.unanchored

object Convention:
	def makeH(level: Int): Either[String, Convention] =
		if level < 1 || level > MAX_H_LEVEL then
			Left("scala-bot can only play HGroup between levels 1-$MAX_H_LEVEL.")
		else
			Right(Convention.HGroup(level))

	def from(s: String, parseLevel: Boolean = true): Either[String, Convention] = s match
		case reactorPattern() => Right(Convention.Reactor)
		case refSievePattern() => Right(Convention.RefSieve)
		case HGroupPattern(level) if parseLevel => makeH(level.toInt)
		case HGroupPatternWithoutLevel() if !parseLevel => makeH(1)
		case levelOnlyPattern(level) => makeH(level.toInt)
		case _ => Left("Unrecognized convention $s. Supported: HGroup, RefSieve, Reactor.")

def infoNote(convention: Convention): String =
	s"[INFO: $BOT_VERSION, ${convention}]"

def parseConventionFromNote(note: String): Option[Convention] =
	for
		_ <- Option.when(note.startsWith("[INFO:"))(())
		parts =	note.split("\\|", 2)
		info <- parts.lift(0)
		openIdx <- Some(info.indexOf('[')).filter(_ >= 0)
		closeIdx <- Some(info.indexOf(']')).filter(_ >= 0)
		bracket <- Some(info.substring(openIdx, closeIdx))
		convStr <- bracket.split(",", 2).lift(1).map(_.trim)
		convention <- Convention.from(convStr).toOption
	yield convention
