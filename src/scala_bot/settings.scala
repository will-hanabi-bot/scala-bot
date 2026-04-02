package scala_bot

val BOT_VERSION = "v0.10.0 (scala-bot)"
val MAX_H_LEVEL = 11

enum Convention:
	case Reactor, RefSieve, HGroup

	override def toString: String = this match
		case Reactor  => "Reactor"
		case RefSieve => "Ref Sieve"
		case HGroup   => "H-Group"

object Convention:
	def from(s: String): Option[Convention] = s match
		case "Reactor" => Some(Convention.Reactor)
		case "RefSieve" => Some(Convention.RefSieve)
		case "HGroup" => Some(Convention.HGroup)
		case _ => None

case class Settings(convention: Convention, level: Option[Int] = None):
	override def toString: String = convention match
		case Convention.Reactor => s"${Convention.Reactor} 1.0"
		case Convention.RefSieve => Convention.RefSieve.toString
		case Convention.HGroup => s"${Convention.HGroup} ${level.getOrElse(1)}"

def infoNote(settings: Settings): String =
	s"[INFO: $BOT_VERSION, ${settings}]"

def parseSettingsFromNote(note: String): Option[Settings] =
	for
		_ <- Option.when(note.startsWith("[INFO:"))(())
		parts =  note.split("\\|", 2)
		info <- parts.lift(0)
		openIdx <- Some(info.indexOf('[')).filter(_ >= 0)
		closeIdx <- Some(info.indexOf(']')).filter(_ >= 0)
		bracket <- Some(info.substring(openIdx, closeIdx))
		convStr <- bracket.split(",", 2).lift(1).map(_.trim)
		settings <- convStr match
			case s if s.startsWith(Convention.HGroup.toString) =>
				s.drop(Convention.HGroup.toString.length).trim.toIntOption
					.map(lvl => Settings(Convention.HGroup, Some(lvl)))
			case s if s == Convention.RefSieve.toString =>
				Some(Settings(Convention.RefSieve))
			case s if s == Convention.Reactor.toString =>
				Some(Settings(Convention.Reactor))
			case _ =>
				None
	yield settings
