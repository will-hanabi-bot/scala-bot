package scala_bot

val BOT_VERSION = "v0.10.0 (scala-bot)"
val MAX_H_LEVEL = 11

enum Convention:
	case Reactor, RefSieve, HGroup

object Convention:
	def from(s: String): Option[Convention] = s match
		case "Reactor" => Some(Convention.Reactor)
		case "RefSieve" => Some(Convention.RefSieve)
		case "HGroup" => Some(Convention.HGroup)
		case _ => None

case class Settings(convention: Convention, level: Option[Int] = None):
	def toString: String = convention match
		case Convention.Reactor => "Reactor 1.0"
		case Convention.RefSieve => "Ref Sieve"
		case Convention.HGroup => s"H-Group ${level.getOrElse(1)}"
