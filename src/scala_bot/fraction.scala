package scala_bot.fraction

import scala.math.Numeric

@annotation.tailrec
def gcd(a: Long, b: Long): Long =
	if b == 0 then math.abs(a) else gcd(b, a % b)

def lcm(a: Long, b: Long) =
	math.abs(a * b) / gcd(a, b)

case class Frac(numer: Long, denom: Long):
	def toDecimal = numer.toDouble / denom.toDouble

	override def toString: String = if denom == 1 then s"$numer" else s"$numer/$denom"

	def reduced =
		lazy val div = gcd(numer, denom)
		if numer == 0 then
			Frac.zero
		else if numer > denom && numer % denom == 0 then
			Frac(numer / denom, 1)
		else if div > 1 then
			Frac(numer / div, denom / div)
		else
			this

	def +(other: Frac) =
		val m = lcm(denom, other.denom)
		Frac(numer * (m / denom) + other.numer * (m / other.denom), m).reduced

	def +(other: Int): Frac =
		this + Frac(other, 1)

	def unary_- : Frac =
		Frac(-numer, denom)

	def -(other: Frac) =
		val m = lcm(this.denom, other.denom)
		Frac(numer * (m / denom) - other.numer * (m / other.denom), m).reduced

	def -(other: Int): Frac =
		this - Frac(other, 1)

	def *(other: Frac) =
		Frac(numer * other.numer, denom * other.denom).reduced

	def *(other: Int): Frac =
		this * Frac(other, 1)

	def /(other: Frac) =
		Frac(numer * other.denom, denom * other.numer).reduced

	def /(other: Int): Frac =
		this / Frac(other, 1)

	def <(other: Frac) =
		(this - other).numer < 0

	def >(other: Frac) =
		(this - other).numer > 0

	def `==`(other: Frac) =
		val (a, b) = (this.reduced, other.reduced)
		a.numer == b.numer && a.denom == b.denom

	def `!=`(other: Frac) =
		!(this == other)

	def `<=`(other: Frac) =
		this < other || this == other

	def `>=`(other: Frac) =
		this > other || this == other

object Frac:
	def zero = Frac(0, 1)
	def one = Frac(1, 1)

	def apply(numer: Int, denom: Int): Frac =
		if denom == 0 then
			throw new IllegalArgumentException("denominator cannot be zero")
		new Frac(numer, denom)

	implicit val fracIsNumeric: Numeric[Frac] = new Numeric[Frac]:
		override def zero: Frac = Frac.zero

		def plus(x: Frac, y: Frac) = x + y

		def minus(x: Frac, y: Frac) = x - y

		def times(x: Frac, y: Frac) = x * y

		def negate(x: Frac) = Frac(-x.numer, x.denom)

		def fromInt(x: Int) = Frac(x, 1)

		def toInt(x: Frac): Int = (x.numer / x.denom).toInt

		def toLong(x: Frac): Long = x.numer / x.denom

		def toFloat(x: Frac): Float = x.toDecimal.toFloat

		def toDouble(x: Frac): Double = x.toDecimal

		def compare(x: Frac, y: Frac): Int =
			val diff = x - y
			if diff.numer < 0 then -1
			else if diff.numer > 0 then 1
			else 0

		def parseString(str: String) =
			val pattern = """(\d+)/(\d+)""".r
			str match
				case pattern(numer, denom) => Some(Frac(numer.toInt, denom.toInt))
				case _ => None
