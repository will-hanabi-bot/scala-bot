package scala_bot.basics

/** A faster bitset that only supports storing integers between 0 and 63. */
opaque type FastBitSet = Long

object FastBitSet:
	inline def empty: FastBitSet = 0L

	/** Creates an FastBitSet of size 1, containing this number. */
	inline def single(num: Int): FastBitSet =
		1L << num

	/** Creates an FastBitSet from an iterable sequence of numbers. */
	inline def from(set: Iterable[Int]): FastBitSet =
		set.foldLeft(0L) { (acc, id) => acc | FastBitSet.single(id) }

	def unapplySeq(set: FastBitSet): Option[Seq[Int]] =
		Some(set.toList)

	extension(set: FastBitSet)
		inline def value: Long = set
		/** The number of numbers contained. */
		inline def size: Int = java.lang.Long.bitCount(set)
		inline def isEmpty: Boolean = size == 0
		inline def nonEmpty: Boolean = size > 0

		inline def ==(other: FastBitSet): Boolean =
			value == other.value

		/** Returns a random identity contained in the set. */
		inline def head: Int =
			if value == 0 then
				throw new NoSuchElementException("head of empty FastBitSet!")

			java.lang.Long.numberOfTrailingZeros(set)

		/** Returns true if the set is of size 1, containing only this integer. */
		inline def isExactly(num: Int): Boolean =
			size == 1 && head == num

		inline def contains(num: Int): Boolean =
			(set & FastBitSet.single(num)) != 0

		inline def foreach(f: Int => Unit): Unit =
			var remaining = set
			while remaining != 0L do
				val bit = java.lang.Long.numberOfTrailingZeros(remaining)
				f(bit)

				remaining &= (remaining - 1)

		inline def forall(inline f: Int => Boolean) =
			var remaining = set
			var res = true
			while remaining != 0L && res do
				val bit = java.lang.Long.numberOfTrailingZeros(remaining)
				res &= f(bit)

				remaining &= (remaining - 1)
			res

		inline def exists(inline f: Int => Boolean) =
			var remaining = set
			var res = false
			while remaining != 0L && !res do
				val bit = java.lang.Long.numberOfTrailingZeros(remaining)
				res |= f(bit)

				remaining &= (remaining - 1)
			res

		inline def find(inline cond: Int => Boolean): Option[Int] =
			var bits = set
			var found = false
			var foundNum: Int = -1

			while bits != 0 && !found do
				val tz = java.lang.Long.numberOfTrailingZeros(bits)
				bits &= bits - 1

				if cond(tz) then
					found = true
					foundNum = tz

			if found then Some(foundNum) else None

		def toList =
			var res = List.empty[Int]
			set.foreach: i =>
				res = i +: res
			res

		def iter: Iterator[Int] =
			new Iterator[Int]:
				private var remaining = set

				def hasNext: Boolean = remaining != 0L

				def next(): Int =
					if remaining == 0L then
						throw new NoSuchElementException("next on empty iterator")

					val tz = java.lang.Long.numberOfTrailingZeros(remaining)
					remaining &= (remaining - 1)

					tz

		def map[A](f: Int => A) =
			var res = Seq.empty[A]
			set.foreach: i =>
				res = f(i) +: res
			res

		inline def filter(inline cond: Int => Boolean): FastBitSet =
			var bits = set
			var res = set

			while bits != 0 do
				val tz = java.lang.Long.numberOfTrailingZeros(bits)
				bits &= bits - 1

				if !cond(tz) then
					res &= ~(1L << tz)
			res

		inline def foldLeft[A](init: A)(f: (A, Int) => A): A =
			var acc = init
			set.foreach: i =>
				acc = f(acc, i)
			acc

		inline def count(inline cond: Int => Boolean): Int =
			var res = 0

			set.foreach: i =>
				if cond(i) then
					res += 1
			res

		def flatMap[A](f: Int => Iterable[A]) =
			var res = Seq.empty[A]
			set.foreach: i =>
				res = f(i) ++: res
			res

		def withFilter(p: Int => Boolean) =
			var remaining = set
			var res = Seq.empty[Int]
			while remaining != 0L do
				val bit = java.lang.Long.numberOfTrailingZeros(remaining)
				if p(bit) then
					res = bit +: res

				remaining &= (remaining - 1)
			res

		def summing[N](f: Int => N)(using numeric: Numeric[N]) =
			var res = numeric.zero
			set.foreach: i =>
				res = numeric.plus(res, f(i))
			res

		/** If empty, returns alternative, otherwise returns this. */
		inline def whenEmpty(inline alternative: => FastBitSet) =
			if set.isEmpty then alternative else set

		/** Returns a new FastBitSet containing all numbers in both this set and the other set. */
		inline def intersect(other: FastBitSet): FastBitSet =
			set & other

		inline def intersect(other: Iterable[Int]): FastBitSet =
			set & FastBitSet.from(other)

		/** Returns a new FastBitSet containing all numbers in either this set or the other set. */
		inline def union(other: FastBitSet): FastBitSet =
			set | other

		inline def incl(num: Int): FastBitSet =
			set | FastBitSet.single(num)

		inline def union(other: Iterable[Int]): FastBitSet =
			set | FastBitSet.from(other)

		/** Returns a new FastBitSet containing all numbers in this set that aren't in the other set. */
		inline def difference(other: FastBitSet): FastBitSet =
			set & ~other

		inline def excl(num: Int): FastBitSet =
			set & ~FastBitSet.single(num)

		inline def difference(other: Iterable[Int]): FastBitSet =
			set & ~FastBitSet.from(other)

		def fmt =
			val str = StringBuilder()
			var remaining = set
			while remaining != 0L do
				val bit = java.lang.Long.numberOfTrailingZeros(remaining)
				str ++= bit + ","

				remaining &= (remaining - 1)

			str.dropRight(1).toString()
