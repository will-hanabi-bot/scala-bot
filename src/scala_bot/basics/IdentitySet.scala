package scala_bot.basics

opaque type IdentitySet = Long

extension(ids: IdentitySet)
	inline def value: Long = ids
	inline def length: Int = java.lang.Long.bitCount(ids)
	inline def isEmpty: Boolean = length == 0
	inline def nonEmpty: Boolean = length > 0

	inline def ==(other: IdentitySet): Boolean =
		value == other.value

	inline def head: Identity =
		if value == 0 then
			throw new NoSuchElementException("head of empty IdentitySet!")
		val bit = java.lang.Long.numberOfTrailingZeros(ids)
		Identity.fromOrd(bit)

	inline def contains(id: Identity): Boolean =
		(ids & IdentitySet.single(id)) != 0

	inline def foreachFast(inline f: Identity => Unit): Unit =
		var remaining = ids
		while remaining != 0L do {
			val bit = java.lang.Long.numberOfTrailingZeros(remaining)
			f(Identity.fromOrd(bit))

			remaining &= (remaining - 1)
		}

	def foreach(f: Identity => Unit): Unit =
		var remaining = ids
		while remaining != 0L do {
			val bit = java.lang.Long.numberOfTrailingZeros(remaining)
			f(Identity.fromOrd(bit))

			remaining &= (remaining - 1)
		}

	inline def forall(inline f: Identity => Boolean) =
		var remaining = ids
		var res = true
		while remaining != 0L && res do {
			val bit = java.lang.Long.numberOfTrailingZeros(remaining)
			res &= f(Identity.fromOrd(bit))

			remaining &= (remaining - 1)
		}
		res

	inline def exists(inline f: Identity => Boolean) =
		var remaining = ids
		var res = false
		while remaining != 0L && !res do {
			val bit = java.lang.Long.numberOfTrailingZeros(remaining)
			res |= f(Identity.fromOrd(bit))

			remaining &= (remaining - 1)
		}
		res

	def toList =
		var remaining = ids
		var res = List.empty[Identity]
		while remaining != 0L do {
			val bit = java.lang.Long.numberOfTrailingZeros(remaining)
			res = Identity.fromOrd(bit) +: res

			remaining &= (remaining - 1)
		}
		res

	def map[A](f: Identity => A) =
		var remaining = ids
		var res = Seq.empty[A]
		while remaining != 0L do {
			val bit = java.lang.Long.numberOfTrailingZeros(remaining)
			res = f(Identity.fromOrd(bit)) +: res

			remaining &= (remaining - 1)
		}
		res

	inline def filter(inline cond: Identity => Boolean): IdentitySet =
		var bits = ids
		var res = ids

		while bits != 0 do {
			val tz = java.lang.Long.numberOfTrailingZeros(bits)
			bits &= bits - 1

			val id = Identity.fromOrd(tz)
			if !cond(id) then
				res &= ~(1L << tz)
		}
		res

	def flatMap[A](f: Identity => Iterable[A]) =
		var remaining = ids
		var res = Seq.empty[A]
		while remaining != 0L do {
			val bit = java.lang.Long.numberOfTrailingZeros(remaining)
			res = f(Identity.fromOrd(bit)) ++: res

			remaining &= (remaining - 1)
		}
		res

	def withFilter(p: Identity => Boolean) =
		var remaining = ids
		var res = Seq.empty[Identity]
		while remaining != 0L do {
			val bit = java.lang.Long.numberOfTrailingZeros(remaining)
			if p(Identity.fromOrd(bit)) then
				res = Identity.fromOrd(bit) +: res

			remaining &= (remaining - 1)
		}
		res

	inline def intersect(other: IdentitySet): IdentitySet =
		ids & other

	inline def intersect(other: Iterable[Identity]): IdentitySet =
		ids & IdentitySet.from(other)

	inline def union(other: IdentitySet): IdentitySet =
		ids | other

	inline def union(id: Identity): IdentitySet =
		ids | IdentitySet.single(id)

	inline def union(other: Iterable[Identity]): IdentitySet =
		ids | IdentitySet.from(other)

	inline def difference(other: IdentitySet): IdentitySet =
		ids & ~other

	inline def difference(id: Identity): IdentitySet =
		ids & ~IdentitySet.single(id)

	inline def difference(other: Iterable[Identity]): IdentitySet =
		ids & ~IdentitySet.from(other)

	def fmt(state: State) =
		val str = StringBuilder()
		var remaining = ids
		while remaining != 0L do {
			val bit = java.lang.Long.numberOfTrailingZeros(remaining)
			str ++= state.logId(Identity.fromOrd(bit)) + ","

			remaining &= (remaining - 1)
		}

		str.dropRight(1).toString()

object IdentitySet:
	inline def empty: IdentitySet = 0L

	inline def single(id: Identity): IdentitySet =
		1L << id.toOrd

	inline def from(ids: Iterable[Identity]): IdentitySet =
		ids.foldLeft(0L) { (acc, id) => acc | IdentitySet.single(id) }

	inline def create(inline cond: Identity => Boolean, maxIds: Int): IdentitySet =
		var i = 0
		var res = 0L

		while i < maxIds do {
			val id = Identity.fromOrd(i)
			if cond(id) then
				res |= (1L << i)

			i = i + 1
		}
		res

	def unapplySeq(ids: IdentitySet): Option[Seq[Identity]] =
		Some(ids.toList)
