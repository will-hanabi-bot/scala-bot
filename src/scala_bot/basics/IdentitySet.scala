package scala_bot.basics

opaque type IdentitySet = Long

extension(ids: IdentitySet)
	inline def value: Long = ids
	/** The number of identities contained. */
	inline def length: Int = java.lang.Long.bitCount(ids)
	inline def isEmpty: Boolean = length == 0
	inline def nonEmpty: Boolean = length > 0

	inline def ==(other: IdentitySet): Boolean =
		value == other.value

	/** Returns a random identity contained in the set. */
	inline def head: Identity =
		if value == 0 then
			throw new NoSuchElementException("head of empty IdentitySet!")
		val bit = java.lang.Long.numberOfTrailingZeros(ids)
		Identity.fromOrd(bit)

	/** Returns true if the set is of size 1, containing only this identity. */
	inline def isExactly(id: Identity): Boolean =
		length == 1 && head == id

	inline def contains(id: Identity): Boolean =
		(ids & IdentitySet.single(id)) != 0

	inline def foreach(f: Identity => Unit): Unit =
		var remaining = ids
		while remaining != 0L do
			val bit = java.lang.Long.numberOfTrailingZeros(remaining)
			f(Identity.fromOrd(bit))

			remaining &= (remaining - 1)

	inline def forall(inline f: Identity => Boolean) =
		var remaining = ids
		var res = true
		while remaining != 0L && res do
			val bit = java.lang.Long.numberOfTrailingZeros(remaining)
			res &= f(Identity.fromOrd(bit))

			remaining &= (remaining - 1)
		res

	inline def exists(inline f: Identity => Boolean) =
		var remaining = ids
		var res = false
		while remaining != 0L && !res do
			val bit = java.lang.Long.numberOfTrailingZeros(remaining)
			res |= f(Identity.fromOrd(bit))

			remaining &= (remaining - 1)
		res

	inline def find(inline cond: Identity => Boolean): Option[Identity] =
		var bits = ids
		var found = false
		var foundId: Identity = null

		while bits != 0 && !found do
			val tz = java.lang.Long.numberOfTrailingZeros(bits)
			bits &= bits - 1

			val id = Identity.fromOrd(tz)
			if cond(id) then
				found = true
				foundId = id

		if found then Some(foundId) else None

	def toList =
		var res = List.empty[Identity]
		ids.foreach: i =>
			res = i +: res
		res

	def iter: Iterator[Identity] =
		new Iterator[Identity]:
			private var remaining = ids

			def hasNext: Boolean = remaining != 0L

			def next(): Identity =
				if remaining == 0L then
					throw new NoSuchElementException("next on empty iterator")

				val tz = java.lang.Long.numberOfTrailingZeros(remaining)
				remaining &= (remaining - 1)

				Identity.fromOrd(tz)

	def map[A](f: Identity => A) =
		var res = Seq.empty[A]
		ids.foreach: i =>
			res = f(i) +: res
		res

	inline def filter(inline cond: Identity => Boolean): IdentitySet =
		var bits = ids
		var res = ids

		while bits != 0 do
			val tz = java.lang.Long.numberOfTrailingZeros(bits)
			bits &= bits - 1

			val id = Identity.fromOrd(tz)
			if !cond(id) then
				res &= ~(1L << tz)
		res

	inline def count(inline cond: Identity => Boolean): Int =
		var res = 0

		ids.foreach: i =>
			if cond(i) then
				res += 1
		res

	def flatMap[A](f: Identity => Iterable[A]) =
		var res = Seq.empty[A]
		ids.foreach: i =>
			res = f(i) ++: res
		res

	def withFilter(p: Identity => Boolean) =
		var remaining = ids
		var res = Seq.empty[Identity]
		while remaining != 0L do
			val bit = java.lang.Long.numberOfTrailingZeros(remaining)
			if p(Identity.fromOrd(bit)) then
				res = Identity.fromOrd(bit) +: res

			remaining &= (remaining - 1)
		res

	def summing2[N](f: Identity => N)(using numeric: Numeric[N]) =
		var res = numeric.zero
		ids.foreach: i =>
			res = numeric.plus(res, f(i))
		res

	/** If empty, returns alternative, otherwise returns this. */
	inline def whenEmpty(inline alternative: => IdentitySet) =
		if ids.isEmpty then alternative else ids

	/** Returns a new IdentitySet containing all identities in both this set and the other set. */
	inline def intersect(other: IdentitySet): IdentitySet =
		ids & other

	inline def intersect(other: Iterable[Identity]): IdentitySet =
		ids & IdentitySet.from(other)

	/** Returns a new IdentitySet containing all identities in either this set or the other set. */
	inline def union(other: IdentitySet): IdentitySet =
		ids | other

	inline def union(id: Identity): IdentitySet =
		ids | IdentitySet.single(id)

	inline def union(other: Iterable[Identity]): IdentitySet =
		ids | IdentitySet.from(other)

	/** Returns a new IdentitySet containing all identities in this set that aren't in the other set. */
	inline def difference(other: IdentitySet): IdentitySet =
		ids & ~other

	inline def difference(id: Identity): IdentitySet =
		ids & ~IdentitySet.single(id)

	inline def difference(other: Iterable[Identity]): IdentitySet =
		ids & ~IdentitySet.from(other)

	def fmt(state: State) =
		val str = StringBuilder()
		var remaining = ids
		while remaining != 0L do
			val bit = java.lang.Long.numberOfTrailingZeros(remaining)
			str ++= state.logId(Identity.fromOrd(bit)) + ","

			remaining &= (remaining - 1)

		str.dropRight(1).toString()

	def toOpt: IdentitySetOpt =
		ids

object IdentitySet:
	inline def empty: IdentitySet = 0L

	/** Creates an IdentitySet of size 1, containing this identity. */
	inline def single(id: Identity): IdentitySet =
		1L << id.toOrd

	/** Creates an IdentitySet from an iterable sequence of identities. */
	inline def from(ids: Iterable[Identity]): IdentitySet =
		ids.foldLeft(0L) { (acc, id) => acc | IdentitySet.single(id) }

	/** Returns an IdentitySet with ids that satisfy the condition, up to the max number. */
	inline def create(inline cond: Identity => Boolean, maxIds: Int): IdentitySet =
		var i = 0
		var res = 0L

		while i < maxIds do
			val id = Identity.fromOrd(i)
			if cond(id) then
				res |= (1L << i)

			i = i + 1
		res

	def unapplySeq(ids: IdentitySet): Option[Seq[Identity]] =
		Some(ids.toList)

opaque type IdentitySetOpt = Long

extension(ids: IdentitySetOpt)
	inline def isDefined = ids != -1L

	inline def get: IdentitySet =
		if ids != -1L then ids else throw new NoSuchElementException("IdentitySetOpt.get")

	inline def getOrElse(a: => IdentitySet): IdentitySet =
		if ids != -1L then ids else a

	inline def orElse(a: => IdentitySetOpt): IdentitySetOpt =
		if ids != -1L then ids else a

	inline def existsO(inline f: IdentitySet => Boolean) =
		ids != -1L && f(ids)

	inline def forallO(inline f: IdentitySet => Boolean) =
		ids == -1L || f(ids)

	/** A mapping to another IdentitySet. */
	inline def mapO(f: IdentitySet => IdentitySet): IdentitySetOpt =
		if ids == -1L then -1L else f(ids)

	/** A mapping to another type. */
	def mapA[A](f: IdentitySet => A): Option[A] =
		Option.when(ids != -1L)(f(ids))

object IdentitySetOpt:
	inline def empty: IdentitySetOpt = -1L
