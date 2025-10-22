package scala_bot.basics

type IdentitySet = Long

extension(ids: IdentitySet)
	inline def value: Long = ids
	inline def length: Int = java.lang.Long.bitCount(ids)
	inline def isEmpty: Boolean = length == 0
	inline def nempty: Boolean = length > 0

	inline def contains(id: Identity): Boolean =
		(ids & IdentitySet.single(id)) != 0

	inline def foreachFast(f: Identity => Unit): Unit = {
		var remaining = ids
		while (remaining != 0L) {
			val bit = java.lang.Long.numberOfTrailingZeros(remaining)
			f(Identity.fromOrd(bit))

			remaining &= (remaining - 1)
		}
	}

	def iterator: Iterator[Identity] = new Iterator[Identity]:
		private var remaining = ids

		def hasNext: Boolean = remaining != 0

		def next(): Identity =
			if (!hasNext)
				throw new NoSuchElementException("IdentitySet is empty!")

			val tz = java.lang.Long.numberOfTrailingZeros(remaining)
			val res = Identity.fromOrd(tz)

			remaining &= (remaining - 1)
			res

	def toIterable: Iterable[Identity] = new Iterable[Identity]:
		def iterator: Iterator[Identity] = ids.iterator

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

	def retain(cond: Identity => Boolean): IdentitySet =
		var bits = ids
		var res = ids

		while (bits != 0) {
			val tz = java.lang.Long.numberOfTrailingZeros(bits)
			bits &= bits - 1

			val id = Identity.fromOrd(tz)
			if (!cond(id))
				res &= ~IdentitySet.single(id)
		}
		res

given Conversion[IdentitySet, Iterable[Identity]] = _.toIterable

object IdentitySet:
	inline def empty: IdentitySet = 0L

	inline def single(id: Identity): IdentitySet =
		1L << id.toOrd

	inline def from(ids: Iterable[Identity]) =
		ids.foldLeft(0L) { (acc, id) => acc | IdentitySet.single(id) }

	def unapplySeq(ids: IdentitySet): Option[Seq[Identity]] =
		Some(ids.iterator.toSeq)
