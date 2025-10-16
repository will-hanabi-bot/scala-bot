package scala_bot.basics

type IdentitySet = Long

extension(ids: IdentitySet)
	def value: Long = ids
	def length: Int = java.lang.Long.bitCount(ids)
	def isEmpty: Boolean = length == 0

	def contains(id: Identity): Boolean =
		(ids & IdentitySet.single(id)) != 0

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

	def intersect(other: IdentitySet): IdentitySet =
		ids & other

	def union(other: IdentitySet): IdentitySet =
		ids | other

	def union(id: Identity): IdentitySet =
		ids | IdentitySet.single(id)

	def difference(other: IdentitySet): IdentitySet =
		ids & ~other

	def difference(id: Identity): IdentitySet =
		ids & ~IdentitySet.single(id)

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
	def empty: IdentitySet = 0L

	def single(id: Identity): IdentitySet =
		1L << id.toOrd

	def from(ids: Iterable[Identity]) =
		ids.foldLeft(0L) { (acc, id) => acc | IdentitySet.single(id) }

	def unapplySeq(ids: IdentitySet): Option[Seq[Identity]] =
		Some(ids.iterator.toSeq)
