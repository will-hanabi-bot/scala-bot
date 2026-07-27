package scala_bot.lib

import scala_bot.utils._

opaque type CertainMapEntry = Int

/** A bit-packed representation of a List[(order: Int, unknownTo: Int)].
 *  From LSB to MSB, [size][[unknownTo1][order1]][[unknownTo2][order2]]...
 *  The size indicates how many slots are stored, and can only go up.
 */
object CertainMapEntry:
	def empty: CertainMapEntry = 0

	inline def orderBits = 6		// 64 orders
	inline def unknownBits = 3		// 6 player indices, plus 1 for "known to everyone"
	inline def slotBits = 10			// order + unknown
	inline def sizeBits = 3			// at most 3 of any identity (but be lenient)

	inline def orderMask = ((1 << orderBits) - 1) << unknownBits
	inline def unknownMask = (1 << unknownBits) - 1

	extension (e: CertainMapEntry)
		def size = e & (1 << sizeBits) - 1

		inline def encodeUnknown(u: Int) =
			if u == -1 then unknownMask else u

		inline def decodeUnknown(u: Int) =
			if u == unknownMask then -1 else u

		inline def makeSlot(order: Int, unknownTo: Int) =
			val u = encodeUnknown(unknownTo)
			(order << unknownBits) | u

		inline def overwriteSlot(i: Int, replacement: Int) =
			val shift = (i * slotBits) + sizeBits
			val cleared = e & ~(((1 << slotBits) - 1) << shift)
			(replacement << shift) | cleared

		inline def foreach(f: (order: Int, unknownTo: Int, slot: Int) => Unit): Unit =
			var bits = e >> sizeBits	// shift past size

			loop(0, _ < e.size, _ + 1): s =>
				val o = (bits & orderMask) >> unknownBits
				val u = decodeUnknown(bits & unknownMask)

				f(o, u, s)
				bits >>= slotBits

		def update(order: Int, unknownTo: Int): CertainMapEntry =
			assert(order < 64)
			var res = e
			var exists = false

			e.foreach: (o, u, s) =>
				if order == o then
					exists = true
					if u != unknownTo then
						res = overwriteSlot(s, makeSlot(order, unknownTo))

			if !exists then
				// assert(e.size < 3)
				res = overwriteSlot(e.size, makeSlot(order, unknownTo))
				res = (res & ~((1 << sizeBits) - 1)) | (e.size + 1)

			res

		def contains(order: Int, unknownTo: Int = -2): Boolean =
			var bits = e >> sizeBits	// shift past size

			loop(0, _ < e.size, _ + 1): _ =>
				val o = (bits & orderMask) >> unknownBits
				val u = decodeUnknown(bits & unknownMask)

				if order == o || u == unknownTo then
					return true
				else
					bits >>= slotBits
			false

		def toFastBitSet: FastBitSet =
			var res = FastBitSet.empty
			e.foreach: (o, _, _) =>
				res = res.incl(o)
			res

		def fmt: String =
			var str = "CertainMapEntry("
			e.foreach: (o, u, s) =>
				str += s"$o/$u${if s == e.size - 1 then "" else ", "}"
			str += ")"
			str.toString
