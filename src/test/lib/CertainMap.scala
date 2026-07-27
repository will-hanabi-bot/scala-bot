package tests

import scala_bot.lib.{CertainMapEntry, FastBitSet}
import scala_bot.logger.{Logger, LogLevel}

class CertainMap extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("basic operations"):
		var entry = CertainMapEntry.empty
		entry = entry.update(order = 10, unknownTo = 2)

		assertEquals(entry.size, 1)

		assert(entry.contains(order = 10))
		assert(entry.contains(order = 9, unknownTo = 2))

		assert(!entry.contains(order = 2))
		assert(!entry.contains(order = 2, unknownTo = 1))

	test("overwrites with the same order"):
		var entry = CertainMapEntry.empty
		entry = entry.update(order = 10, unknownTo = 2)
		entry = entry.update(order = 10, unknownTo = -1)

		assert(!entry.contains(order = 9))
		assertEquals(entry.size, 1)

	test("adds a new slot with a different order"):
		var entry = CertainMapEntry.empty
		entry = entry.update(order = 10, unknownTo = -1)
		entry = entry.update(order = 7, unknownTo = -1)

		assert(entry.contains(order = 10))
		assert(entry.contains(order = 7))
		assertEquals(entry.size, 2)

	test("manages three slots"):
		var entry = CertainMapEntry.empty
		entry = entry.update(order = 10, unknownTo = 1)
		entry = entry.update(order = 7, unknownTo = 2)

		assertEquals(entry.size, 2)

		entry = entry.update(order = 10, unknownTo = -1)

		assertEquals(entry.size, 2)

		entry = entry.update(order = 3, unknownTo = -1)

		assertEquals(entry.size, 3)

		entry = entry.update(order = 7, unknownTo = -1)

		assertEquals(entry.size, 3)
		assert(entry.contains(order = 10))
		assert(entry.contains(order = 7))
		assert(entry.contains(order = 3))

	test("converts to a FastBitSet"):
		var entry = CertainMapEntry.empty
		entry = entry.update(order = 10, unknownTo = 1)
		entry = entry.update(order = 7, unknownTo = 2)
		val fastBitSet = entry.toFastBitSet

		assertEquals(fastBitSet, FastBitSet(10, 7))
