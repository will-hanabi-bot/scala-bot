package scala_bot.basics

import upickle.default._

val VARIANTS_URL = "https://raw.githubusercontent.com/Hanabi-Live/hanabi-live/main/packages/game/src/json/variants.json"
val COLOURS_URL = "https://raw.githubusercontent.com/Hanabi-Live/hanabi-live/main/packages/game/src/json/suits.json"

val WHITISH = "White|Gray|Light|Null".r
val RAINBOWISH = "Rainbow|Omni".r
val PINKISH = "Pink|Omni".r
val BROWNISH = "Brown|Muddy|Cocoa|Null".r
val DARK = "Black|Dark|Gray|Cocoa".r
val PRISM = "Prism".r
val MUDDY = "Muddy|Cocoa".r
val NO_COLOUR = "White|Gray|Light|Null|Rainbow|Omni|Prism".r

case class Variant(
	id: Int,
	name: String,
	suits: IndexedSeq[String],
	criticalRank: Option[Int] = None,
	clueStarved: Boolean = false,
	specialRank: Option[Int] = None,
	rainbowS: Boolean = false,
	whiteS: Boolean = false,
	pinkS: Boolean = false,
	brownS: Boolean = false,
	deceptiveS: Boolean = false,
	shorts: Option[Vector[Char]] = None
):
	lazy val shortForms: Vector[Char] =
		shorts.getOrElse:
			suits.foldLeft(Vector()): (acc, suit) =>
				val short: Char = suit match
					case "Black" => 'k'
					case "Pink" => 'i'
					case "Brown" => 'n'
					case _ =>
						val colour = Variant.colours.find(_.name == suit).getOrElse(throw new IllegalArgumentException(s"Colour '$suit' not found!"))
						val abbreviation = colour.abbreviation.getOrElse(suit.charAt(0).toLower)
						if !acc.contains(abbreviation) then
							abbreviation
						else
							suit.toLowerCase.find(c => !acc.contains(c))
								.getOrElse(throw new IllegalArgumentException(s"No unused character found for suit '$suit' in $suits!"))
				short +: acc

	val colourableSuits = suits.filterNot(NO_COLOUR.matches)

	def allIds =
		for
			suitIndex <- 0 until suits.length
			rank      <- 1 to 5
		yield
			Identity(suitIndex, rank)

	def touchPossibilities(clue: BaseClue) =
		allIds.filter(idTouched(_, clue))

	def cardCount(id: Identity): Int =
		val Identity(suitIndex, rank) = id
		if DARK.matches(suits(suitIndex)) || criticalRank.contains(rank) then
			1
		else
			Vector(3, 2, 2, 2, 1)(rank - 1)

	def idTouched(id: Identity, clue: ClueLike): Boolean =
		val Identity(suitIndex, rank) = id
		val suit = suits(suitIndex)

		if clue.kind == ClueKind.Colour then
			if WHITISH.matches(suit) then
				return false

			if RAINBOWISH.matches(suit) then
				return true

			if specialRank.contains(rank) then
				return rainbowS || !whiteS

			if PRISM.matches(suit) then
				return ((rank - 1) % colourableSuits.length) == clue.value

			suits(suitIndex) == colourableSuits(clue.value)
		else
			if BROWNISH.matches(suit) then
				return false

			if specialRank.contains(rank) then
				if pinkS then
					return rank != clue.value

				if brownS then
					return false

				if deceptiveS then
					return (suitIndex % 4) + (if rank == 1 then 2 else 1) == clue.value

			if PINKISH.matches(suit) then
				return true

			rank == clue.value

	def cardTouched(card: Identifiable, clue: ClueLike) =
		card.id().exists(idTouched(_, clue))

case class Suit(name: String, abbreviation: Option[Char])

object Variant:
	private var variants: Map[String, Variant] = Map()
	private var colours: Seq[Suit] = Seq()

	def init(): Unit =
		val variantsJSON = read[ujson.Value](requests.get(VARIANTS_URL).text()).arr
		val coloursJSON = read[ujson.Value](requests.get(COLOURS_URL).text()).arr

		colours = coloursJSON.map(suitJson => Suit(
			suitJson("name").str,
			suitJson.objOpt.flatMap(_.get("abbreviation").strOpt.map(_.charAt(0).toLower))
		)).toSeq

		variants = variantsJSON.map(Variant.fromJSON).map(v => v.name -> v).toMap

	def fromJSON(json: ujson.Value) =
		val obj = json.obj
		Variant(
			json("id").num.toInt,
			json("name").str,
			json("suits").arr.map(_.str).toIndexedSeq,
			obj.get("criticalRank").map(_.num.toInt),
			obj.get("clueStarved").map(_.bool).getOrElse(false),
			obj.get("specialRank").map(_.num.toInt),
			rainbowS = obj.get("specialRankAllClueColors").map(_.bool).getOrElse(false),
			whiteS = obj.get("specialRankNoClueColors").map(_.bool).getOrElse(false),
			pinkS = obj.get("specialRankAllClueRanks").map(_.bool).getOrElse(false),
			brownS = obj.get("specialRankNoClueRanks").map(_.bool).getOrElse(false),
			obj.get("specialRankDeceptive").map(_.bool).getOrElse(false)
		)

	def getVariant(name: String) =
		variants.get(name).getOrElse(throw new IllegalArgumentException(s"Variant '$name' not found! Were variants initialized?"))
