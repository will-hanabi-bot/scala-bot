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
val NO_COLOUR = "White|Gray|Light|Null|Rainbow|Omni|Prism".r

case class JSONVariant(id: Int, name: String, suits: IndexedSeq[String]) derives ReadWriter

case class Variant(
	id: Int,
	name: String,
	suits: IndexedSeq[String],
	shortForms: IndexedSeq[Char],
):
	val colourableSuits = suits.filterNot(NO_COLOUR.matches)

	def allIds =
		(0 until suits.length).flatMap(suitIndex => (1 to 5).map(Identity(suitIndex, _)))

	def touchPossibilities(clue: BaseClue) =
		allIds.filter(idTouched(_, clue))

	def cardCount(id: Identity): Int =
		val Identity(suitIndex, rank) = id
		if (DARK.matches(suits(suitIndex)))
			1
		else
			Vector(3, 2, 2, 2, 1)(rank - 1)

	def idTouched(id: Identity, clue: ClueLike): Boolean =
		val Identity(suitIndex, rank) = id
		val suit = suits(suitIndex)

		if (clue.kind == ClueKind.Colour)
			if (WHITISH.matches(suit))
				false
			else if (RAINBOWISH.matches(suit))
				true
			else if (PRISM.matches(suit))
				((rank - 1) % colourableSuits.length) == clue.value
			else
				suits(suitIndex) == colourableSuits(clue.value)
		else
			if (BROWNISH.matches(suit))
				false
			else if (PINKISH.matches(suit))
				true
			else
				rank == clue.value

	def cardTouched(card: Identifiable, clue: ClueLike) =
		card.id().exists(idTouched(_, clue))

case class Suit(
	name: String,
	abbreviation: Option[Char]
)

object Variant:
	private var variants: Map[String, JSONVariant] = Map()
	private var colours: Vector[Suit] = Vector()

	def init(): Unit =
		val variantsJSON = read[Vector[JSONVariant]](requests.get(VARIANTS_URL).text())
		val coloursJSON = read[ujson.Value](requests.get(COLOURS_URL).text()).arr

		colours = coloursJSON.map { suitJson =>
			Suit(suitJson("name").str, suitJson.objOpt.flatMap(_.get("abbreviation").strOpt.map(_.charAt(0).toLower)))
		}.toVector
		variants = variantsJSON.map(v => v.name -> v).toMap

	def getVariant(name: String) =
		val variant = variants.get(name).getOrElse(throw new IllegalArgumentException(s"Variant '$name' not found!"))

		val shortForms: Vector[Char] = variant.suits.reverse.foldRight(Vector()){ (suit, acc) =>
			val short: Char = suit match {
				case "Black" => 'k'
				case "Pink" => 'i'
				case "Brown" => 'n'
				case _ => {
					val colour = colours.find(_.name == suit).getOrElse(throw new IllegalArgumentException(s"Colour '$suit' not found!"))
					val abbreviation = colour.abbreviation.getOrElse(suit.charAt(0).toLower)
					if (!acc.contains(abbreviation)) {
						abbreviation
					} else {
						suit.toLowerCase.find(c => !acc.contains(c))
							.getOrElse(throw new IllegalArgumentException(s"No unused character found for suit '$suit' in ${variant.suits}!"))
					}
				}
			}
			short +: acc
		}.reverse

		Variant(variant.id, variant.name, variant.suits, shortForms)
