package scala_bot.basics

import scala_bot.utils._
import upickle.default._

val VARIANTS_URL = "https://raw.githubusercontent.com/Hanabi-Live/hanabi-live/main/packages/game/src/json/variants.json"
val COLOURS_URL = "https://raw.githubusercontent.com/Hanabi-Live/hanabi-live/main/packages/game/src/json/suits.json"

val WHITISH = "White|Gray|Light|Null".r.unanchored
val RAINBOWISH = "Rainbow|Omni".r.unanchored
val PINKISH = "Pink|Omni".r.unanchored
val BROWNISH = "Brown|Muddy|Cocoa|Null".r.unanchored
val DARK = "Black|Dark|Gray|Cocoa".r.unanchored
val PRISM = "Prism".r.unanchored
val MUDDY = "Muddy|Cocoa".r.unanchored
val NO_COLOUR = "White|Gray|Light|Null|Rainbow|Omni|Prism".r.unanchored

case class Variant(
	id: Int,
	name: String,
	suits: Vector[Suit],
	shortForms: Vector[Char],
	colourableSuits: Vector[Suit],
	criticalRank: Option[Int],
	clueStarved: Boolean,
	specialRank: Option[Int],
	rainbowS: Boolean,
	whiteS: Boolean,
	pinkS: Boolean,
	brownS: Boolean,
	deceptiveS: Boolean,
	scarce1s: Boolean
):
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
		if suits(suitIndex).suitType.dark || criticalRank.contains(rank) then
			1
		else if rank == 1 && scarce1s then
			2
		else
			Vector(3, 2, 2, 2, 1)(rank - 1)

	def totalCards = allIds.summing(cardCount)

	def idTouched(id: Identity, clue: ClueLike): Boolean =
		val Identity(suitIndex, rank) = id
		val suit = suits(suitIndex)

		if clue.kind == ClueKind.Colour then
			if suit.suitType.rainbowish then
				return true

			if suit.suitType.whitish then
				return false

			if specialRank.contains(rank) then
				if rainbowS then
					return true

				if whiteS then
					return false

			if suit.suitType.prism then
				return ((rank - 1) % colourableSuits.length) == clue.value

			suits(suitIndex) == colourableSuits(clue.value)
		else
			if suit.suitType.pinkish then
				return true

			if suit.suitType.brownish then
				return false

			if specialRank.contains(rank) then
				if pinkS then
					return rank != clue.value

				if brownS then
					return false

				if deceptiveS then
					return (suitIndex % 4) + (if rank == 1 then 2 else 1) == clue.value

			rank == clue.value

	def cardTouched(card: Identifiable, clue: ClueLike) =
		card.id().exists(idTouched(_, clue))

case class SuitType(
	whitish: Boolean,
	rainbowish: Boolean,
	pinkish: Boolean,
	brownish: Boolean,
	dark: Boolean,
	prism: Boolean,
	muddy: Boolean
)

object SuitType:
	def apply(name: String): SuitType =
		SuitType(
			WHITISH.matches(name),
			RAINBOWISH.matches(name),
			PINKISH.matches(name),
			BROWNISH.matches(name),
			DARK.matches(name),
			PRISM.matches(name),
			MUDDY.matches(name)
		)

case class Suit(
	name: String,
	abbreviation: Option[Char],
	suitType: SuitType
)

object Variant:
	private var variants: Map[String, Variant] = Map()
	private var colours: Seq[Suit] = Seq()

	def init(): Unit =
		val variantsJSON = read[ujson.Value](requests.get(VARIANTS_URL).text()).arr
		val coloursJSON = read[ujson.Value](requests.get(COLOURS_URL).text()).arr

		colours = coloursJSON.map(suitJson =>
			Suit(
				suitJson("name").str,
				suitJson.obj.get("abbreviation").flatMap(_.strOpt.map(_.charAt(0).toLower)),
				SuitType(suitJson("name").str)
			)
		).toSeq

		variants = variantsJSON.map(Variant.fromJSON).map(v => v.name -> v).toMap

	def apply(
		id: Int,
		name: String,
		suitNames: Vector[String],
		shorts: Option[Vector[Char]] = None,
		criticalRank: Option[Int] = None,
		clueStarved: Boolean = false,
		specialRank: Option[Int] = None,
		rainbowS: Boolean = false,
		whiteS: Boolean = false,
		pinkS: Boolean = false,
		brownS: Boolean = false,
		deceptiveS: Boolean = false,
		scarce1s: Boolean = false
	): Variant =
		val (suits, shortForms, colourableSuits) = shorts match
			case Some(shortForms) =>
				val suits = suitNames.zipWithIndex.map((name, i) => Suit(name, Some(shortForms(i)), SuitType(name)))
				val colourableSuits = suits.filterNot(suit => NO_COLOUR.matches(suit.name))
				(suits, shortForms, colourableSuits)
			case None =>
				suitNames.foldLeft((Vector.empty[Suit], Vector.empty[Char], Vector.empty[Suit])):
					case ((suits, shortForms, colourableSuits), name) =>
						val colour = Variant.colours.find(_.name == name)

						val short: Char = name match
							case "Black" => 'k'
							case "Pink" => 'i'
							case "Brown" => 'n'
							case _ =>
								val abbreviation = colour.flatMap(_.abbreviation).getOrElse(name.charAt(0).toLower)
								if !shortForms.contains(abbreviation) then
									abbreviation
								else
									name.toLowerCase.find(c => !shortForms.contains(c))
										.getOrElse:
											throw new IllegalArgumentException(s"No unused character found for suit '$name' in $suitNames!")

						val suit = colour.getOrElse:
							Suit(name, Some(short), SuitType(name))

						if NO_COLOUR.matches(name) then
							(suits :+ suit, shortForms :+ short, colourableSuits)
						else
							(suits :+ suit, shortForms :+ short, colourableSuits :+ suit)

		Variant(id, name, suits, shortForms, colourableSuits,
			criticalRank,
			clueStarved, specialRank, rainbowS, whiteS, pinkS, brownS, deceptiveS, scarce1s)

	def fromJSON(json: ujson.Value) =
		val obj = json.obj
		Variant(
			json("id").num.toInt,
			json("name").str,
			json("suits").arr.map(_.str).toVector,
			criticalRank = obj.get("criticalRank").map(_.num.toInt),
			clueStarved = obj.get("clueStarved").map(_.bool).getOrElse(false),
			specialRank = obj.get("specialRank").map(_.num.toInt),
			rainbowS = obj.get("specialRankAllClueColors").map(_.bool).getOrElse(false),
			whiteS = obj.get("specialRankNoClueColors").map(_.bool).getOrElse(false),
			pinkS = obj.get("specialRankAllClueRanks").map(_.bool).getOrElse(false),
			brownS = obj.get("specialRankNoClueRanks").map(_.bool).getOrElse(false),
			deceptiveS = obj.get("specialRankDeceptive").map(_.bool).getOrElse(false),
			scarce1s = obj.get("scarceOnes").map(_.bool).getOrElse(false)
		)

	def getVariant(name: String) =
		variants.get(name).getOrElse(throw new IllegalArgumentException(s"Variant '$name' not found! Were variants initialized?"))
