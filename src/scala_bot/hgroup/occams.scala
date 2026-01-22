package scala_bot.hgroup

import scala_bot.basics._
import scala.util.chaining.scalaUtilChainingOps

def fpSimplicity(fp: FocusPossibility, playerIndex: Int): Int =
	val startsOther = fp.connections.find:
		case _: KnownConn => false
		case _: PlayableConn => false
		case _ => true
	.exists(_.reacting != playerIndex)

	if startsOther then
		0
	else
		val blindPlays = fp.connections.count(_.isInstanceOf[FinesseConn])
		val prompts = fp.connections.count(_.isInstanceOf[PromptConn])

		10 * blindPlays + prompts

def occamsRazor(game: Game, fps: Seq[FocusPossibility], playerIndex: Int, focus: Int, actualId: Option[Identity] = None) =
	val state = game.state

	val initial = (99, Seq.empty[FocusPossibility], Seq.empty[FocusPossibility])
	val simplest = fps.foldRight(initial) { case (fp, (min, acc, impossible)) =>
		if !game.players(playerIndex).thoughts(focus).possible.contains(fp.id) then
			(min, acc, fp +: impossible)
		else
			val simplicity = fpSimplicity(fp, playerIndex)
			// println(s"${state.logId(fp.id)} $playerIndex simplicity: $simplicity")

			if simplicity < min && actualId.forall(_ == fp.id) then
				(simplicity, List(fp), impossible)
			else if simplicity == min then
				(min, fp +: acc, impossible)
			else
				(min, acc, impossible)
	}
	.pipe: (min, acc, impossible) =>
		acc ++ impossible.filter(fpSimplicity(_, playerIndex) <= min)

	if playerIndex != state.ourPlayerIndex then
		// No one else will play into it if they can fulfill it themselves.
		simplest
	else
		// If a simplest possibility involves X finessing, followed by a self-finesse,
		// starting with the same self-finesse is equally simple.
		val sameSelfStart = fps.filter: fp =>
			!simplest.contains(fp) &&
			simplest.exists: s =>
				s.connections.exists: c =>
					c.isInstanceOf[FinesseConn] &&
					c.reacting == playerIndex &&
					fp.connections.find:
						case _: KnownConn => false
						case _: PlayableConn => false
						case _ => true
					.exists(_.order == c.order)


		simplest ++ sameSelfStart
