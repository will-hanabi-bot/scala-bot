package scala_bot.hgroup

import scala_bot.basics._

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

def occamsRazor(state: State, fps: Seq[FocusPossibility], playerIndex: Int, actualId: Option[Identity] = None) =
	val simplest = fps.foldRight((99, Seq.empty[FocusPossibility])) { case (fp, (min, acc)) =>
		val simplicity = fpSimplicity(fp, playerIndex)
		// println(s"${state.logId(fp.id)} $playerIndex simplicity: $simplicity")

		if simplicity < min && actualId.forall(_ == fp.id) then
			(simplicity, List(fp))
		else if simplicity == min then
			(min, fp +: acc)
		else
			(min, acc)
	}._2

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
