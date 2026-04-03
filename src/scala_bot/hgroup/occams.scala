package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.utils._

import scala_bot.logger.Log

def nextUnknown(fp: FocusPossibility, skipPast: Option[Connection] = None): Option[Connection] =
	val conns = skipPast.fold(fp.connections): skip =>
		fp.connections.dropWhile(_ != skip).drop(1)

	conns.find:
		case _: KnownConn => false
		case _: PlayableConn => false
		case _ => true

def fpSimplicity(fp: FocusPossibility, playerIndex: Int, ourPlayerIndex: Int): Int =
	// Note that us prompting/finessing on a clue to someone else is as "complicated"
	// as them self-finessing, since we always wait for them to demonstrate first.
	if nextUnknown(fp).exists(c => c.reacting != playerIndex && c.reacting != ourPlayerIndex) then
		0
	else
		val blindPlays = fp.connections.count(_.isInstanceOf[FinesseConn])
		val prompts = fp.connections.count(_.isInstanceOf[PromptConn])

		10 * blindPlays + prompts

def filterFps(ctx: ClueContext, fps: Seq[FocusPossibility], target: Int) =
	val ClueContext(_, game, action) = ctx
	val state = game.state
	val trueFp = state.deck(ctx.focusResult.focus).id().flatMap(id => fps.find(_.id == id))

	def selfFinesses(fp2: FocusPossibility) = fp2.connections.count:
		case f: FinesseConn => f.reacting == state.ourPlayerIndex
		case _ => false

	trueFp.fold(fps): trueFp =>
		fps.filterNot: fp =>
			// The clue target can see a clued connection in someone else's hand
			trueFp.connections.exists: conn =>
				state.deck(conn.order).clued &&
				conn.reacting != action.target &&
				conn.ids.contains(fp.id)
			||
			// There is an extra self-finesse required from us (we may not have that card)
			!fp.ambiguous && selfFinesses(trueFp) > 0 && selfFinesses(fp) > selfFinesses(trueFp)
	.pipe: fps =>
		fps.filter: fp =>
			val selfBluff = fp.connections.existsM { case f: FinesseConn => f.isBluff && f.reacting == target }

			val startsKnown = fps.exists: fp2 =>
				fp2 != fp &&
				fp2.connections.headOption.existsM:
					case _: KnownConn => true
					case _: PlayableConn => true

			// if selfBluff then
			// 	println(s"${state.logConns(fp.connections, fp.id)} self bluff! $startsKnown $fps")

			!(selfBluff && startsKnown)

def occamsRazor(ctx: ClueContext, fps: Seq[FocusPossibility], playerIndex: Int, actualId: Option[Identity] = None) =
	val ClueContext(_, game, action) = ctx
	val state = game.state

	val initial = (99, Seq.empty[FocusPossibility], Seq.empty[FocusPossibility])

	fps.foldRight(initial) { case (fp, (min, acc, impossible)) =>
		if !game.players(playerIndex).thoughts(ctx.focusResult.focus).possible.contains(fp.id) then
			(min, acc, fp +: impossible)
		else
			val simplicity = fpSimplicity(fp, playerIndex, state.ourPlayerIndex)
			// println(s"${state.logId(fp.id)} $playerIndex simplicity: $simplicity")

			if simplicity < min && actualId.forall(_ == fp.id) then
				(simplicity, List(fp), impossible)
			else if simplicity == min then
				(min, fp +: acc, impossible)
			else
				(min, acc, impossible)
	}
	.pipe: (min, acc, impossible) =>
		acc ++ impossible.filter(fpSimplicity(_, playerIndex, state.ourPlayerIndex) <= min)

	.when(_ => playerIndex == state.ourPlayerIndex): simplest =>
		// If a simplest possibility involves X finessing, followed by a self-finesse,
		// starting with the same self-finesse is equally simple.
		val sameSelfStart = fps.filter: fp =>
			!simplest.contains(fp) &&
			simplest.exists: s =>
				s.connections.exists: c =>
					c.isInstanceOf[FinesseConn] &&
					c.reacting == playerIndex &&
					nextUnknown(fp).exists(_.order == c.order)

		simplest ++ sameSelfStart

	.pipe: simplest =>
		simplest.filterNot: fp =>
			// Log.info(s"checking ${state.logConns(fp.connections, fp.id)}")
			val finesseWhenBluffExists = nextUnknown(fp).exists: conn =>
				fps.exists: fp2 =>
					fp != fp2 &&
					!fp2.symmetric &&
					nextUnknown(fp2).exists: conn2 =>
						conn2.reacting == conn.reacting &&
						conn2.matchesP { case f: FinesseConn => f.bluff } &&
						conn.matchesP { case f: FinesseConn => f.fKind == FinesseKind.True || f.fKind == FinesseKind.Hidden } &&
						// No one else can prove the finesse
						nextUnknown(fp, Some(conn)).forall(_.reacting == playerIndex)

			if finesseWhenBluffExists then
				Log.highlight(Console.CYAN, s"excluding ${state.logConns(fp.connections, fp.id)} due to bluff possibility on same player!")

			finesseWhenBluffExists
