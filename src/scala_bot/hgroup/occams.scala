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

def fpSimplicity(state: State, fp: FocusPossibility, playerIndex: Int, ourPlayerIndex: Int): Double =
	(if state.isInverted(fp.id) && !fp.save then 0.1 else 0) +
	{
		val nextUnknownConn = nextUnknown(fp)
		// Note that us prompting/finessing on a clue to someone else is as "complicated"
		// as them self-finessing, since we always wait for them to demonstrate first.
		if nextUnknownConn.forall(c => c.reacting != playerIndex && c.reacting != ourPlayerIndex) then
			0
		else if nextUnknownConn.exists(_.reacting == playerIndex) then
			val consecutiveConns = nextUnknownConn.fold(Nil)(conn => fp.connections.dropWhile(_ != conn).takeWhile(_.reacting == playerIndex))
			val blindPlays = consecutiveConns.count(_.isInstanceOf[FinesseConn])
			val prompts = consecutiveConns.foldLeft(0.0): (acc, conn) =>
				conn match
					case p: PromptConn => acc + 1 + (100 - p.order) / 100.0
					case _ => acc

			10 * blindPlays + prompts
		else
			val consecutiveConns = nextUnknownConn.fold(Nil)(conn => fp.connections.dropWhile(_ != conn).takeWhile(_.reacting == ourPlayerIndex))
			val blindPlays = consecutiveConns.count(_.isInstanceOf[FinesseConn])
			val prompts = consecutiveConns.foldLeft(0.0): (acc, conn) =>
				conn match
					case p: PromptConn => acc + 1 + (100 - p.order) / 100.0
					case _ => acc

			1000 * blindPlays + 100 * prompts
	}

def filterFps(ctx: ClueContext, fps: Seq[FocusPossibility]) =
	val ClueContext(_, game, action, _) = ctx
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

def occamsRazor(ctx: ClueContext, fps: Seq[FocusPossibility], playerIndex: Int, actualId: Option[Identity] = None) =
	val ClueContext(_, game, action, _) = ctx
	val state = game.state

	val initial = (Double.MaxValue, Seq.empty[FocusPossibility], Seq.empty[FocusPossibility])
	val sorted = fps.sortBy(fp => if actualId.forall(_ == fp.id) then -1 else 0)

	sorted.foldLeft(initial) { case ((min, acc, impossible), fp) =>
		if !game.players(playerIndex).thoughts(ctx.focusResult.focus).possible.contains(fp.id) then
			(min, acc, fp +: impossible)
		else
			val simplicity = fpSimplicity(state, fp, playerIndex, state.ourPlayerIndex)
			// Log.info(s"${state.logId(fp.id)} $playerIndex simplicity: $simplicity")

			if simplicity < min && actualId.forall(_ == fp.id) then
				(simplicity, List(fp), impossible)
			else if simplicity == min then
				(min, fp +: acc, impossible)
			else
				(min, acc, impossible)
	}
	.pipe: (min, acc, impossible) =>
		acc.reverse ++ impossible.filter(fpSimplicity(state, _, playerIndex, state.ourPlayerIndex) == min)

	.when(_ => playerIndex == state.ourPlayerIndex): simplest =>
		// If a simplest possibility involves X finessing, followed by a self-finesse,
		// starting with the same self-finesse is equally simple.
		val sameSelfStart = fps.filter: fp =>
			!simplest.contains(fp) &&
			simplest.exists: s =>
				s.connections.existsM:
					case c: FinesseConn => c.reacting == playerIndex && nextUnknown(fp).exists(_.order == c.order)
					case c: PlayableConn => c.insertingInto.nonEmpty && c.reacting == playerIndex && nextUnknown(fp).exists(_.order == c.order)

		simplest ++ sameSelfStart.map(_.copy(complicated = true))

	.pipe: simplest =>
		simplest.filterNot: fp =>
			// Log.info(s"checking ${state.logConns(fp.connections, fp.id)}")

			val nextSelfConn = fp.connections.zipWithIndex.find: (c, _) =>
				c.reacting == action.target && !(c.isInstanceOf[KnownConn] || c.isInstanceOf[PlayableConn])

			val dominated = nextSelfConn match
				case Some(f: FinesseConn, index) if f.reacting == action.target =>
					fps.find: fp2 =>
						fp != fp2 &&
						!fp2.symmetric &&
						fp2.connections.length == index &&
						// All of the other connections exist in this focus possibility before the self-connection
						fp2.connections.zipWithIndex.forall: (c, i) =>
							val c2 = fp.connections(i)
							c2.order == c.order && c.ids.forall(c2.ids.contains)
				case _ => None

			dominated match
				case Some(d) => Log.highlight(Console.CYAN, s"excluding ${state.logConns(fp.connections, fp.id)} due to strictly simpler possibility ${state.logConns(d.connections, d.id)}!")
				case None => ()

			dominated.isDefined
