package org.enso.flexer

import org.enso.flexer.automata.DFA
import org.enso.flexer.automata.State

import scala.collection.immutable.Range
import scala.collection.mutable
import scala.reflect.macros.blackbox.Context

/** Creates update functions for given DFA ~ nextState : state -> state.
  * Each state has a pattern match on current utf code point.
  * ASCII characters are explicitly matched, so that we get O(1) lookup.
  * The rest of UTF characters is dispatched by tree of if-else,
  * with O(log(N)) lookup.
  */
case class Spec[C <: Context](c: C, dfa: DFA) {
  import c.universe._
  import Spec._

  val stateHasOverlappingRules = mutable.Map(0 -> false)

  case class Branch(range: Range, body: Tree)

  def genBranchBody(
    targetState: Int,
    maybeState: Option[State.Desc],
    rulesOverlap: Boolean
  ): Tree = (targetState, maybeState, rulesOverlap) match {
    case (State.missing, None, _) =>
      Literal(Constant(Parser.State.Status.Exit.FAIL))
    case (State.missing, Some(state), false) =>
      q"state.call(${TermName(state.rule)})"
    case (State.missing, Some(state), true) =>
      q"reader.rewind.rule.run(); state.call(${TermName(state.rule)})"

    case _ =>
      val targetStateHasNoRule = maybeState match {
        case Some(state) if !dfa.endStatePriorityMap.contains(targetState) =>
          dfa.endStatePriorityMap += targetState  -> state
          stateHasOverlappingRules += targetState -> true
          true
        case _ => false
      }
      val trgState = Literal(Constant(targetState))
      if (targetStateHasNoRule && !rulesOverlap)
        q"reader.rewind.rule.set(); $trgState"
      else
        q"$trgState"
  }

  def genSwitch(branchs: Seq[Branch]): Seq[CaseDef] = {
    branchs.map {
      case Branch(range, body) =>
        val pattern =
          Alternative(range.map(i => q"${Literal(Constant(i))}").toList)
        cq"$pattern => $body"
    }
  }

  def genIf(branchs: Seq[Branch]): Branch = {
    branchs match {
      case b +: Seq() => b
      case a +: b +: rest =>
        val range = a.range.start to b.range.end
        val body  = q"if (charCode <= ${a.range.end}) ${a.body} else ${b.body}"
        genIf(Branch(range, body) +: rest)
    }
  }

  def generateCaseBody(stateIx: Int): Tree = {
    val overlaps = stateHasOverlappingRules.getOrElse(stateIx, false)
    val state    = dfa.endStatePriorityMap.get(stateIx)
    var trgState = dfa.links(stateIx)(0)
    var rStart   = Int.MinValue
    val branches = dfa.vocabulary.toVector.flatMap {
      case (range, vocIx) =>
        val newTrgState = dfa.links(stateIx)(vocIx)
        if (newTrgState != trgState) {
          val rEnd      = range.start - 1
          val xtrgState = trgState
          val xrStart   = rStart
          trgState = newTrgState
          rStart   = range.start
          Some(
            Branch(xrStart to rEnd, genBranchBody(xtrgState, state, overlaps))
          )
        } else None
    }

    val allBranches = branches :+
      Branch(rStart to Int.MaxValue, genBranchBody(trgState, state, overlaps))

    val (utf1 :+ b1, rest) = allBranches.span(_.range.start < MIN_MATCH_CODE)
    val (asci, utf2)       = rest.span(_.range.end <= MAX_MATCH_CODE)

    utf2 match {
      case b2 +: utf2 =>
        val b1UTF = Branch(b1.range.start until MIN_MATCH_CODE, b1.body)
        val b1ASC = Branch(MIN_MATCH_CODE to b1.range.end, b1.body)
        val b2ASC = Branch(b2.range.start to MAX_MATCH_CODE, b2.body)
        val b2UTF = Branch(MAX_MATCH_CODE + 1 to b2.range.end, b2.body)

        val emptyB1ASC = b1ASC.range.end < MIN_MATCH_CODE
        val emptyB2UTF = b2UTF.range.start <= MAX_MATCH_CODE

        val ascii     = if (emptyB1ASC) asci :+ b2ASC else b1ASC +: asci :+ b2ASC
        val utfMiddle = if (emptyB2UTF) Vector(b1UTF) else Vector(b1UTF, b2UTF)
        val utf       = utf1 ++ utfMiddle ++ utf2
        val body      = genSwitch(ascii) :+ cq"charCode => ${genIf(utf).body}"

        q"${Match(q"reader.charCode", body.toList)}"
      case _ =>
        genIf(utf1 :+ b1).body
    }
  }

  def generate(i: Int): Tree = {
    val stateNames =
      dfa.links.indices.toList
        .map(st => (st, TermName(s"state${i}_${st}")))

    val stateMatch = Match(q"state", stateNames.map {
      case (st, fun) => cq"$st => $fun"
    })
    val stateBodies = stateNames.map {
      case (st, fun) => q"def $fun = {${generateCaseBody(st)}}"
    }
    q"""
      stateDefs($i) = ${TermName(s"nextState$i")}
      def ${TermName(s"nextState$i")}(state: Int): Int = $stateMatch
      ..$stateBodies
    """
  }

}

object Spec {

  /** Covers all ASCII characters (0 - 255) and End Of Input (-1) */
  val MIN_MATCH_CODE = -1
  val MAX_MATCH_CODE = 255
}
