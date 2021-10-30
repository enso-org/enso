package org.enso.flexer

import org.enso.flexer.automata.NFA
import org.enso.flexer.automata.Pattern
import org.enso.flexer.state.Rule

import scala.reflect.macros.blackbox.Context

class State(val label: String, val ix: Int, val finish: () => Unit) {
  var parent: Option[State]        = None
  private var revRules: List[Rule] = List()

  def parent_=(p: State): Unit =
    parent = Some(p)

  def addRule(rule: Rule): Unit =
    revRules = rule +: revRules

  def rule(expr: Pattern): Rule.Builder =
    Rule.Builder(expr, addRule)

  def ||(expr: Pattern): Rule.Builder =
    rule(expr)

  def rules: List[Rule] = {
    val myRules = revRules.reverse
    parent.map(myRules ++ _.rules).getOrElse(myRules)
  }

  private def ruleName(ruleIx: Int): String =
    s"group${ix}_rule$ruleIx"

  private def buildAutomata(): NFA = {
    val nfa   = new NFA
    val start = nfa.addState()
    val endpoints = rules.zipWithIndex.map { case (rule, ix) =>
      buildRuleAutomata(nfa, start, ix, rule)
    }
    val end = nfa.addState()
    nfa.state(end).rule = Some("")
    for (endpoint <- endpoints) {
      nfa.link(endpoint, end)
    }
    nfa
  }

  def buildRuleAutomata(nfa: NFA, last: Int, ruleIx: Int, rule: Rule): Int = {
    val end = buildExprAutomata(nfa, last, rule.pattern)
    nfa.state(end).rule = Some(ruleName(ruleIx))
    end
  }

  def buildExprAutomata(nfa: NFA, last: Int, expr: Pattern): Int = {
    import Pattern._
    val current = nfa.addState()
    nfa.link(last, current)
    expr match {
      case Always => current
      case Range(start, end) =>
        val state = nfa.addState()
        nfa.link(current, state, scala.Range(start, end))
        state
      case Seq(first, second) =>
        val s1 = buildExprAutomata(nfa, current, first)
        buildExprAutomata(nfa, s1, second)
      case Many(body) =>
        val s1 = nfa.addState()
        val s2 = buildExprAutomata(nfa, s1, body)
        val s3 = nfa.addState()
        nfa.link(current, s1)
        nfa.link(current, s3)
        nfa.link(s2, s3)
        nfa.link(s3, s1)
        s3
      case Or(first, second) =>
        val s1 = buildExprAutomata(nfa, current, first)
        val s2 = buildExprAutomata(nfa, current, second)
        val s3 = nfa.addState()
        nfa.link(s1, s3)
        nfa.link(s2, s3)
        s3
    }
  }

  def generate[C <: Context](c: C): c.Tree = {
    import c.universe._

    val nfa   = buildAutomata()
    val dfa   = nfa.toDFA()
    val state = Spec[c.type](c, dfa).generate(ix)
    val rs = rules.zipWithIndex.map { case (rule, ruleIx) =>
      val tree = c.parse(rule.tree)
      q"def ${TermName(ruleName(ruleIx))}() = $tree"
    }
    q"..$state; ..$rs"
  }
}
