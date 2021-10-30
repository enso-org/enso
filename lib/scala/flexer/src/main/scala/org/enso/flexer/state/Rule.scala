package org.enso.flexer.state

import org.enso.flexer.automata.Pattern
import org.enso.flexer.spec.Macro

final case class Rule(pattern: Pattern, tree: String)
object Rule {
  final case class Builder(pattern: Pattern, finalizer: Rule => Unit) {
    def run(program: String): Unit = finalizer(Rule(pattern, program))
    def ||(program: => Unit): Unit = macro Macro.runRule
  }
}
