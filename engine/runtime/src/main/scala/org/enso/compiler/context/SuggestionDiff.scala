package org.enso.compiler.context
import org.enso.polyglot.Suggestion
import org.enso.polyglot.data.{These, Tree}

object SuggestionDiff {

  sealed trait Op
  object Op {

    case object Add extends Op

    case object Remove extends Op

    case class Modify(scope: Option[Suggestion.Scope]) extends Op
  }

  case class SuggestionUpdate(suggestion: Suggestion, op: Op)

  def isSame(a: Suggestion, b: Suggestion): Boolean =
    a.name == b.name &&
    Suggestion.Kind(a) == Suggestion.Kind(b) &&
    Suggestion.SelfType(a) == Suggestion.SelfType(b)

  def compare(elem: These[Suggestion, Suggestion]): SuggestionUpdate = ???

  /** Compute difference between the two trees. */
  def compute(
    prev: Tree.Root[Suggestion],
    current: Tree.Root[Suggestion]
  ): Tree[SuggestionUpdate] =
    Tree.zipBy(prev, current)(isSame).map(compare)
}
