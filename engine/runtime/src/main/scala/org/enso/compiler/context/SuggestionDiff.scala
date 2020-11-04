package org.enso.compiler.context
import org.enso.polyglot.Suggestion
import org.enso.polyglot.data.{These, Tree}

object SuggestionDiff {

  sealed trait Op
  object Op {

    case object Add extends Op

    case object Remove extends Op

    case class Modify(
      externalId: Option[Option[Suggestion.ExternalId]] = None,
      arguments: Option[Seq[Suggestion.Argument]]       = None,
      returnType: Option[String]                        = None,
      documentation: Option[Option[String]]             = None,
      scope: Option[Suggestion.Scope]                   = None
    ) extends Op {

      def isEmpty: Boolean =
        arguments.isEmpty &&
        returnType.isEmpty &&
        documentation.isEmpty &&
        scope.isEmpty
    }
  }

  case class SuggestionUpdate(suggestion: Suggestion, op: Op)

  /** Compute difference between the two trees.
    *
    * @param prev the tree before
    * @param current the tree after
    * @return the tree with updates
    */
  def compute(
    prev: Tree.Root[Suggestion],
    current: Tree.Root[Suggestion]
  ): Tree[SuggestionUpdate] =
    Tree.zipBy(prev, current)(compare).map(diff)

  private def compare(a: Suggestion, b: Suggestion): Boolean =
    a.name == b.name &&
    a.module == b.module &&
    Suggestion.Kind(a) == Suggestion.Kind(b) &&
    Suggestion.SelfType(a) == Suggestion.SelfType(b)

  private def diff(elem: These[Suggestion, Suggestion]): SuggestionUpdate =
    elem match {
      case These.Here(e) =>
        SuggestionUpdate(e, Op.Remove)
      case These.There(e) =>
        SuggestionUpdate(e, Op.Add)
      case These.Both(e1: Suggestion.Atom, e2: Suggestion.Atom) =>
        diffAtoms(e1, e2)
      case These.Both(e1: Suggestion.Method, e2: Suggestion.Method) =>
        diffMethods(e1, e2)
      case These.Both(e1: Suggestion.Function, e2: Suggestion.Function) =>
        diffFunctions(e1, e2)
      case These.Both(e1: Suggestion.Local, e2: Suggestion.Local) =>
        diffLocals(e1, e2)
      case These.Both(e1, e2) =>
        // this case is not possible because we group suggestions by kind in
        // `compare` method.
        throw new IllegalStateException(s"Illegal diff: $e1, $e2")
    }

  private def diffAtoms(
    e1: Suggestion.Atom,
    e2: Suggestion.Atom
  ): SuggestionUpdate = {
    var op = Op.Modify()
    if (e1.externalId != e2.externalId) {
      op = op.copy(externalId = Some(e2.externalId))
    }
    if (e1.arguments != e2.arguments) {
      op = op.copy(arguments = Some(e2.arguments))
    }
    if (e1.returnType != e2.returnType) {
      op = op.copy(returnType = Some(e2.returnType))
    }
    if (e1.documentation != e2.documentation) {
      op = op.copy(documentation = Some(e2.documentation))
    }
    SuggestionUpdate(e1, op)
  }

  private def diffMethods(
    e1: Suggestion.Method,
    e2: Suggestion.Method
  ): SuggestionUpdate = {
    var op = Op.Modify()
    if (e1.externalId != e2.externalId) {
      op = op.copy(externalId = Some(e2.externalId))
    }
    if (e1.arguments != e2.arguments) {
      op = op.copy(arguments = Some(e2.arguments))
    }
    if (e1.returnType != e2.returnType) {
      op = op.copy(returnType = Some(e2.returnType))
    }
    if (e1.documentation != e2.documentation) {
      op = op.copy(documentation = Some(e2.documentation))
    }
    SuggestionUpdate(e1, op)
  }

  private def diffFunctions(
    e1: Suggestion.Function,
    e2: Suggestion.Function
  ): SuggestionUpdate = {
    var op = Op.Modify()
    if (e1.externalId != e2.externalId) {
      op = op.copy(externalId = Some(e2.externalId))
    }
    if (e1.arguments != e2.arguments) {
      op = op.copy(arguments = Some(e2.arguments))
    }
    if (e1.returnType != e2.returnType) {
      op = op.copy(returnType = Some(e2.returnType))
    }
    if (e1.scope != e2.scope) {
      op = op.copy(scope = Some(e2.scope))
    }
    SuggestionUpdate(e1, op)
  }

  private def diffLocals(
    e1: Suggestion.Local,
    e2: Suggestion.Local
  ): SuggestionUpdate = {
    var op = Op.Modify()
    if (e1.externalId != e2.externalId) {
      op = op.copy(externalId = Some(e2.externalId))
    }
    if (e1.returnType != e2.returnType) {
      op = op.copy(returnType = Some(e2.returnType))
    }
    if (e1.scope != e2.scope) {
      op = op.copy(scope = Some(e2.scope))
    }
    SuggestionUpdate(e1, op)
  }
}
