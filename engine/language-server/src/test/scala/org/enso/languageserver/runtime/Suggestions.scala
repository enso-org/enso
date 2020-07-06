package org.enso.languageserver.runtime

import org.enso.searcher.Suggestion

/** Suggestion instances used in tests. */
object Suggestions {

  val atom: Suggestion.Atom =
    Suggestion.Atom(
      name          = "MyType",
      arguments     = Vector(Suggestion.Argument("a", "Any", false, false, None)),
      returnType    = "MyAtom",
      documentation = None
    )

  val method: Suggestion.Method =
    Suggestion.Method(
      name = "foo",
      arguments = Vector(
        Suggestion.Argument("this", "MyType", false, false, None),
        Suggestion.Argument("foo", "Number", false, true, Some("42"))
      ),
      selfType      = "MyType",
      returnType    = "Number",
      documentation = Some("Lovely")
    )

  val function: Suggestion.Function =
    Suggestion.Function(
      name       = "print",
      arguments  = Vector(),
      returnType = "IO",
      scope      = Suggestion.Scope(9, 22)
    )

  val local: Suggestion.Local =
    Suggestion.Local(
      name       = "x",
      returnType = "Number",
      scope      = Suggestion.Scope(34, 68)
    )

  val all = Seq(atom, method, function, local)
}
