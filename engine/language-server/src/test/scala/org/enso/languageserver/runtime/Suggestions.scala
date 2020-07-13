package org.enso.languageserver.runtime

import java.util.UUID

import org.enso.polyglot.Suggestion

/** Suggestion instances used in tests. */
object Suggestions {

  val atom: Suggestion.Atom =
    Suggestion.Atom(
      externalId    = None,
      module        = "Test.Main",
      name          = "MyType",
      arguments     = Vector(Suggestion.Argument("a", "Any", false, false, None)),
      returnType    = "MyAtom",
      documentation = None
    )

  val method: Suggestion.Method =
    Suggestion.Method(
      externalId =
        Some(UUID.fromString("ea9d7734-26a7-4f65-9dd9-c648eaf57d63")),
      module = "Test.Main",
      name   = "foo",
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
      externalId =
        Some(UUID.fromString("78d452ce-ed48-48f1-b4f2-b7f45f8dff89")),
      module     = "Test.Main",
      name       = "print",
      arguments  = Vector(),
      returnType = "IO",
      scope =
        Suggestion.Scope(Suggestion.Position(1, 9), Suggestion.Position(1, 22))
    )

  val local: Suggestion.Local =
    Suggestion.Local(
      externalId =
        Some(UUID.fromString("dc077227-d9b6-4620-9b51-792c2a69419d")),
      module     = "Test.Main",
      name       = "x",
      returnType = "Number",
      scope =
        Suggestion.Scope(Suggestion.Position(21, 0), Suggestion.Position(89, 0))
    )

  val all = Seq(atom, method, function, local)
}
