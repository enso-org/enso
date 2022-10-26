package org.enso.languageserver.search

import org.enso.docs.generator.DocsGenerator
import org.enso.polyglot.Suggestion

import java.util.UUID

/** Suggestion instances used in tests. */
object Suggestions {

  object comment {

    val atom =
      """ PRIVATE
        |
        | A key-value store. This type assumes all keys are pairwise comparable,
        | using the `<`, `>` and `==` operators.
        |
        | Arguments:
        | - one: The first.
        | - two_three: The *second*.
        |
        | ? Info
        |   Here is a thing.
        |""".stripMargin.linesIterator.mkString("\n")
  }

  val htmlDocsGenerator: DocsGenerator =
    DocsGenerator
  val docSectionsBuilder: DocSectionsBuilder =
    DocSectionsBuilder()

  val module: Suggestion.Module = Suggestion.Module(
    module                = "local.Test.Main",
    documentation         = Some("Module doc"),
    documentationHtml     = None,
    documentationSections = Some(docSectionsBuilder.build("Module doc"))
  )

  val atom: Suggestion.Atom = Suggestion.Atom(
    externalId            = None,
    module                = "local.Test.Main",
    name                  = "MyType",
    arguments             = Vector(Suggestion.Argument("a", "Any", false, false, None)),
    returnType            = "MyAtom",
    documentation         = Some(comment.atom),
    documentationHtml     = None,
    documentationSections = Some(docSectionsBuilder.build(comment.atom))
  )

  val method: Suggestion.Method = Suggestion.Method(
    externalId = Some(UUID.fromString("ea9d7734-26a7-4f65-9dd9-c648eaf57d63")),
    module     = "local.Test.Main",
    name       = "foo",
    arguments = Vector(
      Suggestion.Argument("this", "MyType", false, false, None),
      Suggestion.Argument("foo", "Number", false, true, Some("42"))
    ),
    selfType              = "MyType",
    returnType            = "Number",
    isStatic              = false,
    documentation         = Some("Lovely"),
    documentationHtml     = None,
    documentationSections = Some(docSectionsBuilder.build("Lovely"))
  )

  val function: Suggestion.Function = Suggestion.Function(
    externalId = Some(UUID.fromString("78d452ce-ed48-48f1-b4f2-b7f45f8dff89")),
    module     = "local.Test.Main",
    name       = "print",
    arguments = Vector(
      Suggestion.Argument("a", "Any", false, false, None),
      Suggestion.Argument("b", "Any", true, false, None),
      Suggestion.Argument("c", "Any", false, true, Some("C"))
    ),
    returnType = "IO",
    scope =
      Suggestion.Scope(Suggestion.Position(1, 9), Suggestion.Position(1, 22))
  )

  val local: Suggestion.Local = Suggestion.Local(
    externalId = Some(UUID.fromString("dc077227-d9b6-4620-9b51-792c2a69419d")),
    module     = "local.Test.Main",
    name       = "x",
    returnType = "Number",
    scope =
      Suggestion.Scope(Suggestion.Position(21, 0), Suggestion.Position(89, 0))
  )

  val methodOnAny: Suggestion.Method = Suggestion.Method(
    externalId = Some(UUID.fromString("6cfe1538-5df7-42e4-bf55-64f8ac2ededa")),
    module     = "Standard.Base.Data.Any.Extensions",
    name       = "<<",
    arguments = Vector(
      Suggestion.Argument("this", "Any", false, false, None),
      Suggestion.Argument("that", "Any", false, false, None)
    ),
    selfType              = "Any",
    returnType            = "Any",
    isStatic              = false,
    documentation         = Some("Lovely"),
    documentationHtml     = None,
    documentationSections = Some(docSectionsBuilder.build("Lovely"))
  )

  val methodOnNumber: Suggestion.Method = Suggestion.Method(
    externalId = Some(UUID.fromString("33b426aa-2f74-42c0-9032-1159b5386eac")),
    module     = "Standard.Base.Data.Number.Extensions",
    name       = "asin",
    arguments = Vector(
      Suggestion.Argument("this", "Number", false, false, None)
    ),
    selfType              = "Number",
    returnType            = "Number",
    isStatic              = false,
    documentation         = None,
    documentationHtml     = None,
    documentationSections = None
  )

  val methodOnInteger: Suggestion.Method = Suggestion.Method(
    externalId = Some(UUID.fromString("2849c0f0-3c27-44df-abcb-5c163dd7ac91")),
    module     = "Standard.Builtins.Main",
    name       = "+",
    arguments = Vector(
      Suggestion.Argument("that", "Number", false, false, None)
    ),
    selfType              = "Integer",
    returnType            = "Number",
    isStatic              = false,
    documentation         = Some("Blah, blah"),
    documentationHtml     = None,
    documentationSections = Some(docSectionsBuilder.build("Blah, blah"))
  )

  val all = Seq(
    module,
    atom,
    method,
    function,
    local,
    methodOnAny,
    methodOnNumber,
    methodOnInteger
  )
}
