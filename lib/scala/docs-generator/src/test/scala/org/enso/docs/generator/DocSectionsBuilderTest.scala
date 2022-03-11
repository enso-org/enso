package org.enso.docs.generator

import org.enso.syntax.text.ast.Doc
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DocSectionsBuilderTest extends AnyWordSpec with Matchers {

  "DocSectionsGenerator" should {

    "generate single tag" in {
      val comment =
        """ UNSTABLE
          |""".stripMargin
      val expected = List(
        DocSection.Tag("UNSTABLE", None)
      )

      DocParserWrapper.buildSections(comment) shouldEqual expected
    }

    "generate multiple tags" in {
      val comment =
        """ UNSTABLE
          | DEPRECATED
          |""".stripMargin
      val expected = List(
        DocSection.Tag("UNSTABLE", None),
        DocSection.Tag("DEPRECATED", None)
      )

      DocParserWrapper.buildSections(comment) shouldEqual expected
    }

    "generate tag with description" in {
      val comment =
        """ ALIAS Check Matches
          |""".stripMargin
      val expected = List(
        DocSection.Tag("ALIAS", Some("Check Matches"))
      )

      DocParserWrapper.buildSections(comment) shouldEqual expected
    }

    "generate description single line" in {
      val comment =
        """ hello world
          |""".stripMargin
      val expected = List(
        DocSection.Paragraph(List(Doc.Elem.Text("hello world")))
      )

      DocParserWrapper.buildSections(comment) shouldEqual expected
    }

    "generate description multiline" in {
      val comment =
        """ hello world
          | second line
          |""".stripMargin
      val expected = List(
        DocSection.Paragraph(
          List(
            Doc.Elem.Text("hello world"),
            Doc.Elem.Newline,
            Doc.Elem.Text("second line")
          )
        )
      )

      DocParserWrapper.buildSections(comment) shouldEqual expected
    }

    "generate description multiple paragraphs" in {
      val comment =
        """ Hello world
          | second line
          |
          | Second paragraph
          | multiline
          |""".stripMargin
      val expected = List(
        DocSection.Paragraph(
          List(
            Doc.Elem.Text("Hello world"),
            Doc.Elem.Newline,
            Doc.Elem.Text("second line"),
            Doc.Elem.Newline
          )
        ),
        DocSection.Paragraph(
          List(
            Doc.Elem.Text("Second paragraph"),
            Doc.Elem.Newline,
            Doc.Elem.Text("multiline")
          )
        )
      )

      DocParserWrapper.buildSections(comment) shouldEqual expected
    }

    "generate keyed arguments" in {
      val comment =
        """ Description
          |
          | Arguments:
          | - one: The first
          | - two: The second
          |""".stripMargin
      val expected = List(
        DocSection.Paragraph(
          List(Doc.Elem.Text("Description"), Doc.Elem.Newline)
        ),
        DocSection.Keyed(
          "Arguments",
          List(
            Doc.Elem.Newline,
            Doc.Elem.List(
              1,
              Doc.Elem.List.Unordered,
              Doc.Elem.Text(" one: The first"),
              Doc.Elem.Text(" two: The second")
            )
          )
        )
      )

      DocParserWrapper.buildSections(comment) shouldEqual expected
    }

    "generate keyed icon" in {
      val comment =
        """ Description
          |
          | Icon: my-icon
          |""".stripMargin
      val expected = List(
        DocSection.Paragraph(
          List(Doc.Elem.Text("Description"), Doc.Elem.Newline)
        ),
        DocSection.Keyed(
          "Icon",
          List(Doc.Elem.Text("my-icon"))
        )
      )

      DocParserWrapper.buildSections(comment) shouldEqual expected
    }

    "generate keyed aliases" in {
      val comment =
        """ Description
          |
          | Aliases: foo, bar baz, redshift®
          |""".stripMargin
      val expected = List(
        DocSection.Paragraph(
          List(Doc.Elem.Text("Description"), Doc.Elem.Newline)
        ),
        DocSection.Keyed(
          "Aliases",
          List(Doc.Elem.Text("foo, bar baz, redshift"), Doc.Elem.Text("®"))
        )
      )

      DocParserWrapper.buildSections(comment) shouldEqual expected
    }

    "generate marked example" in {
      val comment =
        """ Description
          |
          | > Example
          |   Simple program
          |       main = 42
          |""".stripMargin
      val expected = List(
        DocSection.Paragraph(
          List(Doc.Elem.Text("Description"), Doc.Elem.Newline)
        ),
        DocSection.Marked(
          DocSection.Mark.Example,
          Some("Example"),
          List(
            Doc.Elem.Newline,
            Doc.Elem.Text("Simple program"),
            Doc.Elem.Newline,
            Doc.Elem.CodeBlock(Doc.Elem.CodeBlock.Line(7, "main = 42"))
          )
        )
      )

      DocParserWrapper.buildSections(comment) shouldEqual expected
    }

    "generate marked multiple examples" in {
      val comment =
        """ Description
          |
          | > Example
          |   Simple program
          |   Multiline
          |       main = 42
          |
          | > Example
          |   Another example
          |
          |       import Foo.Bar
          |
          |       main =
          |           42
          |""".stripMargin
      val expected = List(
        DocSection.Paragraph(
          List(Doc.Elem.Text("Description"), Doc.Elem.Newline)
        ),
        DocSection.Marked(
          DocSection.Mark.Example,
          Some("Example"),
          List(
            Doc.Elem.Newline,
            Doc.Elem.Text("Simple program"),
            Doc.Elem.Newline,
            Doc.Elem.Text("Multiline"),
            Doc.Elem.Newline,
            Doc.Elem.CodeBlock(Doc.Elem.CodeBlock.Line(7, "main = 42")),
            Doc.Elem.Newline
          )
        ),
        DocSection.Marked(
          DocSection.Mark.Example,
          Some("Example"),
          List(
            Doc.Elem.Newline,
            Doc.Elem.Text("Another example"),
            Doc.Elem.Newline,
            Doc.Elem.CodeBlock(
              Doc.Elem.CodeBlock.Line(7, "import Foo.Bar"),
              Doc.Elem.CodeBlock.Line(7, "main ="),
              Doc.Elem.CodeBlock.Line(11, "42")
            )
          )
        )
      )

      DocParserWrapper.buildSections(comment) shouldEqual expected
    }

    "generate marked important" in {
      val comment =
        """ Description
          |
          | ! This is important
          |   Beware of nulls.
          |""".stripMargin
      val expected = List(
        DocSection.Paragraph(
          List(Doc.Elem.Text("Description"), Doc.Elem.Newline)
        ),
        DocSection.Marked(
          DocSection.Mark.Important,
          Some("This is important"),
          List(
            Doc.Elem.Newline,
            Doc.Elem.Text("Beware of nulls.")
          )
        )
      )

      DocParserWrapper.buildSections(comment) shouldEqual expected
    }

    "generate marked info" in {
      val comment =
        """ Description
          |
          | ? Out of curiosity
          |   FYI.
          |""".stripMargin
      val expected = List(
        DocSection.Paragraph(
          List(Doc.Elem.Text("Description"), Doc.Elem.Newline)
        ),
        DocSection.Marked(
          DocSection.Mark.Info,
          Some("Out of curiosity"),
          List(
            Doc.Elem.Newline,
            Doc.Elem.Text("FYI.")
          )
        )
      )

      DocParserWrapper.buildSections(comment) shouldEqual expected
    }

    "generate marked info multiple sections" in {
      // TODO[DB] Marked section can not be split into paragraphs.
      // It is parsed as code block.
      pending
      val comment =
        """ Description
          |
          | ? Out of curiosity
          |   FYI.
          |   
          |   Another section.
          |""".stripMargin
      val expected = List(
        DocSection.Paragraph(
          List(Doc.Elem.Text("Description"), Doc.Elem.Newline)
        ),
        DocSection.Marked(
          DocSection.Mark.Info,
          Some("Out of curiosity"),
          List(
            Doc.Elem.Newline,
            Doc.Elem.Text("FYI."),
            Doc.Elem.Newline,
            Doc.Elem.CodeBlock(Doc.Elem.CodeBlock.Line(3, "Another section."))
          )
        )
      )

      DocParserWrapper.buildSections(comment) shouldEqual expected
    }

    "generate multiple sections" in {
      val comment =
        """ DEPRECATED
          |
          | Some paragraph
          | Second line
          | 
          | Arguments:
          | - one: The first
          | - two: The second
          |
          | ! This is important
          |   Boo.
          |
          | ? Out of curiosity
          |   FYI.
          |""".stripMargin
      val expected = List(
        DocSection.Tag("DEPRECATED", None),
        DocSection.Paragraph(List(Doc.Elem.Newline)),
        DocSection.Paragraph(
          List(
            Doc.Elem.Text("Some paragraph"),
            Doc.Elem.Newline,
            Doc.Elem.Text("Second line"),
            Doc.Elem.Newline
          )
        ),
        DocSection.Keyed(
          "Arguments",
          List(
            Doc.Elem.Newline,
            Doc.Elem.List(
              1,
              Doc.Elem.List.Unordered,
              Doc.Elem.Text(" one: The first"),
              Doc.Elem.Text(" two: The second")
            ),
            Doc.Elem.Newline
          )
        ),
        DocSection.Marked(
          DocSection.Mark.Important,
          Some("This is important"),
          List(
            Doc.Elem.Newline,
            Doc.Elem.Text("Boo."),
            Doc.Elem.Newline
          )
        ),
        DocSection.Marked(
          DocSection.Mark.Info,
          Some("Out of curiosity"),
          List(
            Doc.Elem.Newline,
            Doc.Elem.Text("FYI.")
          )
        )
      )

      DocParserWrapper.buildSections(comment) shouldEqual expected
    }
  }

}
