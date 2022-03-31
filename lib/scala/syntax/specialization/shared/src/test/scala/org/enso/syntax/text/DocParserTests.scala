package org.enso.syntax.text

import org.enso.syntax.text.ast.Doc
import org.enso.syntax.text.ast.Doc._
import org.enso.syntax.text.ast.Doc.Elem._
import org.enso.Logger
import org.enso.flexer.Parser.Result
import org.enso.flexer.Reader
import org.enso.syntax.text.ast.Doc.Tags.Tag
import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DocParserTests extends AnyFlatSpec with Matchers {
  val logger = new Logger()

  def assertExpr(
    input: String,
    result: Doc,
    assertShow: Boolean = true
  ): Assertion = {
    val output = DocParser.run(input)
    output match {
      case Result(_, Result.Success(value)) =>
        if (assertShow) {
          assert(value.show() == new Reader(input).toString())
        }
        assert(value == result)
      case _ =>
        fail(s"Parsing documentation failed, consumed ${output.offset} chars")
    }
  }

  implicit class TestString(input: String) {
    def parseDocumentation(str: String): String = {
      val escape = (str: String) => str.replace("\n", "\\n")
      s"parse `${escape(str)}`"
    }

    private val testBase = it should parseDocumentation(input)

    def ?=(out: Doc): Unit = testBase in {
      assertExpr(input, out)
    }
    def ?==(out: Doc): Unit = testBase in {
      assertExpr(input, out, assertShow = false)
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Formatters //////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  "Foo"             ?= Doc(Synopsis(Section.Raw("Foo")))
  "Foo\uD83D\uDC98" ?= Doc(Synopsis(Section.Raw("Foo", "\uD83D\uDC98")))
  "*Foo*"           ?= Doc(Synopsis(Section.Raw(Formatter(Formatter.Bold, "Foo"))))
  "_Foo_"           ?= Doc(Synopsis(Section.Raw(Formatter(Formatter.Italic, "Foo"))))
  "~Foo~" ?= Doc(
    Synopsis(Section.Raw(Formatter(Formatter.Strikeout, "Foo")))
  )
  "`Foo`" ?= Doc(Synopsis(Section.Raw(CodeBlock.Inline("Foo"))))
  "~*Foo*~" ?= Doc(
    Synopsis(
      Section.Raw(
        Formatter(Formatter.Strikeout, Formatter(Formatter.Bold, "Foo"))
      )
    )
  )
  "~_Foo_~" ?= Doc(
    Synopsis(
      Section.Raw(
        Formatter(Formatter.Strikeout, Formatter(Formatter.Italic, "Foo"))
      )
    )
  )
  "_~Foo~_" ?= Doc(
    Synopsis(
      Section.Raw(
        Formatter(Formatter.Italic, Formatter(Formatter.Strikeout, "Foo"))
      )
    )
  )
  "_*Foo*_" ?= Doc(
    Synopsis(
      Section.Raw(
        Formatter(Formatter.Italic, Formatter(Formatter.Bold, "Foo"))
      )
    )
  )
  "*_Foo_*" ?= Doc(
    Synopsis(
      Section.Raw(
        Formatter(Formatter.Bold, Formatter(Formatter.Italic, "Foo"))
      )
    )
  )
  "*~Foo~*" ?= Doc(
    Synopsis(
      Section.Raw(
        Formatter(Formatter.Bold, Formatter(Formatter.Strikeout, "Foo"))
      )
    )
  )
  "_~*Foo*~_" ?= Doc(
    Synopsis(
      Section.Raw(
        Formatter(
          Formatter.Italic,
          Formatter(Formatter.Strikeout, Formatter(Formatter.Bold, "Foo"))
        )
      )
    )
  )
  "`import foo`" ?= Doc(
    Synopsis(
      Section.Raw(CodeBlock.Inline("import foo"))
    )
  )

  //////////////////////////////////////////////////////////////////////////////
  //// Unclosed formatters /////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  "_*Foo*" ?= Doc(
    Synopsis(
      Section.Raw(
        Formatter.Unclosed(Formatter.Italic, Formatter(Formatter.Bold, "Foo"))
      )
    )
  )
  "~*Foo*" ?= Doc(
    Synopsis(
      Section.Raw(
        Formatter
          .Unclosed(Formatter.Strikeout, Formatter(Formatter.Bold, "Foo"))
      )
    )
  )
  "***Foo" ?= Doc(
    Synopsis(
      Section.Raw(
        Formatter(Formatter.Bold),
        Formatter.Unclosed(Formatter.Bold, "Foo")
      )
    )
  )
  "*_Foo_" ?= Doc(
    Synopsis(
      Section.Raw(
        Formatter.Unclosed(Formatter.Bold, Formatter(Formatter.Italic, "Foo"))
      )
    )
  )
  "~_Foo_" ?= Doc(
    Synopsis(
      Section.Raw(
        Formatter
          .Unclosed(Formatter.Strikeout, Formatter(Formatter.Italic, "Foo"))
      )
    )
  )
  "___Foo" ?= Doc(
    Synopsis(
      Section.Raw(
        Formatter(Formatter.Italic),
        Formatter.Unclosed(Formatter.Italic, "Foo")
      )
    )
  )
  "*~Foo~" ?= Doc(
    Synopsis(
      Section.Raw(
        Formatter
          .Unclosed(Formatter.Bold, Formatter(Formatter.Strikeout, "Foo"))
      )
    )
  )
  "_~Foo~" ?= Doc(
    Synopsis(
      Section.Raw(
        Formatter
          .Unclosed(Formatter.Italic, Formatter(Formatter.Strikeout, "Foo"))
      )
    )
  )
  "~~~Foo" ?= Doc(
    Synopsis(
      Section.Raw(
        Formatter(Formatter.Strikeout),
        Formatter.Unclosed(Formatter.Strikeout, "Foo")
      )
    )
  )
  " foo *bar* _baz *bo*_" ?= Doc(
    Synopsis(
      Section.Raw(
        1,
        "foo ",
        Formatter(Formatter.Bold, "bar"),
        " ",
        Formatter(Formatter.Italic, "baz ", Formatter(Formatter.Bold, "bo"))
      )
    )
  )
  """foo *bar
    |*""".stripMargin.replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(Section.Raw("foo ", Formatter(Formatter.Bold, "bar", Newline)))
  )

  """foo _foo
    |_foo2""".stripMargin.replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section
        .Raw("foo ", Formatter(Formatter.Italic, "foo", Newline), "foo2")
    )
  )

  """foo *foo
    |*foo2""".stripMargin.replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section
        .Raw("foo ", Formatter(Formatter.Bold, "foo", Newline), "foo2")
    )
  )

  """foo ~foo
    |~foo2""".stripMargin.replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section
        .Raw("foo ", Formatter(Formatter.Strikeout, "foo", Newline), "foo2")
    )
  )

  //////////////////////////////////////////////////////////////////////////////
  //// Segments ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  "!Important" ?= Doc(
    Synopsis(
      Section.Marked(Section.Marked.Important, Section.Header("Important"))
    )
  )
  " ! Important" ?= Doc(
    Synopsis(
      Section
        .Marked(1, 1, Section.Marked.Important, Section.Header("Important"))
    )
  )
  "   ! Important" ?= Doc(
    Synopsis(
      Section
        .Marked(3, 1, Section.Marked.Important, Section.Header("Important"))
    )
  )
  " !    Important" ?= Doc(
    Synopsis(
      Section
        .Marked(1, 4, Section.Marked.Important, Section.Header("Important"))
    )
  )
  """ ! Important
    |   This is important.""".stripMargin.replaceAll(
    System.lineSeparator(),
    "\n"
  ) ?= Doc(
    Synopsis(
      Section.Marked(
        1,
        1,
        Section.Marked.Important,
        Section.Header("Important"),
        Doc.Elem.Newline,
        "This is important."
      )
    )
  )
  """! Synopsis
    |  This _is_ important""".stripMargin.replaceAll(
    System.lineSeparator(),
    "\n"
  ) ?= Doc(
    Synopsis(
      Section.Marked(
        0,
        1,
        Section.Marked.Important,
        Section.Header("Synopsis"),
        Doc.Elem.Newline,
        "This ",
        Formatter(Formatter.Italic, "is"),
        " important"
      )
    )
  )
  """Synopsis
    |This _is1_ important""".stripMargin.replaceAll(
    System.lineSeparator(),
    "\n"
  ) ?= Doc(
    Synopsis(
      Section.Raw(
        "Synopsis",
        Doc.Elem.Newline,
        "This ",
        Formatter(Formatter.Italic, "is1"),
        " important"
      )
    )
  )
  """ Synopsis
    |
    | ! Important
    |   This is important.""".stripMargin.replaceAll(
    System.lineSeparator(),
    "\n"
  ) ?= Doc(
    Synopsis(
      Section.Raw(1, "Synopsis", Doc.Elem.Newline)
    ),
    Body(
      Section.Marked(
        1,
        1,
        Section.Marked.Important,
        Section.Header("Important"),
        Doc.Elem.Newline,
        "This is important."
      )
    )
  )
  """ Synopsis
    |
    | ! Important
    |   This is important.
    |
    |   And this""".stripMargin.replaceAll(
    System.lineSeparator(),
    "\n"
  ) ?= Doc(
    Synopsis(
      Section.Raw(1, "Synopsis", Doc.Elem.Newline)
    ),
    Body(
      Section.Marked(
        1,
        1,
        Section.Marked.Important,
        Section.Header("Important"),
        Doc.Elem.Newline,
        "This is important.",
        Doc.Elem.Newline
      ),
      Section.Raw(3, "And this")
    )
  )
  """ Synopsis
    |
    | !Important
    |    This is a code
    |
    |  And this is not""".stripMargin.replaceAll(
    System.lineSeparator(),
    "\n"
  ) ?= Doc(
    Synopsis(
      Section.Raw(1, "Synopsis", Doc.Elem.Newline)
    ),
    Body(
      Section.Marked(
        1,
        0,
        Section.Marked.Important,
        Section.Header("Important"),
        Doc.Elem.Newline,
        CodeBlock(CodeBlock.Line(4, "This is a code")),
        Doc.Elem.Newline
      ),
      Section.Raw(2, "And this is not")
    )
  )
  """Synopsis
    |
    |! Important
    |  This is important
    |
    |    And this is a code""".stripMargin.replaceAll(
    System.lineSeparator(),
    "\n"
  ) ?== Doc(
    Synopsis(
      Section.Raw("Synopsis", Doc.Elem.Newline)
    ),
    Body(
      Section.Marked(
        0,
        1,
        Section.Marked.Important,
        Section.Header("Important"),
        Doc.Elem.Newline,
        "This is important",
        Doc.Elem.Newline,
        CodeBlock(CodeBlock.Line(4, "And this is a code"))
      )
    )
  )
  "?Info" ?= Doc(
    Synopsis(Section.Marked(Section.Marked.Info, Section.Header("Info")))
  )
  ">Example" ?= Doc(
    Synopsis(
      Section.Marked(Section.Marked.Example, Section.Header("Example"))
    )
  )
  """?Info
    |
    |!Important""".stripMargin.replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Marked(Section.Marked.Info, Section.Header("Info"), Newline)
    ),
    Body(
      Section.Marked(Section.Marked.Important, Section.Header("Important"))
    )
  )
  """?Info
    |
    |!Important
    |
    |>Example""".stripMargin.replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Marked(Section.Marked.Info, Section.Header("Info"), Newline)
    ),
    Body(
      Section.Marked(
        Section.Marked.Important,
        Section.Header("Important"),
        Newline
      ),
      Section.Marked(Section.Marked.Example, Section.Header("Example"))
    )
  )
  """Synopsis
    |
    | ! Important
    |   This is important
    |     And this is a code
    |
    |> Example
    |  This is example
    |      More code""".stripMargin.replaceAll(
    System.lineSeparator(),
    "\n"
  ) ?= Doc(
    Synopsis(
      Section.Raw("Synopsis", Doc.Elem.Newline)
    ),
    Body(
      Section.Marked(
        1,
        1,
        Section.Marked.Important,
        Section.Header("Important"),
        Doc.Elem.Newline,
        "This is important",
        Doc.Elem.Newline,
        CodeBlock(CodeBlock.Line(5, "And this is a code")),
        Doc.Elem.Newline
      ),
      Section.Marked(
        0,
        1,
        Section.Marked.Example,
        Section.Header("Example"),
        Doc.Elem.Newline,
        "This is example",
        Doc.Elem.Newline,
        CodeBlock(CodeBlock.Line(6, "More code"))
      )
    )
  )
  """Foo *Foo* ~*Bar~ `foo bar baz bo`
    |
    |
    |Hello Section
    |
    |!important
    |
    |?info
    |
    |>Example""".stripMargin.replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        "Foo ",
        Formatter(Formatter.Bold, "Foo"),
        " ",
        Formatter(
          Formatter.Strikeout,
          Formatter.Unclosed(Formatter.Bold, "Bar")
        ),
        " ",
        CodeBlock.Inline("foo bar baz bo"),
        Newline
      )
    ),
    Body(
      Section.Raw(Section.Header("Hello Section"), Newline),
      Section
        .Marked(
          Section.Marked.Important,
          Section.Header("important"),
          Newline
        ),
      Section.Marked(Section.Marked.Info, Section.Header("info"), Newline),
      Section.Marked(Section.Marked.Example, Section.Header("Example"))
    )
  )

  //////////////////////////////////////////////////////////////////////////////
  //// Lists ///////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  "ul:\n  - Foo\n  - Bar" ?= Doc(
    Synopsis(
      Section.Raw(
        "ul:",
        Newline,
        List(2, List.Unordered, "Foo", "Bar")
      )
    )
  )
  "ol:\n  * Foo\n  * Bar" ?= Doc(
    Synopsis(
      Section.Raw(
        "ol:",
        Newline,
        List(2, List.Ordered, "Foo", "Bar")
      )
    )
  )
  """List
    |- First
    |- Second
    |- Third""".stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        "List",
        Newline,
        List(
          0,
          List.Unordered,
          "First",
          "Second",
          "Third"
        )
      )
    )
  )
  """List
    |  - First unordered item
    |  - Second unordered item
    |  - Third unordered item
    |""".stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        "List",
        Newline,
        List(
          2,
          List.Unordered,
          "First unordered item",
          "Second unordered item",
          "Third unordered item"
        ),
        Newline
      )
    )
  )
  """List
    |- _First_
    |- *Second*""".stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        "List",
        Newline,
        List(
          0,
          List.Unordered,
          Formatter(Formatter.Italic, "First"),
          Formatter(Formatter.Bold, "Second")
        )
      )
    )
  )
  """List
    |- _First_ list `item`
    |- *Second* list ~item""".stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        "List",
        Newline,
        List(
          0,
          List.Unordered,
          ListItem(
            Formatter(Formatter.Italic, "First"),
            " list ",
            CodeBlock.Inline("item")
          ),
          ListItem(
            Formatter(Formatter.Bold, "Second"),
            " list ",
            Formatter.Unclosed(Formatter.Strikeout, "item")
          )
        )
      )
    )
  )
  """List
    |- _First_ list `item`
    |- *Second* list ~item
    |""".stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        "List",
        Newline,
        List(
          0,
          List.Unordered,
          ListItem(
            Formatter(Formatter.Italic, "First"),
            " list ",
            CodeBlock.Inline("item")
          ),
          ListItem(
            Formatter(Formatter.Bold, "Second"),
            " list ",
            Formatter.Unclosed(Formatter.Strikeout, "item", Newline)
          )
        )
      )
    )
  )
  """ List
    |   - unclosed_formatter
    |   - second""".stripMargin.stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        1,
        "List",
        Newline,
        List(
          3,
          List.Unordered,
          ListItem(
            "unclosed",
            Formatter.Unclosed(Formatter.Italic, "formatter")
          ),
          "second"
        )
      )
    )
  )
  """ unclosed_formatter
    | - first
    | - second
    |""".stripMargin.stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        1,
        "unclosed",
        Formatter.Unclosed(Formatter.Italic, "formatter"),
        Newline,
        List(
          1,
          List.Unordered,
          "first",
          "second"
        ),
        Newline
      )
    )
  )
  """ unclosed_formatter
    | - first
    | - second_unclosed
    |   - sub""".stripMargin.stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        1,
        "unclosed",
        Formatter.Unclosed(Formatter.Italic, "formatter"),
        Newline,
        List(
          1,
          List.Unordered,
          ListItem("first"),
          ListItem("second", Formatter.Unclosed(Formatter.Italic, "unclosed")),
          List(
            3,
            List.Unordered,
            "sub"
          )
        )
      )
    )
  )
  """List
    |  - First
    |  - Second
    |    * First1
    |    * Second1
    |  - Third""".stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        "List",
        Newline,
        List(
          2,
          List.Unordered,
          "First",
          "Second",
          List(
            4,
            List.Ordered,
            "First1",
            "Second1"
          ),
          "Third"
        )
      )
    )
  )
  """List
    |  - First
    |    - First1
    |      - First2
    |        - First3""".stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        "List",
        Newline,
        List(
          2,
          List.Unordered,
          "First",
          List(
            4,
            List.Unordered,
            "First1",
            List(
              6,
              List.Unordered,
              "First2",
              List(
                8,
                List.Unordered,
                "First3"
              )
            )
          )
        )
      )
    )
  )
  """List
    |  - First
    |    - First1
    |      - First2
    |        - First3
    |""".stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        "List",
        Newline,
        List(
          2,
          List.Unordered,
          "First",
          List(
            4,
            List.Unordered,
            "First1",
            List(
              6,
              List.Unordered,
              "First2",
              List(
                8,
                List.Unordered,
                ListItem("First3", Newline)
              )
            )
          )
        )
      )
    )
  )
  """List
    |  - First
    |    - First1
    |      - First2
    |        - First3
    |  - Second""".stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        "List",
        Newline,
        List(
          2,
          List.Unordered,
          "First",
          List(
            4,
            List.Unordered,
            "First1",
            List(
              6,
              List.Unordered,
              "First2",
              List(
                8,
                List.Unordered,
                "First3"
              )
            )
          ),
          "Second"
        )
      )
    )
  )

  """List
    |  - First unordered item
    |  - Second unordered item
    |    * First ordered sub item
    |    * Second ordered sub item
    |  - Third unordered item""".stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        "List",
        Newline,
        List(
          2,
          List.Unordered,
          "First unordered item",
          "Second unordered item",
          List(
            4,
            List.Ordered,
            "First ordered sub item",
            "Second ordered sub item"
          ),
          "Third unordered item"
        )
      )
    )
  )
  """List
    |  - First unordered item
    |  - Second unordered item
    |    * First ordered sub item
    |    *    Second ordered sub item
    |  - Third unordered item""".stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        "List",
        Newline,
        List(
          2,
          List.Unordered,
          "First unordered item",
          "Second unordered item",
          List(
            4,
            List.Ordered,
            "First ordered sub item",
            "   Second ordered sub item"
          ),
          "Third unordered item"
        )
      )
    )
  )
  """List
    |  - First unordered item
    |  - Second unordered item
    |    * First ordered sub item
    |    * Second ordered sub item
    |  - Third unordered item
    |    * First ordered sub item
    |    * Second ordered sub item
    |      - First unordered sub item
    |      - Second unordered sub item
    |    * Third ordered sub item
    |  - Fourth unordered item""".stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        "List",
        Newline,
        List(
          2,
          List.Unordered,
          "First unordered item",
          "Second unordered item",
          List(
            4,
            List.Ordered,
            "First ordered sub item",
            "Second ordered sub item"
          ),
          "Third unordered item",
          List(
            4,
            List.Ordered,
            "First ordered sub item",
            "Second ordered sub item",
            List(
              6,
              List.Unordered,
              "First unordered sub item",
              "Second unordered sub item"
            ),
            "Third ordered sub item"
          ),
          "Fourth unordered item"
        )
      )
    )
  )

  //////////////////////////////////////////////////////////////////////////////
  //// Wrong indent ////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  """List
    |  - First
    |     * Aligned
    |    * Misaligned
    |   * Misaligned _styled_
    |     * Correct
    |  - Second""".stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        "List",
        Newline,
        List(
          2,
          List.Unordered,
          "First",
          List(
            5,
            List.Ordered,
            "Aligned",
            Elem.MisalignedItem(4, List.Ordered, "Misaligned"),
            Elem.MisalignedItem(
              3,
              List.Ordered,
              "Misaligned ",
              Formatter(Formatter.Italic, "styled")
            ),
            "Correct"
          ),
          "Second"
        )
      )
    )
  )

  """List
    |  - First
    |    - First1
    |   - Second1
    |      - First2
    |  - Second""".stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        "List",
        Newline,
        List(
          2,
          List.Unordered,
          "First",
          List(
            4,
            List.Unordered,
            "First1",
            Elem.MisalignedItem(3, List.Unordered, "Second1"),
            List(
              6,
              List.Unordered,
              "First2"
            )
          ),
          "Second"
        )
      )
    )
  )

  //////////////////////////////////////////////////////////////////////////////
  //// Links ///////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  "[Hello](Http://Google.com)" ?= Doc(
    Synopsis(
      Section.Raw(
        Link.URL(
          "Hello",
          "Http://Google.com"
        )
      )
    )
  )
  "![Media](http://foo.com)" ?= Doc(
    Synopsis(
      Section.Raw(
        Link.Image(
          "Media",
          "http://foo.com"
        )
      )
    )
  )
  "![foo)" ?= Doc(
    Synopsis(
      Section.Raw(
        Link.Invalid(
          "![foo)"
        )
      )
    )
  )
  "[foo)" ?= Doc(
    Synopsis(
      Section.Raw(
        Link.Invalid(
          "[foo)"
        )
      )
    )
  )
  "[foo]bo)" ?= Doc(
    Synopsis(
      Section.Raw(
        Link.Invalid(
          "[foo]bo)"
        )
      )
    )
  )
  "![foo]google" ?= Doc(
    Synopsis(
      Section.Raw(
        Link.Invalid(
          "![foo]google"
        )
      )
    )
  )

  "[foo]google" ?= Doc(
    Synopsis(
      Section.Raw(
        Link.Invalid(
          "[foo]google"
        )
      )
    )
  )

  """[foo]bo)
    |basdbasd""".stripMargin.replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        Link.Invalid(
          "[foo]bo)"
        ),
        Newline,
        "basdbasd"
      )
    )
  )

  //////////////////////////////////////////////////////////////////////////////
  //// Tags ////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  val allPossibleTags: Set[Tag.Type] =
    Tags.Tag.Type.codes.-(Tags.Tag.Unrecognized)

  allPossibleTags.foreach(t =>
    s"${t.toString.toUpperCase()}\nFoo" ?= Doc(
      Tags(Tags.Tag(t)),
      Synopsis(Section.Raw("Foo"))
    )
  )
  "DEPRECATED in 1.0\nFoo" ?= Doc(
    Tags(Tags.Tag(Tags.Tag.Type.Deprecated, " in 1.0")),
    Synopsis(Section.Raw("Foo"))
  )
  "DEPRECATED in 1.0\nMODIFIED\nFoo" ?= Doc(
    Tags(
      Tags.Tag(Tags.Tag.Type.Deprecated, " in 1.0"),
      Tags.Tag(Tags.Tag.Type.Modified)
    ),
    Synopsis(Section.Raw("Foo"))
  )
  """   ALAMAKOTA
    | DEPRECATED
    | PRIVATE
    | foo bar""".stripMargin.replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Tags(
      Tags.Tag(3, Tags.Tag.Unrecognized, "ALAMAKOTA"),
      Tags.Tag(1, Tags.Tag.Type.Deprecated),
      Tags.Tag(1, Tags.Tag.Type.Private)
    ),
    Synopsis(Section.Raw(1, "foo bar"))
  )

  //////////////////////////////////////////////////////////////////////////////
  //// Multiline code //////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  """afsfasfsfjanfjanfa
    |jfnajnfjadnbfjabnf
    |   siafjaifhjiasjf
    |   fasfknfanfijnf""".stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        "afsfasfsfjanfjanfa",
        Newline,
        "jfnajnfjadnbfjabnf",
        Newline,
        CodeBlock(
          CodeBlock.Line(3, "siafjaifhjiasjf"),
          CodeBlock.Line(3, "fasfknfanfijnf")
        )
      )
    )
  )
  """afsfasfsfjanfjanfa
    |jfnajnfjadnbfjabnf
    |   siafjaifhjiasjf
    |     fasfknfanfijnf
    |   fasfknfanfijnf""".stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        "afsfasfsfjanfjanfa",
        Newline,
        "jfnajnfjadnbfjabnf",
        Newline,
        CodeBlock(
          CodeBlock.Line(3, "siafjaifhjiasjf"),
          CodeBlock.Line(5, "fasfknfanfijnf"),
          CodeBlock.Line(3, "fasfknfanfijnf")
        )
      )
    )
  )
  """afsfasfsfjanfjanfa
    |jfnajnfjadnbfjabnf
    |   fasfknfanfijnf
    |     fasfknfanfijnf
    |          fasfknfanfijnf
    |     fasfknfanfijnf
    |   fasfknfanfijnf""".stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        "afsfasfsfjanfjanfa",
        Newline,
        "jfnajnfjadnbfjabnf",
        Newline,
        CodeBlock(
          CodeBlock.Line(3, "fasfknfanfijnf"),
          CodeBlock.Line(5, "fasfknfanfijnf"),
          CodeBlock.Line(10, "fasfknfanfijnf"),
          CodeBlock.Line(5, "fasfknfanfijnf"),
          CodeBlock.Line(3, "fasfknfanfijnf")
        )
      )
    )
  )
  """afsfasfsfjanfjanfa
    |jfnajnfjadnbfjabnf
    |   fasfknfanfijnf
    |     fasfknfanfijnf
    |  fasfknfanfijnf
    |     fasfknfanfijnf
    |   fasfknfanfijnf""".stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        "afsfasfsfjanfjanfa",
        Newline,
        "jfnajnfjadnbfjabnf",
        Newline,
        CodeBlock(
          CodeBlock.Line(3, "fasfknfanfijnf"),
          CodeBlock.Line(5, "fasfknfanfijnf"),
          CodeBlock.Line(2, "fasfknfanfijnf"),
          CodeBlock.Line(5, "fasfknfanfijnf"),
          CodeBlock.Line(3, "fasfknfanfijnf")
        )
      )
    )
  )

  //////////////////////////////////////////////////////////////////////////////
  //// Unclassified tests //////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  """
    | - bar
    |   baz
    |""".stripMargin.replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        Newline,
        List(1, List.Unordered, ListItem("bar", Newline, "   ", "baz")),
        Newline
      )
    )
  )
  """
    | - bar
    |     baz
    |""".stripMargin.replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        Newline,
        List(
          1,
          List.Unordered,
          ListItem("bar", Newline, CodeBlock(CodeBlock.Line(5, "baz")))
        ),
        Newline
      )
    )
  )
  """
    | - bar
    |   baz
    | - bar
    |   baz
    |""".stripMargin.replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        Newline,
        List(
          1,
          List.Unordered,
          ListItem("bar", Newline, "   ", "baz"),
          ListItem("bar", Newline, "   ", "baz")
        ),
        Newline
      )
    )
  )
  """
    | - bar
    |     baz
    | - bar
    |   baz
    |""".stripMargin.replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        Newline,
        List(
          1,
          List.Unordered,
          ListItem("bar", Newline, CodeBlock(CodeBlock.Line(5, "baz"))),
          ListItem("bar", Newline, "   ", "baz")
        ),
        Newline
      )
    )
  )

  """ This does foo:
    | - bar
    |   baz
    | Another raw text.
    |""".stripMargin.replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        1,
        "This does foo:",
        Newline,
        List(
          1,
          List.Unordered,
          ListItem(
            "bar",
            Newline,
            "   ",
            "baz",
            Newline,
            " ",
            "Another raw text."
          )
        ),
        Newline
      )
    )
  )
  """ This does foo:
    | - bar
    |   baz
    |
    | Another raw text.
    |""".stripMargin.replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Raw(
        1,
        "This does foo:",
        Newline,
        List(1, List.Unordered, ListItem("bar", Newline, "   ", "baz")),
        Newline
      )
    ),
    Body(
      Section.Raw(
        1,
        "Another raw text.",
        Newline
      )
    )
  )

  """ > Example
    |   This does foo:
    |    - bar
    |      baz
    |""".stripMargin.replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Synopsis(
      Section.Marked(
        1,
        1,
        Section.Marked.Example,
        Section.Header("Example"),
        Newline,
        "This does foo:",
        Newline,
        List(4, List.Unordered, ListItem("bar", Newline, "      ", "baz")),
        Newline
      )
    )
  )

  """   DEPRECATED das sfa asf
    |REMOVED fdsdf
    |Construct and manage a graphical, event-driven user interface for your iOS or
    |tvOS app.
    |
    | foo *foo*""".stripMargin.replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Tags(
      Tags.Tag(3, Tags.Tag.Type.Deprecated, " das sfa asf"),
      Tags.Tag(0, Tags.Tag.Type.Removed, " fdsdf")
    ),
    Synopsis(
      Section.Raw(
        "Construct and manage a graphical, event-driven user interface for your iOS or",
        Newline,
        "tvOS app.",
        Newline
      )
    ),
    Body(Section.Raw(1, "foo ", Formatter(Formatter.Bold, "foo")))
  )

  """   DEPRECATED das sfa asf
    |REMOVED fdsdf
    |Construct and manage a graphical, event-driven user interface for your iOS or
    |tvOS app.
    |
    | foo *foo""".stripMargin.replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Tags(
      Tags.Tag(3, Tags.Tag.Type.Deprecated, " das sfa asf"),
      Tags.Tag(0, Tags.Tag.Type.Removed, " fdsdf")
    ),
    Synopsis(
      Section.Raw(
        "Construct and manage a graphical, event-driven user interface for your iOS or",
        Newline,
        "tvOS app.",
        Newline
      )
    ),
    Body(Section.Raw(1, "foo ", Formatter.Unclosed(Formatter.Bold, "foo")))
  )

  """    DEPRECATED das sfa asf
    |  REMOVED
    | Foo""".stripMargin.replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Tags(
      Tags.Tag(4, Tags.Tag.Type.Deprecated, " das sfa asf"),
      Tags.Tag(2, Tags.Tag.Type.Removed)
    ),
    Synopsis(Section.Raw(1, "Foo"))
  )

  """   DEPRECATED das sfa asf
    |REMOVED fdsdf
    |ALIAS Convert From
    |Construct and manage a graphical user interface for your iOS or
    |tvOS app.
    |
    |   fooo bar baz
    |   dsadasfsaf asfasfas
    |   asfasfa sf
    |   asfas fasf """.stripMargin
    .replaceAll(System.lineSeparator(), "\n") ?= Doc(
    Tags(
      Tags.Tag(3, Tags.Tag.Type.Deprecated, " das sfa asf"),
      Tags.Tag(0, Tags.Tag.Type.Removed, " fdsdf"),
      Tags.Tag(0, Tags.Tag.Type.Alias, " Convert From")
    ),
    Synopsis(
      Section.Raw(
        "Construct and manage a graphical user interface for your iOS or",
        Newline,
        "tvOS app.",
        Newline
      )
    ),
    Body(
      Section.Raw(
        3,
        "fooo bar baz",
        Newline,
        "dsadasfsaf asfasfas",
        Newline,
        "asfasfa sf",
        Newline,
        "asfas fasf "
      )
    )
  )

  """ALIAS New File
    |
    |Creates a new file object, pointing to the given path.
    |
    |> Example
    |  Create a new file pointing to the `data.csv` file in the project directory.
    |
    |      import Standard.Base.System.File
    |      import Standard.Examples
    |
    |      example_new =
    |          path =
    |              Examples.csv_path
    |          File.new path
    |""".stripMargin.replaceAll(System.lineSeparator(), "\n") ?== Doc(
    Tags(Tags.Tag(0, Tags.Tag.Type.Alias, " New File")),
    Synopsis(Section.Raw(0, Newline)),
    Body(
      Section.Raw(
        0,
        "Creates a new file object, pointing to the given path.",
        Newline
      ),
      Section.Marked(
        0,
        1,
        Section.Marked.Example,
        Section.Header("Example"),
        Newline,
        "Create a new file pointing to the ",
        CodeBlock.Inline("data.csv"),
        " file in the project directory.",
        Newline,
        CodeBlock(
          CodeBlock.Line(6, "import Standard.Base.System.File"),
          CodeBlock.Line(6, "import Standard.Examples"),
          CodeBlock.Line(6, "example_new ="),
          CodeBlock.Line(10, "path ="),
          CodeBlock.Line(14, "Examples.csv_path"),
          CodeBlock.Line(10, "File.new path")
        )
      )
    )
  )

}
