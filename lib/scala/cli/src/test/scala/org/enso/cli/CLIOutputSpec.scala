package org.enso.cli

import org.enso.cli.internal.CLIOutputInternal
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CLIOutputSpec extends AnyWordSpec with Matchers {
  "wrapLine" should {
    "wrap at the wrapPoint without breaking words" in {
      val wrapAt = 10
      val text10 = "a a a a aa"
      CLIOutputInternal.wrapLine(text10, wrapAt).toList shouldEqual List(text10)

      val textExact = "a a a a aa a"
      CLIOutputInternal.wrapLine(textExact, wrapAt).toList shouldEqual
      List("a a a a aa", "a")

      val textTogether = "a a a a aaa"
      CLIOutputInternal.wrapLine(textTogether, wrapAt).toList shouldEqual
      List("a a a a", "aaa")
    }

    "not wrap text without whitespace" in {
      val denseText = "*" * 20
      CLIOutputInternal.wrapLine(denseText, 10).head shouldEqual denseText
    }
  }

  "groupTables" should {
    "group tables but keep lines separate " +
    "and preserve ordering and empty lines" in {
      val lines = Seq(
        "first line",
        "",
        "",
        "Table:",
        "A\tX",
        "B\tY",
        "C\tZ",
        "",
        "last line"
      )

      CLIOutputInternal.groupTables(lines) should be(
        Seq(
          CLIOutputInternal.TextLine("first line"),
          CLIOutputInternal.TextLine(""),
          CLIOutputInternal.TextLine(""),
          CLIOutputInternal.TextLine("Table:"),
          CLIOutputInternal.TextTable(
            Seq(
              ("A", "X"),
              ("B", "Y"),
              ("C", "Z")
            )
          ),
          CLIOutputInternal.TextLine(""),
          CLIOutputInternal.TextLine("last line")
        )
      )
    }
  }

  "alignAndWrapTable" should {
    "align the table and wrap the content" in {
      val wrapped = CLIOutputInternal.alignAndWrapTable(
        Seq(
          ("aa", "a b c d e f"),
          ("b", "a")
        ),
        10,
        5,
        2
      )

      wrapped.mkString(System.lineSeparator()) shouldEqual
      """aa a b c d
        |   e f
        |b  a""".stripMargin
    }

    "keep the minimum wrap width" in {
      val wrapped = CLIOutputInternal.alignAndWrapTable(
        Seq(
          ("abcdef", "a b c d e f"),
          ("abcde", "f"),
          ("b", "a")
        ),
        10,
        5,
        2
      )

      wrapped.mkString(System.lineSeparator()) shouldEqual
      """abcdef
        |     a b c
        |     d e f
        |abcde
        |     f
        |b    a""".stripMargin
    }
  }

  "CLIOutput" should {
    val tabulation = "\t"
    "align tables" in {
      val unaligned =
        s"""short${tabulation}row
           |very long${tabulation}row
           |""".stripMargin
      val aligned =
        s"""short     row
           |very long row
           |""".stripMargin
      CLIOutput.alignAndWrap(unaligned) shouldEqual aligned
    }

    "align single row tables too" in {
      val unaligned = s"a${tabulation}b"
      val aligned   = "a b"
      CLIOutput.alignAndWrap(unaligned) shouldEqual aligned
    }

    "align tables independently" in {
      val unaligned =
        s"""Table One
           |short${tabulation}row
           |very long${tabulation}row
           |
           |Table Two
           |a${tabulation}b
           |c${tabulation}d
           |""".stripMargin
      val aligned =
        s"""Table One
           |short     row
           |very long row
           |
           |Table Two
           |a b
           |c d
           |""".stripMargin
      CLIOutput.alignAndWrap(unaligned) shouldEqual aligned
    }

    "wrap text to 80 characters" in {
      val text10 = "TEN LETTER"
      val text80 = text10 * 8
      CLIOutput.alignAndWrap(text80) shouldEqual text80

      val longer = s"""$text80 MORE"""
      val longerWrapped =
        s"""$text80
           |MORE""".stripMargin
      CLIOutput.alignAndWrap(longer) shouldEqual longerWrapped
    }
  }
}
