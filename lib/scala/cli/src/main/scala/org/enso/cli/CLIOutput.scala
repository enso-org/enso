package org.enso.cli

import org.enso.cli.internal.CLIOutputInternal

object CLIOutput {
  val terminalWidth: Int          = 80
  val minimumColumnWrapWidth: Int = 50

  /**
    * Aligns tables and wraps all text to [[terminalWidth]] (with some
    * exceptions).
    *
    * A table is a continuous sequence of lines containing the `\t` character.
    * Such continuous sequences are grouped into tables and each table is
    * aligned, so that the second column of each row starts at the same
    * indentation level (determined by the longest cell in the first column).
    * Only two-column tables are supported (more columns can be included, but
    * they will not be aligned). If the first column is so wide, that the second
    * column would be thinner than [[minimumColumnWrapWidth]], the second column
    * is wrapped to [[minimumColumnWrapWidth]] exceeding the  [[terminalWidth]]
    * limit.
    *
    * If a word without spaces is longer than [[terminalWidth]] it is also not
    * wrapped. This is done to avoid breaking long URLs when wrapping text.
    *
    * @param text the text to align and wrap
    * @param minTableWidth if provided, the first column in each table is
    *                      padded to have at least this width
    * @return the `text` with added whitespace
    */
  def alignAndWrap(text: String, minTableWidth: Option[Int] = None): String = {
    val lines    = splitLinesPreservingTrailing(text)
    val entities = CLIOutputInternal.groupTables(lines)
    val wrapped: Seq[String] = entities flatMap {
        case CLIOutputInternal.TextLine(line) =>
          CLIOutputInternal.wrapLine(line, terminalWidth).toList
        case CLIOutputInternal.TextTable(rows) =>
          CLIOutputInternal.alignAndWrapTable(
            rows,
            terminalWidth,
            minimumColumnWrapWidth,
            minimumTableWidth = minTableWidth.getOrElse(2)
          )
      }
    joinLines(wrapped)
  }

  /**
    * Prints the provided text to stdout, first aligning and wrapping it as
    * described in [[alignAndWrap]].
    */
  def println(text: String): Unit = {
    Predef.println(alignAndWrap(text))
  }

  /**
    * Default indentation used for printing lists.
    */
  val indent: String = "    "

  /**
    * Splits the text into lines, preserving trailing newlines by adding empty
    * lines.
    */
  private def splitLinesPreservingTrailing(string: String): Seq[String] = {
    val lines     = (string + "|").split("\\R")
    val lastIndex = lines.length - 1
    lines(lastIndex) = lines(lastIndex).stripSuffix("|")
    lines.toIndexedSeq
  }

  private def joinLines(lines: Seq[String]): String =
    lines.mkString(System.lineSeparator())
}
