package org.enso.launcher.cli

import org.enso.launcher.cli.internal.CLIOutputInternal

object CLIOutput {
  val terminalWidth: Int          = 80
  val minimumColumnWrapWidth: Int = 50

  /**
    * TODO [RW] explain wrapping logic
    * @param text
    * @return
    */
  def alignAndWrap(text: String, minTableWidth: Option[Int] = None): String = {
    val entities = CLIOutputInternal.groupTables(text.split('\n'))
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
    wrapped.mkString("\n")
  }

  def println(text: String): Unit = {
    Predef.println(alignAndWrap(text))
  }
}
