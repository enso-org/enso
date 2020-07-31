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
    * Prints out the given question and asks the user to confirm the action by
    * typing 'y' or 'n'.
    *
    * Also handles uppercase variants. An empty line defaults to the default
    * option specified by `yesDefault`. Other inputs result in the question
    * being asked again.
    *
    * @param question The question to print.
    * @param yesDefault Specifies if an empty line defaults to true or false.
    */
  @scala.annotation.tailrec
  def askConfirmation(
    question: String,
    yesDefault: Boolean = false
  ): Boolean = {
    val prompt = if (yesDefault) "[Y/n]" else "[y/N]"
    val text   = alignAndWrap(question + " " + prompt + " ")
    Predef.print(text)
    val line = Console.in.readLine().strip().toLowerCase
    if (line.isEmpty) yesDefault
    else if (line == "y") true
    else if (line == "n") false
    else {
      println(s"Unexpected answer `$line`.")
      askConfirmation(question, yesDefault)
    }
  }

  /**
    * A type for [[askQuestion]] which specifies how to display the possible
    * answers.
    */
  trait Answer {

    /**
      * The key that is associated with this answer. Should be a single
      * character.
      */
    def key: String

    /**
      * A short description of this answer.
      */
    def description: String
  }

  /**
    * Asks the user to choose one of the possible answers.
    *
    * The first answer in the provided sequence is treated as the default and it
    * is returned if the user inputs an empty line. If the user inputs a
    * non-empty value that does not fit any answer, the question is asked again.
    *
    * @param question The question to print.
    * @param answers A sequence of possible answers. No two answers should have
    *                the same key.
    * @tparam A The type of possible answers. Must be a sub-type of [[Answer]].
    * @return The answer chosen by the user.
    */
  def askQuestion[A <: Answer](question: String, answers: Seq[A]): A = {
    val explanations =
      "(" +
      answers
        .map(a => a.key.toLowerCase + " - " + a.description)
        .mkString(", ") +
      ")"
    val shortcuts = "[" + answers.map(_.key.toLowerCase).mkString("/") + "]"
    val prompt =
      CLIOutput.alignAndWrap(
        question + " " + explanations + " " + shortcuts + " "
      )
    Predef.print(prompt)
    val line = Console.in.readLine().strip().toLowerCase
    if (line.isEmpty)
      answers.head
    else
      answers.find(_.key.toLowerCase == line).getOrElse {
        CLIOutput.println(s"`$line` is not a valid option.")
        askQuestion(question, answers)
      }
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
