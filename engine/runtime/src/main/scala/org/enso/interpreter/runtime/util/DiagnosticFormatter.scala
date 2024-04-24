package org.enso.interpreter.runtime.util

import com.oracle.truffle.api.source.{Source, SourceSection}
import org.enso.compiler.core.ir.expression.Error
import org.enso.compiler.core.ir.{Diagnostic, IdentifiedLocation, Warning}
import org.enso.interpreter.runtime.EnsoContext

/** Formatter of IR diagnostics. Heavily inspired by GCC. Can format one-line as well as multiline
  * diagnostics. The output is colorized if the output stream supports ANSI colors.
  * Also prints the offending lines from the source along with line number - the same way as
  * GCC does.
  *
  * @param diagnostic the diagnostic to pretty print
  * @param source     the original source code
  */
class DiagnosticFormatter(
  private val diagnostic: Diagnostic,
  private val source: Source,
  private val isOutputRedirected: Boolean
) {
  private val maxLineNum                     = 99999
  private val blankLinePrefix                = "      | "
  private val maxSourceLinesToPrint          = 3
  private val linePrefixSize                 = blankLinePrefix.length
  private val outSupportsAnsiColors: Boolean = outSupportsColors
  private val (textAttrs: fansi.Attrs, subject: String) = diagnostic match {
    case _: Error   => (fansi.Color.Red ++ fansi.Bold.On, "error: ")
    case _: Warning => (fansi.Color.Yellow ++ fansi.Bold.On, "warning: ")
    case _          => throw new IllegalStateException("Unexpected diagnostic type")
  }

  def fileLocationFromSection(loc: IdentifiedLocation) = {
    val section =
      source.createSection(loc.location().start(), loc.location().length());
    val locStr = "" + section.getStartLine() + ":" + section
      .getStartColumn() + "-" + section.getEndLine() + ":" + section
      .getEndColumn()
    source.getName() + "[" + locStr + "]";
  }

  private val sourceSection: Option[SourceSection] =
    diagnostic.location match {
      case Some(location) =>
        if (location.length > source.getLength) {
          None
        } else {
          Some(source.createSection(location.start, location.length))
        }
      case None => None
    }
  private val shouldPrintLineNumber = sourceSection match {
    case Some(section) =>
      section.getStartLine <= maxLineNum && section.getEndLine <= maxLineNum
    case None => false
  }

  def format(): String = {
    sourceSection match {
      case Some(section) =>
        val isOneLine = section.getStartLine == section.getEndLine
        val srcPath: String =
          if (source.getPath == null && source.getName == null) {
            "<Unknown source>"
          } else if (source.getPath != null) {
            source.getPath
          } else {
            source.getName
          }
        var str = fansi.Str()
        if (isOneLine) {
          val lineNumber      = section.getStartLine
          val startColumn     = section.getStartColumn
          val endColumn       = section.getEndColumn
          val isLocationEmpty = startColumn == endColumn
          str ++= fansi
            .Str(srcPath + ":" + lineNumber + ":" + startColumn + ": ")
            .overlay(fansi.Bold.On)
          str ++= fansi.Str(subject).overlay(textAttrs)
          str ++= diagnostic.formattedMessage(fileLocationFromSection)
          if (!isLocationEmpty) {
            str ++= "\n"
            str ++= oneLineFromSourceColored(lineNumber, startColumn, endColumn)
            str ++= "\n"
            str ++= underline(startColumn, endColumn)
          }
        } else {
          str ++= fansi
            .Str(
              srcPath + ":[" + section.getStartLine + ":" + section.getStartColumn + "-" + section.getEndLine + ":" + section.getEndColumn + "]: "
            )
            .overlay(fansi.Bold.On)
          str ++= fansi.Str(subject).overlay(textAttrs)
          str ++= diagnostic.formattedMessage(fileLocationFromSection)
          str ++= "\n"
          val printAllSourceLines =
            section.getEndLine - section.getStartLine <= maxSourceLinesToPrint
          val endLine =
            if (printAllSourceLines) section.getEndLine
            else section.getStartLine + maxSourceLinesToPrint
          for (lineNum <- section.getStartLine to endLine) {
            str ++= oneLineFromSource(lineNum)
            str ++= "\n"
          }
          if (!printAllSourceLines) {
            val restLineCount =
              section.getEndLine - section.getStartLine - maxSourceLinesToPrint
            str ++= blankLinePrefix + "... and " + restLineCount + " more lines ..."
            str ++= "\n"
          }
        }
        if (outSupportsAnsiColors) {
          str.render.stripLineEnd
        } else {
          str.plainText.stripLineEnd
        }
      case None =>
        // There is no source section associated with the diagnostics
        var str = fansi.Str()
        val fileLocation = diagnostic.location match {
          case Some(_) =>
            fileLocationFromSectionOption(diagnostic.location, source)
          case None =>
            Option(source.getPath).getOrElse("<Unknown source>")
        }

        str ++= fansi
          .Str(fileLocation)
          .overlay(fansi.Bold.On)
        str ++= ": "
        str ++= fansi.Str(subject).overlay(textAttrs)
        str ++= diagnostic.formattedMessage(fileLocationFromSection)
        if (outSupportsAnsiColors) {
          str.render.stripLineEnd
        } else {
          str.plainText.stripLineEnd
        }
    }
  }

  /** @see https://github.com/termstandard/colors/
    * @see https://no-color.org/
    * @return
    */
  private def outSupportsColors: Boolean = {
    if (System.console() == null) {
      // Non-interactive output is always without color support
      return false
    }
    if (isOutputRedirected) {
      return false
    }
    return EnsoContext.get(null).isColorTerminalOutput;
  }

  private def oneLineFromSource(lineNum: Int): String = {
    val line = source.createSection(lineNum).getCharacters.toString
    linePrefix(lineNum) + line
  }

  private def oneLineFromSourceColored(
    lineNum: Int,
    startCol: Int,
    endCol: Int
  ): String = {
    val line = source.createSection(lineNum).getCharacters.toString
    linePrefix(lineNum) + fansi
      .Str(line)
      .overlay(textAttrs, startCol - 1, endCol)
  }

  private def linePrefix(lineNum: Int): String = {
    if (shouldPrintLineNumber) {
      val pipeSymbol = " | "
      val prefixWhitespaces =
        linePrefixSize - lineNum.toString.length - pipeSymbol.length
      " " * prefixWhitespaces + lineNum + pipeSymbol
    } else {
      blankLinePrefix
    }
  }

  private def underline(startColumn: Int, endColumn: Int): String = {
    val sectionLen = endColumn - startColumn
    blankLinePrefix +
    " " * (startColumn - 1) +
    fansi.Str("^" + ("~" * sectionLen)).overlay(textAttrs)
  }

  private def fileLocationFromSectionOption(
    loc: Option[IdentifiedLocation],
    source: Source
  ): String = {
    val srcLocation = loc match {
      case Some(identifiedLoc)
          if isLocationInSourceBounds(identifiedLoc, source) =>
        val section =
          source.createSection(identifiedLoc.start, identifiedLoc.length)
        val locStr =
          "" + section.getStartLine + ":" +
          section.getStartColumn + "-" +
          section.getEndLine + ":" +
          section.getEndColumn
        "[" + locStr + "]"
      case _ => ""
    }

    source.getPath + ":" + srcLocation
  }

  private def isLocationInSourceBounds(
    loc: IdentifiedLocation,
    source: Source
  ): Boolean = {
    loc.end() <= source.getLength
  }
}
