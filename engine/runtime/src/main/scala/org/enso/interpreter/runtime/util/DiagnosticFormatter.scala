package org.enso.interpreter.runtime.util

import com.oracle.truffle.api.source.{Source, SourceSection}
import fansi.Str
import org.enso.compiler.core.ir.expression.Error
import org.enso.compiler.core.ir.{Diagnostic, IdentifiedLocation, Warning}

/** Formatter of IR diagnostics. Heavily inspired by GCC. Can format one-line as well as multiline
  * diagnostics. The output is colorized if the output stream supports ANSI colors.
  * Also prints the offending lines from the source along with line number - the same way as
  * GCC does.
  *
  * @param diagnostic the diagnostic to pretty print
  * @param source     the original source code
  * @param isOutputRedirected whether the stdout is redirected from the console.
  * @param isColorTerminalOutput whether the output is a color terminal.
  */
class DiagnosticFormatter(
  private val diagnostic: Diagnostic,
  private val source: Source,
  private val isOutputRedirected: Boolean,
  private val isColorTerminalOutput: Boolean
) {
  private val maxLineNum                     = 99999
  private val blankLinePrefix                = "      | "
  private val maxSourceLinesToPrint          = 3
  private val linePrefixSize                 = blankLinePrefix.length
  private val outSupportsAnsiColors: Boolean = outSupportsColors

  sealed protected trait DiagnosticKind

  protected object DiagnosticKind {
    case object Error   extends DiagnosticKind
    case object Warning extends DiagnosticKind
  }

  protected val diagnosticKind: DiagnosticKind = diagnostic match {
    case _: Error   => DiagnosticKind.Error
    case _: Warning => DiagnosticKind.Warning
    case _          => throw new IllegalStateException("Unexpected diagnostic type")
  }

  private val textAttrs: fansi.Attrs = diagnosticKind match {
    case DiagnosticKind.Error   => fansi.Color.Red ++ fansi.Bold.On
    case DiagnosticKind.Warning => fansi.Color.Yellow ++ fansi.Bold.On
  }

  private val subject: String = diagnosticKind match {
    case DiagnosticKind.Error   => "error: "
    case DiagnosticKind.Warning => "warning: "
  }

  protected lazy val location: Location = computeLocation()

  def format(): String = {
    val str = location.format()
    if (outSupportsAnsiColors) {
      str.render.stripLineEnd
    } else {
      str.plainText.stripLineEnd
    }
  }

  final def where(): SourceSection = location.sourceSection

  final protected def fileLocationFromSection(
    loc: IdentifiedLocation
  ): String = {
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

  sealed protected trait Location {
    def sourceSection: SourceSection
    def format():      fansi.Str
  }

  protected object Location {
    sealed trait FileLocation
    case class SourcePath(path: String) extends FileLocation {
      override def toString: String = path
    }
    case class SourceName(name: String) extends FileLocation {
      override def toString: String = name
    }
    case object Unknown extends FileLocation {
      override def toString: String = "<Unknown source>"
    }
  }

  protected case class SingleLineSection(
    sourceSection: SourceSection,
    fileLocation: Location.FileLocation,
    lineNumber: Int,
    startColumn: Int,
    endColumn: Int
  ) extends Location {
    override def format(): fansi.Str = {
      var str = fansi.Str()
      str ++= fansi
        .Str(
          fileLocation.toString + ":" + lineNumber + ":" + startColumn + ": "
        )
        .overlay(fansi.Bold.On)
      str ++= fansi.Str(subject).overlay(textAttrs)
      str ++= diagnostic.formattedMessage(fileLocationFromSection)
      val isLocationEmpty = startColumn == endColumn
      if (!isLocationEmpty) {
        str ++= "\n"
        str ++= oneLineFromSourceColored(lineNumber, startColumn, endColumn)
        str ++= "\n"
        str ++= underline(startColumn, endColumn)
      }
      str
    }
  }

  protected case class MultiLineSection(
    sourceSection: SourceSection,
    fileLocation: Location.FileLocation,
    startLine: Int,
    endLine: Int,
    startColumn: Int,
    endColumn: Int
  ) extends Location {
    override def format(): Str = {
      var str = fansi.Str()
      str ++= fansi
        .Str(
          fileLocation.toString + ":[" + startLine + ":" + startColumn + "-" + endLine + ":" + endColumn + "]: "
        )
        .overlay(fansi.Bold.On)
      str ++= fansi.Str(subject).overlay(textAttrs)
      str ++= diagnostic.formattedMessage(fileLocationFromSection)
      str ++= "\n"
      val printAllSourceLines =
        endLine - startLine <= maxSourceLinesToPrint
      val printEndLine =
        if (printAllSourceLines) endLine
        else startLine + maxSourceLinesToPrint
      for (lineNum <- startLine to printEndLine) {
        str ++= oneLineFromSource(lineNum)
        str ++= "\n"
      }
      if (!printAllSourceLines) {
        val restLineCount =
          endLine - startLine - maxSourceLinesToPrint
        str ++= blankLinePrefix + "... and " + restLineCount + " more lines ..."
        str ++= "\n"
      }
      str
    }
  }

  protected case class UnknownSection(
    fileLocation: Location.FileLocation
  ) extends Location {
    override def sourceSection: SourceSection = null

    override def format(): Str = {
      var str = fansi.Str()
      str ++= fansi
        .Str(fileLocation.toString)
        .overlay(fansi.Bold.On)
      str ++= ": "
      str ++= fansi.Str(subject).overlay(textAttrs)
      str ++= diagnostic.formattedMessage(fileLocationFromSection)
      str
    }
  }

  private def computeLocation(): Location = {
    val fileLocation: Location.FileLocation =
      if (source.getPath == null && source.getName == null) {
        Location.Unknown
      } else if (source.getPath != null) {
        Location.SourcePath(source.getPath)
      } else {
        Location.SourceName(source.getName)
      }
    sourceSection match {
      case Some(section) =>
        val isOneLine   = section.getStartLine == section.getEndLine
        val startColumn = section.getStartColumn
        val endColumn   = section.getEndColumn
        if (isOneLine) {
          val lineNumber = section.getStartLine
          SingleLineSection(
            section,
            fileLocation,
            lineNumber,
            startColumn,
            endColumn
          )
        } else {
          val startLine = section.getStartLine
          val endLine   = section.getEndLine
          MultiLineSection(
            section,
            fileLocation,
            startLine,
            endLine,
            startColumn,
            endColumn
          )
        }
      // There is no source section associated with the diagnostics
      case None => UnknownSection(fileLocation)
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
    return isColorTerminalOutput
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
    val suffix =
      try {
        fansi
          .Str(line)
          .overlay(textAttrs, startCol - 1, endCol)
      } catch {
        case _: IllegalArgumentException => line
      }
    linePrefix(lineNum) + suffix
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

}

object DiagnosticFormatter {
  def make(
    diagnostic: Diagnostic,
    source: Source,
    isOutputRedirected: Boolean,
    isColorTerminalOutput: Boolean
  ): DiagnosticFormatter =
    if (GitHubDiagnosticFormatter.shouldIncludeGithubAnnotations)
      new GitHubDiagnosticFormatter(
        diagnostic,
        source,
        isOutputRedirected,
        isColorTerminalOutput
      )
    else
      new DiagnosticFormatter(
        diagnostic,
        source,
        isOutputRedirected,
        isColorTerminalOutput
      )
}
