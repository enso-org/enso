package org.enso.interpreter.runtime.util

import com.oracle.truffle.api.source.{Source, SourceSection}
import fansi.Str
import org.enso.compiler.core.ir.expression.Error
import org.enso.compiler.core.ir.{Diagnostic, IdentifiedLocation, Warning}

import java.nio.file.Path

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

  protected val diagnosticKind: DiagnosticKind = diagnostic match {
    case _: Error   => DiagnosticKind.ERROR
    case _: Warning => DiagnosticKind.WARNING
    case _          => throw new IllegalStateException("Unexpected diagnostic type")
  }

  private def textAttrs: fansi.Attrs = diagnosticKind match {
    case DiagnosticKind.ERROR   => fansi.Color.Red ++ fansi.Bold.On
    case DiagnosticKind.WARNING => fansi.Color.Yellow ++ fansi.Bold.On
  }

  private def subject: String = diagnosticKind match {
    case DiagnosticKind.ERROR   => "error: "
    case DiagnosticKind.WARNING => "warning: "
  }

  protected lazy val sectionForDisplay: SourceSectionForDisplay = {
    val fileLocation: FileLocation =
      if (source.getPath == null && source.getName == null) {
        FileLocation.Unknown
      } else if (source.getPath != null) {
        FileLocation.SourcePath(source.getPath)
      } else {
        FileLocation.SourceName(source.getName)
      }
    sourceSectionFromDiagnostic match {
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

  def format(): String = {
    val str = sectionForDisplay.format()
    if (outSupportsAnsiColors) {
      str.render.stripLineEnd
    } else {
      str.plainText.stripLineEnd
    }
  }

  final def where(): SourceSection = sectionForDisplay.sourceSection

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

  private def sourceSectionFromDiagnostic: Option[SourceSection] =
    diagnostic.location match {
      case Some(location) =>
        if (location.length > source.getLength) {
          None
        } else {
          Some(source.createSection(location.start, location.length))
        }
      case None => None
    }

  private def shouldPrintLineNumber = sourceSectionFromDiagnostic match {
    case Some(section) =>
      section.getStartLine <= maxLineNum && section.getEndLine <= maxLineNum
    case None => false
  }

  sealed protected trait SourceSectionForDisplay {
    def sourceSection: SourceSection
    def format():      fansi.Str
  }

  sealed trait FileLocation
  protected object FileLocation {
    case class SourcePath(path: String) extends FileLocation {
      override def toString: String =
        try {
          val parsedPath = Path.of(path)
          RepositoryFinder
            .rewritePath(parsedPath)
            .toAbsolutePath
            .normalize()
            .toString
        } catch {
          case _: IllegalArgumentException =>
            path
        }
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
    fileLocation: FileLocation,
    lineNumber: Int,
    startColumn: Int,
    endColumn: Int
  ) extends SourceSectionForDisplay {
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
    fileLocation: FileLocation,
    startLine: Int,
    endLine: Int,
    startColumn: Int,
    endColumn: Int
  ) extends SourceSectionForDisplay {
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
    fileLocation: FileLocation
  ) extends SourceSectionForDisplay {
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
  def create(
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
