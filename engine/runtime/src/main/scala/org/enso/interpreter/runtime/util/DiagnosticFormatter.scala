package org.enso.interpreter.runtime.util

import com.oracle.truffle.api.source.{Source, SourceSection}
import fansi.Str
import org.enso.compiler.core.ir.expression.Error
import org.enso.compiler.core.ir.{Diagnostic, IdentifiedLocation, Warning}

import java.nio.file.Path
import scala.annotation.tailrec

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

  sealed private trait DiagnosticKind

  private object DiagnosticKind {
    case object Error   extends DiagnosticKind
    case object Warning extends DiagnosticKind
  }

  private val diagnosticKind: DiagnosticKind = diagnostic match {
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

  private lazy val location = computeLocation()

  def format(): String = {
    val str = location.format()
    val text = if (outSupportsAnsiColors) {
      str.render.stripLineEnd
    } else {
      str.plainText.stripLineEnd
    }

    if (includeGithubAnnotation) {
      location.asGithubAnnotation().format() + "\n" + text
    } else text
  }

  def where(): SourceSection = location.sourceSection

  def fileLocationFromSection(loc: IdentifiedLocation): String = {
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

  sealed private trait Location {
    def sourceSection:        SourceSection
    def format():             fansi.Str
    def asGithubAnnotation(): GithubAnnotation
  }

  private object Location {
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

  private case class SingleLineSection(
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

    override def asGithubAnnotation(): GithubAnnotation =
      GithubAnnotation(
        kind    = diagnosticKind,
        message = diagnostic.formattedMessage(fileLocationFromSection),
        file    = fileLocation,
        line    = Some(lineNumber),
        endLine = None,
        col     = Some(startColumn),
        endCol  = Some(endColumn)
      )
  }

  private case class MultiLineSection(
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

    override def asGithubAnnotation(): GithubAnnotation =
      GithubAnnotation(
        kind    = diagnosticKind,
        message = diagnostic.formattedMessage(fileLocationFromSection),
        file    = fileLocation,
        line    = Some(startLine),
        endLine = Some(endLine),
        col     = Some(startColumn),
        endCol  = Some(endColumn)
      )
  }

  private case class UnknownSection(
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

    override def asGithubAnnotation(): GithubAnnotation =
      GithubAnnotation(
        kind    = diagnosticKind,
        message = diagnostic.formattedMessage(fileLocationFromSection),
        file    = fileLocation,
        line    = None,
        endLine = None,
        col     = None,
        endCol  = None
      )
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

  private def includeGithubAnnotation: Boolean =
    sys.env("GITHUB_ACTIONS") == "true"

  private case class GithubAnnotation(
    kind: DiagnosticKind,
    message: String,
    file: Location.FileLocation,
    line: Option[Int],
    col: Option[Int],
    endLine: Option[Int],
    endCol: Option[Int]
  ) {
    def format(): String = {
      val annotationLevel = kind match {
        case DiagnosticKind.Error   => "error"
        case DiagnosticKind.Warning => "warning"
      }

      val title = kind match {
        case DiagnosticKind.Error   => s"Enso Compiler Error @ $file"
        case DiagnosticKind.Warning => s"Enso Compiler Warning @ $file"
      }

      val path = file match {
        case Location.SourcePath(path) =>
          RepositoryFinder.root
            .map(_.relativize(Path.of(path)))
            .map(_.toString)
            .getOrElse(path)
        case _ => file.toString
      }
      val parameters = Map(
        "file"             -> sanitizeParameter(path),
        "title"            -> sanitizeParameter(title)
      ) ++ line.map("line" -> _.toString) ++ col.map(
        "col" -> _.toString
      ) ++ endLine.map("endLine" -> _.toString) ++ endCol.map(
        "endCol" -> _.toString
      )

      val parametersStr = parameters
        .map { case (k, v) => s"$k=$v" }
        .mkString(",")

      s"::${annotationLevel} $parametersStr::${sanitizeMessage(message)}"
    }

    private def sanitizeMessage(message: String): String = {
      message.replace("%", "%25").replace("\n", "%0A").replace("::", "%3A%3A")
    }

    private def sanitizeParameter(message: String): String = {
      sanitizeMessage(message).replace(",", "%2C")
    }
  }

  private object RepositoryFinder {
    @tailrec
    private def findRepositoryRoot(path: Path): Option[Path] = {
      val gitDir = path.resolve(".git")
      if (gitDir.toFile.exists()) {
        Some(path)
      } else {
        val parent = path.getParent
        if (parent != null) {
          findRepositoryRoot(parent)
        } else {
          None
        }
      }
    }

    lazy val root: Option[Path] = {
      val currentDir = Path.of(".").toAbsolutePath.normalize()
      findRepositoryRoot(currentDir)
    }
  }
}
