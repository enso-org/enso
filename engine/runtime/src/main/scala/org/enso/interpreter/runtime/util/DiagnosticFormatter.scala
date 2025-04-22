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

  private case class SingleLineLocation(
    sourceSection: SourceSection,
    srcPath: String,
    lineNumber: Int,
    startColumn: Int,
    endColumn: Int
  ) extends Location {
    override def format(): fansi.Str = {
      var str = fansi.Str()
      str ++= fansi
        .Str(srcPath + ":" + lineNumber + ":" + startColumn + ": ")
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
        file    = srcPath,
        line    = Some(lineNumber),
        endLine = None,
        col     = Some(startColumn),
        endCol  = Some(endColumn)
      )
  }

  private case class MultiLineLocation(
    sourceSection: SourceSection,
    srcPath: String,
    startLine: Int,
    endLine: Int,
    startColumn: Int,
    endColumn: Int
  ) extends Location {
    override def format(): Str = {
      var str = fansi.Str()
      str ++= fansi
        .Str(
          srcPath + ":[" + startLine + ":" + startColumn + "-" + endLine + ":" + endColumn + "]: "
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
        file    = srcPath,
        line    = Some(startLine),
        endLine = Some(endLine),
        col     = Some(startColumn),
        endCol  = Some(endColumn)
      )
  }

  private case class UnknownLocation(
    fileLocation: String
  ) extends Location {
    override def sourceSection: SourceSection = null

    override def format(): Str = {
      var str = fansi.Str()
      str ++= fansi
        .Str(fileLocation)
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
        val startColumn = section.getStartColumn
        val endColumn   = section.getEndColumn
        if (isOneLine) {
          val lineNumber = section.getStartLine
          SingleLineLocation(
            section,
            srcPath,
            lineNumber,
            startColumn,
            endColumn
          )
        } else {
          val startLine = section.getStartLine
          val endLine   = section.getEndLine
          MultiLineLocation(
            section,
            srcPath,
            startLine,
            endLine,
            startColumn,
            endColumn
          )
        }
      case None =>
        // There is no source section associated with the diagnostics
        val fileLocation = diagnostic.location match {
          case Some(_) =>
            fileLocationFromSectionOption(diagnostic.location, source)
          case None =>
            Option(source.getPath).getOrElse("<Unknown source>")
        }

        UnknownLocation(fileLocation)
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

  private def includeGithubAnnotation: Boolean =
    sys.env("GITHUB_ACTIONS") == "true"

  private case class GithubAnnotation(
    kind: DiagnosticKind,
    message: String,
    file: String,
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

      val parameters = Map(
        "file"             -> sanitizeParameter(file),
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
  }

  private def sanitizeMessage(message: String): String = {
    message.replace("%", "%25").replace("\n", "%0A").replace("::", "%3A%3A")
  }

  private def sanitizeParameter(message: String): String = {
    sanitizeMessage(message).replace(",", "%2C")
  }
}
