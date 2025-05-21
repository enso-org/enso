package org.enso.interpreter.runtime.util

import com.oracle.truffle.api.source.Source
import org.enso.compiler.core.ir.Diagnostic

import java.nio.file.Path

/** An extension of [[DiagnosticFormatter]] that additionally prints commands for a GitHub workflow that will add annotations for each warning/error. */
private[util] class GitHubDiagnosticFormatter(
  diagnostic: Diagnostic,
  source: Source,
  isOutputRedirected: Boolean,
  isColorTerminalOutput: Boolean
) extends DiagnosticFormatter(
      diagnostic,
      source,
      isOutputRedirected,
      isColorTerminalOutput
    ) {

  override def format(): String = {
    val githubCommand = createAnnotationCommandFor(sectionForDisplay).format()
    githubCommand + "\n" + super.format()
  }

  private case class GithubAnnotation(
    kind: DiagnosticKind,
    message: String,
    file: FileLocation,
    line: Option[Int],
    col: Option[Int],
    endLine: Option[Int],
    endCol: Option[Int]
  ) {
    def format(): String = {
      val annotationLevel = kind match {
        case DiagnosticKind.ERROR   => "error"
        case DiagnosticKind.WARNING => "warning"
      }

      val title = kind match {
        case DiagnosticKind.ERROR   => s"Enso Compiler Error @ $file"
        case DiagnosticKind.WARNING => s"Enso Compiler Warning @ $file"
      }

      val path = file match {
        case FileLocation.SourcePath(path) =>
          RepositoryFinder.rewritePath(Path.of(path)).toString
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

  private def createAnnotationCommandFor(
    location: SourceSectionForDisplay
  ): GithubAnnotation =
    location match {
      case SingleLineSection(
            _,
            fileLocation,
            lineNumber,
            startColumn,
            endColumn
          ) =>
        GithubAnnotation(
          kind    = diagnosticKind,
          message = diagnostic.formattedMessage(fileLocationFromSection),
          file    = fileLocation,
          line    = Some(lineNumber),
          endLine = None,
          col     = Some(startColumn),
          endCol  = Some(endColumn)
        )

      case MultiLineSection(
            _,
            fileLocation,
            startLine,
            endLine,
            startColumn,
            endColumn
          ) =>
        GithubAnnotation(
          kind    = diagnosticKind,
          message = diagnostic.formattedMessage(fileLocationFromSection),
          file    = fileLocation,
          line    = Some(startLine),
          endLine = Some(endLine),
          col     = Some(startColumn),
          endCol  = Some(endColumn)
        )

      case UnknownSection(fileLocation) =>
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
}

object GitHubDiagnosticFormatter {
  def shouldIncludeGithubAnnotations: Boolean =
    sys.env.get("ENSO_LINT_ENABLE_GITHUB_ANNOTATIONS").contains("true")
}
