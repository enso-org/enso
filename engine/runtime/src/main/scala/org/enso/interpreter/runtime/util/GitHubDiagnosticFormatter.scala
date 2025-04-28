package org.enso.interpreter.runtime.util

import com.oracle.truffle.api.source.Source
import org.enso.compiler.core.ir.Diagnostic

import java.nio.file.Path
import scala.annotation.tailrec

/** An extension of [[DiagnosticFormatter]] that additionally prints commands for a GitHub workflow that will add annotations for each warning/error. */
class GitHubDiagnosticFormatter(
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
    createAnnotationCommandFor(location).format() + "\n" + super.format()
  }

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

  private def createAnnotationCommandFor(location: Location): GithubAnnotation =
    location match {
      case SingleLineSection(
            sourceSection,
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
            sourceSection,
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
    sys.env.get("GITHUB_ACTIONS").contains("true")
}
