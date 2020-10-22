package org.enso.launcher.cli

import akka.http.scaladsl.model.{IllegalUriException, Uri}
import nl.gn0s1s.bump.SemVer
import org.enso.cli.arguments.{Argument, OptsParseError}
import org.enso.loggingservice.LogLevel

object Arguments {

  /** [[Argument]] instance that tries to parse the String as a [[SemVer]]
    * version string.
    */
  implicit val semverArgument: Argument[SemVer] = (string: String) =>
    SemVer(string).toRight(
      OptsParseError(s"`$string` is not a valid semantic version string.")
    )

  implicit val uriArgument: Argument[Uri] = (string: String) =>
    try {
      Right(Uri(string))
    } catch {
      case error: IllegalUriException =>
        Left(OptsParseError(s"`$string` is not a valid Uri: $error."))
    }

  implicit val logLevelArgument: Argument[LogLevel] = (string: String) => {
    val provided = string.toLowerCase
    LogLevel.allLevels
      .find(_.toString.toLowerCase == provided)
      .toRight(
        OptsParseError(s"`$string` is not a valid log level.")
      )
  }
}
