package org.enso.runtimeversionmanager.cli

import akka.http.scaladsl.model.{IllegalUriException, Uri}
import org.enso.cli.arguments.{Argument, OptsParseError}
import org.enso.loggingservice.LogLevel

object Arguments {
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
