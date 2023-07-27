package org.enso.runtimeversionmanager.cli

import akka.http.scaladsl.model.{IllegalUriException, Uri}
import org.enso.cli.arguments.{Argument, OptsParseError}
import org.enso.logger.Converter
//import org.enso.loggingservice.LogLevel
import org.slf4j.event.Level

object Arguments {
  implicit val uriArgument: Argument[Uri] = (string: String) =>
    try {
      Right(Uri(string))
    } catch {
      case error: IllegalUriException =>
        Left(OptsParseError(s"`$string` is not a valid Uri: $error."))
    }

  implicit val logLevelArgument: Argument[Level] = (string: String) => {
    val provided = Converter.backwardCompatibleName(string.toLowerCase)
    Level
      .values()
      //LogLevel.allLevels
      .find(_.toString.toLowerCase == provided)
      .toRight(
        OptsParseError(s"`$string` is not a valid log level.")
      )
  }
}
