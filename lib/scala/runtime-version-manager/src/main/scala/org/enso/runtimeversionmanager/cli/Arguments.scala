package org.enso.runtimeversionmanager.cli

import akka.http.scaladsl.model.{IllegalUriException, Uri}
import org.enso.cli.arguments.{Argument, OptsParseError}
import org.enso.logger.LoggerUtils

import java.net.URI
import java.net.URISyntaxException
import org.slf4j.event.Level

object Arguments {
  implicit val uriAkkaArgument: Argument[Uri] = (string: String) =>
    try {
      Right(Uri(string))
    } catch {
      case error: IllegalUriException =>
        Left(OptsParseError(s"`$string` is not a valid Uri: $error."))
    }

  implicit val uriArgument: Argument[URI] = (string: String) =>
    try {
      Right(URI.create(string))
    } catch {
      case error: IllegalArgumentException =>
        Left(OptsParseError(s"`$string` is not a valid URI: $error."))
      case error: URISyntaxException =>
        Left(OptsParseError(s"`$string` is not a valid URI: $error."))
    }

  implicit val logLevelArgument: Argument[Level] = (string: String) => {
    val provided = LoggerUtils.backwardCompatibleName(string.toLowerCase)
    Level
      .values()
      .find(_.toString.toLowerCase == provided)
      .toRight(
        OptsParseError(s"`$string` is not a valid log level.")
      )
  }
}
