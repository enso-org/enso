package org.enso.loggingservice.internal
import org.enso.loggingservice.LogLevel

import scala.io.AnsiColor

/**
  * Renders log messages in the same way as [[DefaultLogMessageRenderer]] but
  * adds ANSI escape codes to display the log level in color.
  */
class ANSIColorsMessageRenderer(printExceptions: Boolean)
    extends DefaultLogMessageRenderer(printExceptions) {

  /**
    * @inheritdoc
    */
  override def renderLevel(logLevel: LogLevel): String = {
    val color = logLevel match {
      case LogLevel.Error   => Some(AnsiColor.RED)
      case LogLevel.Warning => Some(AnsiColor.YELLOW)
      case LogLevel.Debug   => Some(AnsiColor.CYAN)
      case LogLevel.Trace   => Some(AnsiColor.CYAN)
      case _                => None
    }
    color match {
      case Some(ansiColor) =>
        s"$ansiColor${super.renderLevel(logLevel)}${AnsiColor.RESET}"
      case None => super.renderLevel(logLevel)
    }
  }
}
