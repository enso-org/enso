package org.enso.loggingservice.internal
import org.enso.loggingservice.LogLevel
import org.enso.loggingservice.internal.protocol.WSLogMessage

import scala.io.AnsiColor

class ANSIColorsMessageRenderer(printStackTraces: Boolean)
    extends DefaultLogMessageRenderer(printStackTraces) {
  override def render(logMessage: WSLogMessage): String = {
    val level     = renderLevelWithColors(logMessage.logLevel)
    val timestamp = renderTimestamp(logMessage.timestamp)
    val base =
      s"[$level] [$timestamp] [${logMessage.group}] ${logMessage.message}"
    addStackTrace(base, logMessage.exception)
  }

  def renderLevelWithColors(logLevel: LogLevel): String = {
    val color = logLevel match {
      case LogLevel.Error   => Some(AnsiColor.RED)
      case LogLevel.Warning => Some(AnsiColor.YELLOW)
      case LogLevel.Debug   => Some(AnsiColor.CYAN)
      case LogLevel.Trace   => Some(AnsiColor.CYAN)
      case _                => None
    }
    color match {
      case Some(ansiColor) =>
        s"$ansiColor${renderLevel(logLevel)}${AnsiColor.RESET}"
      case None => renderLevel(logLevel)
    }
  }
}
