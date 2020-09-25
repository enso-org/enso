package org.enso.loggingservice.internal
import org.enso.loggingservice.LogLevel
import org.enso.loggingservice.internal.protocol.WSLogMessage

import scala.io.AnsiColor

class ANSIColorsMessageRenderer extends DefaultLogMessageRenderer {
  override def render(logMessage: WSLogMessage): String = {
    val level     = renderLevelWithColors(logMessage.logLevel)
    val timestamp = renderTimestamp(logMessage.timestamp)
    val base =
      s"[$level] [$timestamp] [${logMessage.group}] ${logMessage.message}"
    logMessage.exception match {
      case Some(exception) =>
        base + "\n" + renderException(exception)
      case None => base
    }
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
