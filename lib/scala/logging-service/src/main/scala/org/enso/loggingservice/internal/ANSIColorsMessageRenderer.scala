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

  def renderLevelWithColors(logLevel: LogLevel): String =
    logLevel match {
      case LogLevel.Error   => s"${AnsiColor.RED}error${AnsiColor.RESET}"
      case LogLevel.Warning => s"${AnsiColor.YELLOW}warn${AnsiColor.RESET}"
      case LogLevel.Info    => "info"
      case LogLevel.Debug   => s"${AnsiColor.CYAN}debug${AnsiColor.RESET}"
      case LogLevel.Trace   => s"${AnsiColor.CYAN}trace${AnsiColor.RESET}"
      case _                => "none"
    }
}
