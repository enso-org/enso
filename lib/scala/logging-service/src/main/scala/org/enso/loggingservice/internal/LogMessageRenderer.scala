package org.enso.loggingservice.internal

import org.enso.loggingservice.internal.protocol.WSLogMessage

/**
  * Specifies a strategy of rendering log messages to string.
  */
trait LogMessageRenderer {

  /**
    * Creates a string representation of the log message that can be written to
    * an output.
    */
  def render(logMessage: WSLogMessage): String
}
