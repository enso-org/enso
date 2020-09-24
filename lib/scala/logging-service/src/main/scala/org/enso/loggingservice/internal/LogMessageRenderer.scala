package org.enso.loggingservice.internal

import org.enso.loggingservice.internal.protocol.WSLogMessage

trait LogMessageRenderer {
  def render(logMessage: WSLogMessage): String
}
