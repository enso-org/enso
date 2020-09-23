package org.enso.loggingservice.internal

trait LogMessageRenderer {
  def render(logMessage: WSLogMessage): String
}
