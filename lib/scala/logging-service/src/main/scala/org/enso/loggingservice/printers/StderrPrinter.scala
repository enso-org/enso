package org.enso.loggingservice.printers

import org.enso.loggingservice.internal.DefaultLogMessageRenderer
import org.enso.loggingservice.internal.protocol.WSLogMessage

object StderrPrinter extends Printer {
  def print(logMessage: WSLogMessage): Unit = {
    val lines = DefaultLogMessageRenderer.render(logMessage)
    System.err.println(lines)
  }

  override def shutdown(): Unit =
    System.err.flush()
}
