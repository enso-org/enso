package org.enso.loggingservice.printers

import org.enso.loggingservice.internal.DefaultLogMessageRenderer
import org.enso.loggingservice.internal.protocol.WSLogMessage

class StderrPrinter(printStackTraces: Boolean) extends Printer {
  private val renderer = new DefaultLogMessageRenderer(printStackTraces)

  def print(logMessage: WSLogMessage): Unit = {
    val lines = renderer.render(logMessage)
    System.err.println(lines)
  }

  override def shutdown(): Unit =
    System.err.flush()
}

object StderrPrinter {
  def create(printStackTraces: Boolean = false): StderrPrinter =
    new StderrPrinter(printStackTraces)
}
