package org.enso.loggingservice.printers

import org.enso.loggingservice.internal.DefaultLogMessageRenderer
import org.enso.loggingservice.internal.protocol.WSLogMessage

/**
  * Prints the log messages to the standard error output.
  *
  * @param printExceptions specifies if attached exceptions should be printed
  */
class StderrPrinter(printExceptions: Boolean) extends Printer {
  private val renderer = new DefaultLogMessageRenderer(printExceptions)

  /**
    * @inheritdoc
    */
  override def print(logMessage: WSLogMessage): Unit = {
    val lines = renderer.render(logMessage)
    System.err.println(lines)
  }

  /**
    * @inheritdoc
    */
  override def shutdown(): Unit =
    System.err.flush()
}

object StderrPrinter {

  /**
    * Creates an instance of [[StderrPrinter]].
    */
  def create(printExceptions: Boolean = false): StderrPrinter =
    new StderrPrinter(printExceptions)
}
