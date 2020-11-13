package org.enso.loggingservice.printers

import org.enso.loggingservice.internal.protocol.WSLogMessage

/** Defines a strategy for outputting the log messages.
  *
  * It can output them to the console, a file, a database etc.
  */
trait Printer {

  /** Outputs the log message.
    */
  def print(message: WSLogMessage): Unit

  /** Shuts down this output channel.
    *
    * It should flush any buffers and release resources.
    */
  def shutdown(): Unit
}
