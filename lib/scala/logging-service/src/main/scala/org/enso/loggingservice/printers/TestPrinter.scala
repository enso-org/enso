package org.enso.loggingservice.printers

import org.enso.loggingservice.TestLogger.TestLogMessage
import org.enso.loggingservice.internal.protocol.WSLogMessage

class TestPrinter extends Printer {
  private val messages =
    scala.collection.mutable.ListBuffer.empty[TestLogMessage]

  def print(message: WSLogMessage): Unit = {
    val testMessage =
      TestLogMessage(logLevel = message.logLevel, message = message.message)
    messages.synchronized {
      messages.append(testMessage)
    }
  }

  def shutdown(): Unit = {}

  def getLoggedMessages(): Seq[TestLogMessage] = messages.toSeq
}
