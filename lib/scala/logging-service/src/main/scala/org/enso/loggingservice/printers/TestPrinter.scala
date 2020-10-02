package org.enso.loggingservice.printers

import org.enso.loggingservice.TestLogger.TestLogMessage
import org.enso.loggingservice.internal.protocol.WSLogMessage

/**
  * A [[Printer]] instance that may be used in tests to gather reported log
  * messages.
  */
class TestPrinter extends Printer {
  private val messages =
    scala.collection.mutable.ListBuffer.empty[TestLogMessage]

  /**
    * @inheritdoc
    */
  override def print(message: WSLogMessage): Unit = {
    val testMessage =
      TestLogMessage(logLevel = message.level, message = message.message)
    messages.synchronized {
      messages.append(testMessage)
    }
  }

  @volatile private var alreadyShutdown = false

  /**
    * @inheritdoc
    */
  def shutdown(): Unit = {
    alreadyShutdown = true
  }

  /**
    * Reports if [[shutdown]] has been called.
    */
  def wasShutdown: Boolean = alreadyShutdown

  /**
    * Returns all log messages that have been gathered so far.
    */
  def getLoggedMessages: Seq[TestLogMessage] = messages.toSeq
}
