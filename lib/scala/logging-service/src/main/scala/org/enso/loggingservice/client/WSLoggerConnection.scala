package org.enso.loggingservice.client

import java.util.concurrent.LinkedTransferQueue

import org.enso.loggingservice.client.internal.LogMessage

object WSLoggerConnection {

  val maxQueueSizeForFallback: Int = 1000
  private val messageQueue         = new LinkedTransferQueue[LogMessage]()

  private var actualConnection: Option[Nothing] = None

  private def addMessage(message: LogMessage): Unit = {
    if (
      actualConnection.isEmpty && messageQueue.size() > maxQueueSizeForFallback
    ) {
      startFallbackStderrLogger()
    }
    messageQueue.add(message)
  }

  /**
    * TODO must not block
    */
  def establishConnection(): Unit = {
    ???
  }

  def startFallbackStderrLogger(): Unit =
    actualConnection.synchronized {
      actualConnection match {
        case Some(_) =>
        case None =>
          setUpFallback()
      }
    }

  private def setUpFallback(): Unit = {
    actualConnection = Some(???)
  }
}
