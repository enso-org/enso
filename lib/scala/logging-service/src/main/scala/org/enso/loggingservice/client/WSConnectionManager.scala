package org.enso.loggingservice.client

import java.util.concurrent.LinkedTransferQueue

import org.enso.loggingservice.internal.{
  InternalLogMessage,
  Level,
  LoggerConnection
}

object WSConnectionManager {

  val maxQueueSizeForFallback: Int = 1000
  private val messageQueue         = new LinkedTransferQueue[InternalLogMessage]()
  object Connection extends LoggerConnection {
    override def send(message: InternalLogMessage): Unit = addMessage(message)
    override def logLevel: Level                         = ???
  }

  private var actualConnection: Option[Nothing] = None

  private def addMessage(message: InternalLogMessage): Unit = {
    System.err.println(s"addMessage($message)")
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
//    actualConnection = ???
    ???
  }
}
