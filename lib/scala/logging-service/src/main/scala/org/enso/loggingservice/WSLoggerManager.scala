package org.enso.loggingservice

import java.util.concurrent.LinkedTransferQueue

import org.enso.loggingservice.internal.{
  InternalLogMessage,
  Level,
  LoggerConnection
}

object WSLoggerManager {

  val maxQueueSizeForFallback: Int = 1000
  private val messageQueue         = new LinkedTransferQueue[InternalLogMessage]()
  private var currentLevel         = Level.Trace // TODO configurable
  object Connection extends LoggerConnection {
    override def send(message: InternalLogMessage): Unit = addMessage(message)
    override def logLevel: Level                         = currentLevel
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

  def startServer(): Unit = {
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
