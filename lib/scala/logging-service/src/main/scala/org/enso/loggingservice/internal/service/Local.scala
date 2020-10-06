package org.enso.loggingservice.internal.service

import org.enso.loggingservice.LogLevel
import org.enso.loggingservice.internal.BlockingConsumerMessageQueue
import org.enso.loggingservice.internal.protocol.WSLogMessage
import org.enso.loggingservice.printers.Printer

/**
  * A local [[Service]] that handles all log messages locally.
  *
  * @param logLevel log level used to filter messages
  * @param queue log message queue
  * @param printers printers defining handling of the log messages
  */
case class Local(
  logLevel: LogLevel,
  queue: BlockingConsumerMessageQueue,
  printers: Seq[Printer]
) extends ThreadProcessingService {

  /**
    * Passes each message to all printers.
    */
  override protected def processMessage(message: WSLogMessage): Unit =
    printers.foreach(_.print(message))

  /**
    * Shuts down the printers.
    */
  override protected def afterShutdown(): Unit = {
    printers.foreach(_.shutdown())
  }
}

object Local {

  /**
    * Starts the [[Local]] service and returns it.
    *
    * @param logLevel log level used to filter messages
    * @param queue log message queue
    * @param printers printers defining handling of the log messages
    */
  def setup(
    logLevel: LogLevel,
    queue: BlockingConsumerMessageQueue,
    printers: Seq[Printer]
  ): Local = {
    val local = new Local(logLevel, queue, printers)
    local.startQueueProcessor()
    local
  }
}
