package org.enso.loggingservice.internal.serviceconnection

import org.enso.loggingservice.LogLevel
import org.enso.loggingservice.internal.BlockingConsumerMessageQueue
import org.enso.loggingservice.internal.protocol.WSLogMessage
import org.enso.loggingservice.printers.Printer

case class Local(
  logLevel: LogLevel,
  queue: BlockingConsumerMessageQueue,
  printers: Seq[Printer]
) extends ThreadProcessingService {
  override protected def processMessage(message: WSLogMessage): Unit =
    printers.foreach(_.print(message))

  override protected def shutdownProcessors(): Unit =
    printers.foreach(_.shutdown())
}

object Local {
  def setup(
    logLevel: LogLevel,
    queue: BlockingConsumerMessageQueue,
    printers: Seq[Printer]
  ): Local = {
    val fallback = new Local(logLevel, queue, printers)
    fallback.startQueueProcessor()
    fallback
  }
}
