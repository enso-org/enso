package org.enso.loggingservice.internal.serviceconnection

import org.enso.loggingservice.LogLevel
import org.enso.loggingservice.internal.BlockingConsumerMessageQueue
import org.enso.loggingservice.internal.protocol.WSLogMessage
import org.enso.loggingservice.printers.Printer

case class Fallback(
  logLevel: LogLevel,
  queue: BlockingConsumerMessageQueue,
  printers: Seq[Printer]
) extends ThreadProcessingService {
  override protected def processMessage(message: WSLogMessage): Unit =
    printers.foreach(_.print(message))
}

object Fallback {
  def setup(
    logLevel: LogLevel,
    queue: BlockingConsumerMessageQueue,
    printers: Seq[Printer]
  ): Fallback = {
    val fallback = new Fallback(logLevel, queue, printers)
    fallback.startQueueProcessor()
    fallback
  }
}
