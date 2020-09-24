package org.enso.loggingservice.internal.serviceconnection

import org.enso.loggingservice.LogLevel
import org.enso.loggingservice.internal.BlockingConsumerMessageQueue
import org.enso.loggingservice.printers.Printer

case class Fallback(thread: Thread) extends Service {
  def terminate(): Unit = {
    thread.interrupt()
  }
}

object Fallback {
  def setup(
    printers: Seq[Printer],
    logLevel: LogLevel,
    queue: BlockingConsumerMessageQueue
  ): Fallback = {
    val thread = new Thread(() => run(printers, logLevel, queue))
    thread.start()
    Fallback(thread)
  }

  private def run(
    printers: Seq[Printer],
    logLevel: LogLevel,
    queue: BlockingConsumerMessageQueue
  ): Unit = {
    try {
      while (!Thread.currentThread().isInterrupted) {
        val logMessage = queue.nextMessage()
        if (logLevel.shouldLog(logMessage.logLevel)) {
          printers.foreach(_.print(logMessage))
        }
      }
    } catch {
      case _: InterruptedException =>
    }
  }
}
