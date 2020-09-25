package org.enso.loggingservice.internal.serviceconnection

import org.enso.loggingservice.LogLevel
import org.enso.loggingservice.internal.BlockingConsumerMessageQueue
import org.enso.loggingservice.internal.protocol.WSLogMessage

trait ThreadProcessingService extends Service {
  protected def queue:    BlockingConsumerMessageQueue
  protected def logLevel: LogLevel

  protected def processMessage(message: WSLogMessage): Unit

  private var queueThread: Option[Thread] = None
  protected def startQueueProcessor(): Unit = {
    val thread = new Thread(() => runQueue())
    queueThread = Some(thread)
    thread.start()
  }

  private def runQueue(): Unit = {
    try {
      while (!Thread.currentThread().isInterrupted) {
        val message = queue.nextMessage()
        if (logLevel.shouldLog(message.logLevel)) {
          processMessage(message)
        }
      }
    } catch {
      case _: InterruptedException =>
    }
  }

  abstract override def terminate(): Unit = {
    super.terminate()
    queueThread match {
      case Some(thread) =>
        thread.interrupt()
        thread.join(100)
      case None =>
    }
  }
}
