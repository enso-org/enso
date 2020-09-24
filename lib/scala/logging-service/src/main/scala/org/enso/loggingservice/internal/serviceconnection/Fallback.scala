package org.enso.loggingservice.internal.serviceconnection

import java.util.concurrent.LinkedTransferQueue

import org.enso.loggingservice.LoggingConfig
import org.enso.loggingservice.internal.{InternalLogMessage, StderrPrinter}

case class Fallback(thread: Thread) extends Service {
  def terminate(): Unit = {
    thread.interrupt()
  }
}

object Fallback {
  def setup(
    config: LoggingConfig,
    queue: LinkedTransferQueue[InternalLogMessage]
  ): Fallback = {
    if (config.outputToFile.isDefined)
      throw new NotImplementedError("File output is not implemented currently")
    val thread = new Thread(() => run(config, queue))
    thread.start()
    Fallback(thread)
  }

  private def run(
    config: LoggingConfig,
    queue: LinkedTransferQueue[InternalLogMessage]
  ): Unit = {
    try {
      // TODO file output open file etc. Using
      while (!Thread.currentThread().isInterrupted) {
        val logMessage = queue.take().toLogMessage()
        if (config.stderrEnabled) {
          if (config.ansiColors) StderrPrinter.printColors(logMessage)
          else StderrPrinter.print(logMessage)
        }
      }
    } catch {
      case _: InterruptedException =>
    }
  }
}
