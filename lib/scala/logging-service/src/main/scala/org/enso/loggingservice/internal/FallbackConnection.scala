package org.enso.loggingservice.internal

import java.util.concurrent.LinkedTransferQueue

import org.enso.loggingservice.LoggingConfig

case class FallbackConnection(thread: Thread) extends ServiceConnection {
  def terminate(): Unit = {
    thread.interrupt()
  }
}

object FallbackConnection {
  def setup(
    config: LoggingConfig,
    queue: LinkedTransferQueue[InternalLogMessage]
  ): FallbackConnection = {
    if (config.outputToFile.isDefined)
      throw new NotImplementedError("File output is not implemented currently")
    val thread = new Thread(() => run(config, queue))
    thread.start()
    FallbackConnection(thread)
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
