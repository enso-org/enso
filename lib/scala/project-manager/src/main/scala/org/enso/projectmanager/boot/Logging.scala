package org.enso.projectmanager.boot

import org.enso.loggingservice.printers.StderrPrinterWithColors
import org.enso.loggingservice.{LogLevel, LoggerMode, LoggingServiceManager}

import scala.concurrent.{ExecutionContext, Future}

object Logging {

  def setup(
    logLevel: LogLevel,
    executionContext: ExecutionContext
  ): Future[Unit] = {
    val printer = StderrPrinterWithColors.colorPrinterIfAvailable(false)
    LoggingServiceManager.setup(LoggerMode.Local(Seq(printer)), logLevel)(
      executionContext
    )
  }
}
