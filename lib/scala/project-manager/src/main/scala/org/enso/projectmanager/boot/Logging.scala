package org.enso.projectmanager.boot

import org.enso.loggingservice.printers.StderrPrinterWithColors
import org.enso.loggingservice.{LogLevel, LoggerMode, LoggingServiceManager}

import scala.concurrent.{ExecutionContext, Future}

/** A helper for setting up the logging service in the Project Manager. */
object Logging {

  /** Sets up the logging service for local logging. */
  def setup(
    logLevel: LogLevel,
    executionContext: ExecutionContext
  ): Future[Unit] = {
    // TODO [RW] setting up the logging server will be added in #1151
    val printer = StderrPrinterWithColors.colorPrinterIfAvailable(false)
    LoggingServiceManager.setup(LoggerMode.Local(Seq(printer)), logLevel)(
      executionContext
    )
  }
}
