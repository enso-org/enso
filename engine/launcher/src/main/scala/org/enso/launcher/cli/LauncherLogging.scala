package org.enso.launcher.cli

import akka.http.scaladsl.model.Uri
import com.typesafe.scalalogging.Logger
import org.enso.launcher.installation.DistributionManager
import org.enso.loggingservice.printers.{
  FileOutputPrinter,
  StderrPrinterWithColors
}
import org.enso.loggingservice.{LogLevel, WSLoggerManager, WSLoggerMode}

import scala.util.control.NonFatal

object LauncherLogging {
  def setup(
    logLevel: Option[LogLevel],
    connectToExternalLogger: Option[Uri]
  ): Unit = {
    val actualLogLevel =
      logLevel.getOrElse(LogLevel.Debug) // TODO [RW] info
    val stderrPrinter = StderrPrinterWithColors.colorPrinterIfAvailable()

    val fallbackMode = WSLoggerMode.Local(Seq(stderrPrinter))
    connectToExternalLogger match {
      case Some(uri) =>
        WSLoggerManager.setupWithFallbackToLocal(
          WSLoggerMode.Client(uri),
          fallbackMode,
          actualLogLevel
        )
      case None =>
        val printers =
          try {
            val filePrinter =
              FileOutputPrinter.create(DistributionManager.paths.logs)
            Seq(stderrPrinter, filePrinter)
          } catch {
            case NonFatal(error) =>
              Logger[LauncherLogging.type].error(
                "Failed to initialize the write-to-file logger, " +
                "falling back to stderr only.",
                error
              )
              Seq(stderrPrinter)
          }
        // TODO [RW] automatic port fnding
        WSLoggerManager.setupWithFallbackToLocal(
          WSLoggerMode.Server(8080, printers = printers),
          fallbackMode,
          actualLogLevel
        )
    }
  }
}
