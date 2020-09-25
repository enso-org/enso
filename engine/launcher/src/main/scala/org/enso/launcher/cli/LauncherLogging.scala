package org.enso.launcher.cli

import akka.http.scaladsl.model.Uri
import com.typesafe.scalalogging.Logger
import org.enso.launcher.installation.DistributionManager
import org.enso.loggingservice.printers.{
  FileOutputPrinter,
  Printer,
  StderrPrinter,
  StderrPrinterWithColors
}
import org.enso.loggingservice.{LogLevel, WSLoggerManager, WSLoggerMode}

import scala.util.control.NonFatal
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

object LauncherLogging {
  private val logger            = Logger[LauncherLogging.type]
  val defaultLogLevel: LogLevel = LogLevel.Warning

  /**
    * Sets up launcher's logging service as either a server that gathers other
    * component's logs or a client that forwards them further.
    *
    * Forwarding logs to another server in the launcher is an internal,
    * development-mode feature that is not designed to be used by end-users
    * unless they specifically know what they are doing. Redirecting logs to an
    * external server may result in some important information not being printed
    * by the launcher as it is forwarded.
    *
    * @param logLevel the log level to use for launcher's logs; does not affect
    *                 other component's log level, which has to be set
    *                 separately
    * @param connectToExternalLogger specifies an Uri of an external logging
    *                                service that the launcher should forward
    *                                its logs to; advanced feature, use with
    *                                caution
    */
  def setup(
    logLevel: Option[LogLevel],
    connectToExternalLogger: Option[Uri]
  ): Unit = {
    val actualLogLevel = logLevel.getOrElse(defaultLogLevel)
    connectToExternalLogger match {
      case Some(uri) =>
        WSLoggerManager
          .setupWithFallbackToLocal(
            WSLoggerMode.Client(uri),
            WSLoggerMode.Local(),
            actualLogLevel
          )
          .onComplete {
            case Failure(exception) =>
              logger.error("Failed to initialize the logger.", exception)
            case Success(connected) =>
              if (connected) {
                loggingServiceEndpointOpt = Some(uri)
                val msg =
                  s"Log messages from this launcher are forwarded to `$uri`."
                System.err.println(msg)
              }
          }
      case None =>
        setupLoggingServer(actualLogLevel)
    }
  }

  /**
    * Sets up a fallback logger that just logs to stderr.
    *
    * It can be used when the application has failed to parse the CLI options
    * and does not know which logger to set up.
    */
  def setupFallback(): Unit = {
    WSLoggerManager.setup(WSLoggerMode.Local(), defaultLogLevel)
  }

  /**
    * Returns a [[Uri]] of the logging service that launched components can
    * connect to.
    *
    * Points to the local server if it has been set up, or to the endpoint that
    * the launcher was told to connect to. May be empty if the initialization
    * failed and local logging is used as a fallback.
    */
  def loggingServiceEndpoint(): Option[Uri] = loggingServiceEndpointOpt

  private var loggingServiceEndpointOpt: Option[Uri] = None

  /**
    * Returns a printer for outputting the logs to the standard error output, if
    * it is enabled.
    */
  private def stderrPrinter(): Option[Printer] =
    Some(StderrPrinterWithColors.colorPrinterIfAvailable())

  private def setupLoggingServer(logLevel: LogLevel): Unit = {
    val printers =
      try {
        val filePrinter =
          FileOutputPrinter.create(DistributionManager.paths.logs)
        stderrPrinter().toSeq ++ Seq(filePrinter)
      } catch {
        case NonFatal(error) =>
          logger.error(
            "Failed to initialize the write-to-file logger, " +
            "falling back to stderr only.",
            error
          )
          Seq(StderrPrinter)
      }

    WSLoggerManager
      .setup(WSLoggerMode.Server(printers), logLevel)
      .onComplete {
        case Failure(exception) =>
          logger.error(
            s"Failed to initialize the logging service server: $exception",
            exception
          )
          logger.warn("Falling back to local-only logger.")
          WSLoggerManager.setup(
            WSLoggerMode.Local(printers),
            logLevel
          )
        case Success(serverBinding) =>
          val uri = serverBinding.toUri()
          loggingServiceEndpointOpt = Some(uri)
          logger.trace(
            s"Logging service has been set-up and is listening at `$uri`."
          )
      }
  }
}
