package org.enso.runner

import akka.http.scaladsl.model.Uri
import com.typesafe.scalalogging.Logger
import org.enso.loggingservice.printers.StderrPrinter
import org.enso.loggingservice.{LogLevel, LoggerMode, LoggingServiceManager}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
  * Manages setting up the logging service within the runner.
  */
object RunnerLogging {

  /**
    * Sets up the runner's logging service.
    *
    * If `connectionUri` is provided it tries to connect to a logging service
    * server and pass logs to it. If it is not provided, or the connection could
    * not be established, falls back to logging to standard error output.
    *
    * @param connectionUri optional uri of logging service server to connect to
    * @param logLevel log level to use for the runner and runtime
    */
  def setup(connectionUri: Option[Uri], logLevel: LogLevel): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val loggerSetup = connectionUri match {
      case Some(uri) =>
        LoggingServiceManager
          .setup(
            LoggerMode.Client(uri),
            logLevel
          )
          .map { _ =>
            logger.trace(s"Connected to logging service at `$uri`.")
          }
          .recoverWith { _ =>
            logger.error(
              "Failed to connect to the logging service server, " +
              "falling back to local logging."
            )
            setupLocalLogger(logLevel)
          }
      case None =>
        setupLocalLogger(logLevel)
    }
    loggerSetup.onComplete {
      case Failure(exception) =>
        System.err.println(s"Failed to initialize logging: $exception")
        exception.printStackTrace()
      case Success(_) =>
    }
  }

  private def setupLocalLogger(logLevel: LogLevel): Future[Unit] =
    LoggingServiceManager
      .setup(
        LoggerMode.Local(
          Seq(StderrPrinter.create(printExceptions = true))
        ),
        logLevel
      )

  private val logger = Logger[RunnerLogging.type]

  /**
    * Shuts down the logging service gracefully.
    */
  def tearDown(): Unit =
    LoggingServiceManager.tearDown()
}
