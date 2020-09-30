package org.enso.runner

import akka.http.scaladsl.model.Uri
import com.typesafe.scalalogging.Logger
import org.enso.loggingservice.printers.StderrPrinter
import org.enso.loggingservice.{LogLevel, LoggerMode, LoggingServiceManager}

import scala.util.{Failure, Success}

/**
  * Manages setting up the logging service within the runner.
  */
object RunnerLogging {

  /**
    * Sets up the runner's logging service.
    *
    * If `connectionUri` is provided it tries to connect to a logging service
    * server and pass logs to it. If it is not provided or the connection could
    * not be established, falls back to logging to standard error output.
    *
    * @param connectionUri optional uri of logging service server to connect to
    * @param logLevel log level to use for the runner and runtime
    */
  def setup(connectionUri: Option[Uri], logLevel: LogLevel): Unit = {
    val local =
      LoggerMode.Local(Seq(StderrPrinter.create(printExceptions = true)))
    connectionUri match {
      case Some(uri) =>
        import scala.concurrent.ExecutionContext.Implicits.global
        LoggingServiceManager
          .setupWithFallback(
            LoggerMode.Client(uri),
            local,
            logLevel
          )
          .onComplete {
            case Failure(exception) =>
              System.err.println(s"Failed to initialize logging: $exception")
            case Success(connected) =>
              val logger = Logger[RunnerLogging.type]
              if (connected) {
                logger.trace(s"Connected to logging service at `$uri`.")
              }
          }
      case None =>
        LoggingServiceManager.setup(local, logLevel)
    }
  }

}
