package org.enso.runner

import java.net.URI
import com.typesafe.scalalogging.Logger
import org.enso.logger.LoggerSetup
import org.enso.logger.masking.Masking
import org.slf4j.event.Level

import scala.util.{Failure, Success}
import scala.concurrent.Future

/** Manages setting up the logging service within the runner.
  */
object RunnerLogging {

  private val logger = Logger[RunnerLogging.type]

  /** Sets up the runner's logging service.
    *
    * If `connectionUri` is provided it tries to connect to a logging service
    * server and pass logs to it. If it is not provided, or the connection could
    * not be established, falls back to logging to standard error output.
    *
    * @param connectionUri optional uri of logging service server to connect to
    * @param logLevel log level to use for the runner and runtime
    * @param logMasking switches log masking on and off
    */
  def setup(
    connectionUri: Option[URI],
    logLevel: Level,
    logMasking: Boolean
  ): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    Masking.setup(logMasking)
    val loggerSetup = LoggerSetup.get()
    val initializedLogger = connectionUri match {
      case Some(uri) =>
        Future {
          loggerSetup.setupSocketAppender(
            logLevel,
            uri.getHost(),
            uri.getPort()
          )
        }
          .map(success =>
            if (success) {
              logger.trace("Connected to logging service at [{}].", uri)
              true
            } else
              throw new RuntimeException("Failed to connect to logging service")
          )
          .recoverWith[Boolean] { _ =>
            System.err.println(
              "Failed to connect to the logging service server, " +
              "falling back to local logging."
            )
            Future.successful(loggerSetup.setupConsoleAppender(logLevel))
          }
      case None =>
        Future.successful(loggerSetup.setupConsoleAppender(logLevel))
    }
    initializedLogger.onComplete {
      case Failure(exception) =>
        exception.printStackTrace()
        System.err.println("Logger setup: " + exception.getMessage)
      case Success(success) =>
        if (!success) {
          System.err.println("Failed to initialize logging infrastructure")
        }
    }
  }

  /** Shuts down the logging service gracefully.
    */
  def tearDown(): Unit = {
    LoggerSetup.get().teardown()
  }
}
