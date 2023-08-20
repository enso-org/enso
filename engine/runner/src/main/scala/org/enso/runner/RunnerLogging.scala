package org.enso.runner

import java.net.URI
import com.typesafe.scalalogging.Logger
import org.enso.logger.masking.Masking

import scala.annotation.unused
import org.slf4j.event.Level

import scala.util.{Failure, Success}
import scala.concurrent.Future
import org.enso.logger.LoggerSetup
import org.enso.logger.config.LoggingServiceConfig

/** Manages setting up the logging service within the runner.
  */
object RunnerLogging {

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
    val loggingConfig = LoggingServiceConfig.parseConfig()
    val loggerSetup = connectionUri match {
      case Some(uri) =>
        Future {
          LoggerSetup.setupSocketAppender(
            logLevel,
            uri.getHost(),
            uri.getPort(),
            loggingConfig
          )
        }
          .map(success =>
            if (success) ()
            else throw new RuntimeException("setup failed")
          )
          .map { _ =>
            logger.trace("Connected to logging service at [{}].", uri)
          }
          .recoverWith { _ =>
            System.err.println(
              "Failed to connect to the logging service server, " +
              "falling back to local logging."
            )
            setupLocalLogger(logLevel, loggingConfig)
          }
      case None =>
        setupLocalLogger(logLevel, loggingConfig)
    }
    loggerSetup.onComplete {
      case Failure(exception) =>
        exception.printStackTrace()
      case Success(_) =>
    }
  }

  private def setupLocalLogger(
    @unused logLevel: Level,
    config: LoggingServiceConfig
  ): Future[Unit] = {
    Future.successful(
      LoggerSetup.setupConsoleAppender(
        logLevel,
        config
      )
    )
  }

  private val logger = Logger[RunnerLogging.type]

  /** Shuts down the logging service gracefully.
    */
  def tearDown(): Unit = {
    LoggerSetup.teardown()
  }
}
