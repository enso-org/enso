package org.enso.runner

import java.net.URI
import com.typesafe.scalalogging.Logger
import org.enso.logger.masking.Masking

import scala.annotation.unused
import org.slf4j.event.Level

import scala.util.{Failure, Success}
import scala.concurrent.Future
import java.io.InputStream
import org.enso.logger.LoggerContextSetup

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
    val loggingConfig = this.getClass.getResourceAsStream("/runner.logback.xml")
    val loggerSetup = connectionUri match {
      case Some(uri) =>
        Future(
          LoggerContextSetup.setup(
            logLevel,
            "runner",
            loggingConfig,
            "forward-via-socket",
            uri.getHost(),
            uri.getPort()
          )
        )
          .map { _ =>
            logger.trace("Connected to logging service at [{}].", uri)
          }
          .recoverWith { _ =>
            logger.error(
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
        System.err.println(s"Failed to initialize logging: $exception")
        exception.printStackTrace()
      case Success(_) =>
    }
  }

  private def setupLocalLogger(
    @unused logLevel: Level,
    logbackConfig: InputStream
  ): Future[Unit] = {
    Future.successful(
      LoggerContextSetup.setup(
        logLevel,
        "runner",
        logbackConfig,
        "console",
        null,
        0
      )
    )
  }

  private val logger = Logger[RunnerLogging.type]

  /** Shuts down the logging service gracefully.
    */
  def tearDown(): Unit = {
    LoggerContextSetup.teardown()
  }
}
