package org.enso.logging

import org.enso.logger.LogbackSetup
import org.enso.logger.config.LoggerSetup
import org.slf4j.event.Level

import java.net.URI
import java.nio.file.Path
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.Promise
import scala.util.{Failure, Success}
import org.enso.logger.masking.Masking

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

abstract class LoggingSetupHelper(implicit executionContext: ExecutionContext) {
  val defaultLogLevel: Level

  val logFileSuffix: String

  def logPath: Path

  def loggingServiceEndpoint(): Future[Option[URI]] =
    loggingServiceEndpointPromise.future

  private val loggingServiceEndpointPromise = Promise[Option[URI]]()

  /** Initialize logging to console prior to establishing logging.
    * Some logs may be added while inferring the parameters of logging infrastructure, leading to catch-22 situations.
    */
  def initLogger(): Unit = {
    LogbackSetup.get().setupNoOpAppender()
  }

  /** Starts a logging server that collects logs from different components and immediate sets up logs from this component
    * to send logs to it.
    * @param logLevel
    * @param logMasking
    */
  def setupServerAndForwardLogs(
    logLevel: Option[Level],
    logMasking: Boolean
  ): Unit = {
    val loggerSetup = LogbackSetup.get()
    val config      = loggerSetup.getConfig
    if (config.loggingServerNeedsBoot()) {
      val actualPort     = config.getServer().port();
      val actualLogLevel = logLevel.getOrElse(defaultLogLevel)
      LoggingServiceManager
        .setupServer(
          actualLogLevel,
          actualPort,
          logPath,
          logFileSuffix,
          config.getServer().appender()
        )
        .onComplete {
          case Failure(_) =>
            // fallback to the default appender
            setup(logLevel, None, logMasking, loggerSetup)
          case Success(uri) =>
            Masking.setup(logMasking)
            val result = loggerSetup.setup(actualLogLevel)
            if (!result) {
              LoggingServiceManager.teardown()
              loggingServiceEndpointPromise.failure(
                new LoggerInitializationFailed()
              )
            } else {
              loggingServiceEndpointPromise.success(Some(uri))
            }
        }
    } else {
      // Setup logger according to config
      val actualLogLevel = logLevel.getOrElse(defaultLogLevel)
      if (loggerSetup.setup(actualLogLevel)) {
        loggingServiceEndpointPromise.success(None)
      }
    }
  }

  def setup(
    logLevel: Option[Level],
    connectToExternalLogger: Option[URI],
    logMasking: Boolean
  ): Unit = {
    setup(
      logLevel,
      connectToExternalLogger,
      logMasking,
      LogbackSetup.get()
    );
  }

  def setup(
    logLevel: Option[Level],
    connectToExternalLogger: Option[URI],
    logMasking: Boolean,
    loggerSetup: LoggerSetup
  ): Unit = {
    val actualLogLevel = logLevel.getOrElse(defaultLogLevel)

    connectToExternalLogger match {
      case Some(uri) =>
        var initialized = loggerSetup.setupSocketAppender(
          actualLogLevel,
          uri.getHost(),
          uri.getPort()
        )
        if (!initialized) {
          // Fallback, try the default appender if it is not the socket appender to the same port
          val defaultAppender = loggerSetup.getConfig().getAppender()
          if (!defaultAppender.isSameTargetAs(uri)) {
            initialized = loggerSetup.setup(actualLogLevel)
            if (!initialized) {
              // Fallback to console
              initialized = loggerSetup.setupConsoleAppender(actualLogLevel)
            }
          } else {
            // Fallback to console
            initialized = loggerSetup.setupConsoleAppender(actualLogLevel)
          }
        }

        if (initialized) {
          Masking.setup(logMasking)
          loggingServiceEndpointPromise.success(None)
        } else {
          loggingServiceEndpointPromise.failure(
            new LoggerInitializationFailed()
          )
        }
      case None =>
        if (loggerSetup.setup(actualLogLevel)) {
          Masking.setup(logMasking)
          loggingServiceEndpointPromise.success(None)
        } else {
          loggingServiceEndpointPromise.failure(
            new LoggerInitializationFailed()
          )
        }
    }
  }

  def waitForSetup(): Unit = {
    Await.ready(loggingServiceEndpointPromise.future, 5.seconds)
  }

  def tearDown(): Unit = {
    LoggingServiceManager.teardown()
  }
}
