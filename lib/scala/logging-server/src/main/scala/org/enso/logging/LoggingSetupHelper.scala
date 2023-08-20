package org.enso.logging

import org.slf4j.event.Level

import java.net.URI
import java.nio.file.Path
import scala.annotation.unused
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.Promise
import scala.util.{Failure, Success}
import org.enso.logger.LoggerSetup
import org.enso.logger.config.{LoggingServiceConfig, SocketAppender}

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

abstract class LoggingSetupHelper(implicit executionContext: ExecutionContext) {
  val defaultLogLevel: Level

  val logFileSuffix: String

  def logPath: Path

  def loggingServiceEndpoint(): Future[Option[URI]] =
    loggingServiceEndpointPromise.future

  private val loggingServiceEndpointPromise = Promise[Option[URI]]()

  def initLogger(logLevel: Option[Level]): Unit = {
    LoggerSetup.setupConsoleAppender(logLevel.getOrElse(Level.ERROR), null);
  }

  def setupServerAndForwardLogs(
    logLevel: Option[Level],
    logMasking: Boolean,
    profilingLog: Option[Path]
  ): Unit = {
    // Setup server at a given port
    val config = LoggingServiceConfig.parseConfig()
    if (config.loggingServerNeedsBoot()) {
      val actualPort     = config.getServer().port();
      val actualLogLevel = logLevel.getOrElse(defaultLogLevel)
      LoggingServiceManager
        .setupServer(actualLogLevel, actualPort, logPath, logFileSuffix, config.getServer().appender().getName())
        .onComplete {
          // fallback to whatever is the default appender
          case Failure(exception) =>
            exception.printStackTrace()
            setup(logLevel, None, logMasking, profilingLog, config)
          case Success(uri) =>
            val result = LoggerSetup.setup(actualLogLevel, config)

            if (!result) {
              loggingServiceEndpointPromise.failure(
                new LoggerInitializationFailed()
              )
            } else {
              loggingServiceEndpointPromise.success(Some(uri))
            }
        }
    } else {
      // just setup whatever is the default
      val actualLogLevel = logLevel.getOrElse(defaultLogLevel)
      if (LoggerSetup.setup(actualLogLevel, config)) {
        loggingServiceEndpointPromise.success(None)
      }
    }
  }

  def setup(
    logLevel: Option[Level],
    connectToExternalLogger: Option[URI],
    logMasking: Boolean,
    profilingLog: Option[Path]
  ): Unit = {
    setup(
      logLevel,
      connectToExternalLogger,
      logMasking,
      profilingLog,
      LoggingServiceConfig.parseConfig()
    );
  }

  def setup(
    logLevel: Option[Level],
    connectToExternalLogger: Option[URI],
    @unused logMasking: Boolean,
    @unused profilingLog: Option[Path],
    config: LoggingServiceConfig
  ): Unit = {
    val actualLogLevel = logLevel.getOrElse(defaultLogLevel)

    connectToExternalLogger match {
      case Some(uri) =>
        var initialized = LoggerSetup.setupSocketAppender(
          actualLogLevel,
          uri.getHost(),
          uri.getPort(),
          config
        )
        if (!initialized) {
          // Fallback, try the default appender if it is not the socket appender to the same port
          config.getAppender() match {
            case defaultAppender: SocketAppender =>
              if (!defaultAppender.isSameTarget(uri)) {
                initialized = LoggerSetup.setup(actualLogLevel, config)
                if (!initialized) {
                  // Fallback to console
                  initialized =
                    LoggerSetup.setupConsoleAppender(actualLogLevel, config);
                }
              } else {
                // Fallback to console
                initialized =
                  LoggerSetup.setupConsoleAppender(actualLogLevel, config);
              }
            case _ =>
              initialized = LoggerSetup.setup(actualLogLevel, config)
          }
        }
        if (initialized) {
          loggingServiceEndpointPromise.success(None)
        } else {
          loggingServiceEndpointPromise.failure(new LoggerInitializationFailed())
        }
      case None =>
        if (LoggerSetup.setup(actualLogLevel, config)) {
          loggingServiceEndpointPromise.success(None)
        } else {
          loggingServiceEndpointPromise.failure(new LoggerInitializationFailed())
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
