package org.enso.logging

import org.slf4j.event.Level

import java.io.InputStream
import java.net.URI
import java.nio.file.Path
import scala.annotation.unused
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.Promise

import scala.util.{Failure, Success}
import org.enso.logger.LoggerContextSetup
import org.enso.logger.config.LoggingService

abstract class LoggingCollectorHelper(implicit
  executionContext: ExecutionContext
) {
  val defaultLogLevel: Level

  val logFileSuffix: String

  def logPath: Path

  def logComponentName: String

  def logConfiguration(): Option[InputStream] = {
    val resource =
      this.getClass.getResourceAsStream(s"/${logComponentName}.logback.xml")
    Option.when(resource != null)(resource)
  }

  protected def logLevelPropertyName: String = s"${logComponentName}.logLevel"

  protected def appenderPropertyName: String = s"${logComponentName}.appender"

  def loggingServiceEndpoint(): Future[Option[URI]] = Future.successful(None)

  private val loggingServiceEndpointPromise = Promise[Option[URI]]()

  def setup(
    logLevel: Option[Level],
    connectToExternalLogger: Option[URI],
    @unused logMasking: Boolean,
    @unused profilingLog: Option[Path]
  ): Unit = {
    val actualLogLevel = logLevel.getOrElse(defaultLogLevel)
    val loggingService = LoggingService.parseConfig()
    //TODO: Masking.setup(logMasking)
    val (host, port, serverNeedsInitialization) =
      connectToExternalLogger match {
        case Some(uri) =>
          (uri.getHost(), uri.getPort(), false)
        case None =>
          (
            loggingService.getServer().hostname(),
            loggingService.getServer().port(),
            true
          )
      }

    if (serverNeedsInitialization) {
      LoggingServiceManager
        .setupServer(actualLogLevel, port, logPath, logFileSuffix)
        .onComplete {
          case Failure(exception) =>
            exception.printStackTrace()
            System.setProperty(
              s"${logComponentName}.logRoot",
              logPath.toAbsolutePath.toString
            )
            System.setProperty(
              s"${logComponentName}.logPrefix",
              logFileSuffix + "-standalone"
            )
            System.setProperty(
              logLevelPropertyName,
              actualLogLevel.toString.toLowerCase()
            )
          // fallback to whatever is the default appender
          case Success(uri) =>
            val appender = loggingService.getAppender()
            System.setProperty(appenderPropertyName, appender)
            System.setProperty("logging-server.host", host)
            System.setProperty("logging-server.port", port.toString)
            System.setProperty(
              logLevelPropertyName,
              actualLogLevel.toString.toLowerCase()
            )
            System.setProperty(appenderPropertyName, appender)
            val result = logConfiguration()
              .map(conf =>
                Boolean.unbox(
                  LoggerContextSetup
                    .setupLogging(actualLogLevel, logComponentName, conf)
                )
              )
              .getOrElse(false)
            if (!result) {
              System.err.println("Failed to set Logger Context")
            }
            loggingServiceEndpointPromise.success(Some(uri))
        }
    } else {
      System.setProperty("logging-server.host", host)
      System.setProperty("logging-server.port", port.toString)
      System.setProperty(
        logLevelPropertyName,
        actualLogLevel.toString.toLowerCase()
      )
      System.setProperty(appenderPropertyName, "forward-via-socket")
      val result = logConfiguration()
        .map(conf =>
          Boolean.unbox(
            LoggerContextSetup
              .setupLogging(actualLogLevel, logComponentName, conf)
          )
        )
        .getOrElse(false)
      if (!result) {
        System.err.println("Failed to set Logger Context")
      }
    }
  }
}
