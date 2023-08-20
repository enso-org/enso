package org.enso.logging

import org.enso.logger.LoggerSetup

import java.net.URI
import org.slf4j.event.Level

import java.nio.file.Path
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

object LoggingServiceManager {

  private[this] var loggingService: LoggingService = null
  private[this] var currentLevel: Level            = Level.TRACE

  def currentLogLevelForThisApplication(): Level = currentLevel

  def setupServer(
    logLevel: Level,
    port: Int,
    logPath: Path,
    logFileSuffix: String,
    appenderName: String
  )(implicit ec: ExecutionContext): Future[URI] = {
    if (loggingService != null) {
      throw new RuntimeException("logging service already setup")
    } else {
      currentLevel = logLevel
      val forwarder = new ForwardToServer(port)
      loggingService = forwarder
      Future {
        forwarder.logToFile(logLevel, logPath, logFileSuffix, appenderName)
      }
    }
  }

  def fallbackToLocalConsole(logLevel: Level): Unit = {
    if (loggingService != null) {
      loggingService.teardown()
    }
    LoggerSetup.setup(logLevel)
    loggingService = null;
  }

  def teardown(): Unit = {
    if (loggingService != null) {
      loggingService.teardown()
    }
  }

}
