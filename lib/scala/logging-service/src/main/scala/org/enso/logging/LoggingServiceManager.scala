package org.enso.logging

import org.enso.logger.config.Appender

import java.net.URI
import org.slf4j.event.Level

import java.nio.file.Path
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

object LoggingServiceManager {

  private[this] var loggingService: LoggingService[_] = null
  private[this] var currentLevel: Level               = Level.TRACE

  def currentLogLevelForThisApplication(): Level = currentLevel

  def setupServer(
    logLevel: Level,
    port: Int,
    logPath: Path,
    logFileSuffix: String,
    appender: Appender
  )(implicit ec: ExecutionContext): Future[URI] = {
    if (loggingService != null) {
      throw new LoggingServiceAlreadySetup()
    } else {
      currentLevel = logLevel
      val server = LoggingServiceFactory.get().localServerFor(port);;
      loggingService = server
      Future {
        server.start(logLevel, logPath, logFileSuffix, appender)
      }
    }
  }

  def teardown(): Unit = {
    if (loggingService != null) {
      loggingService.teardown()
    }
  }

}
