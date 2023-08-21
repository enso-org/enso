package org.enso.logging

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
      throw new LoggingServiceAlreadySetup()
    } else {
      currentLevel = logLevel
      val forwarder = new LoggingServer(port)
      loggingService = forwarder
      Future {
        forwarder.start(logLevel, logPath, logFileSuffix, appenderName)
      }
    }
  }

  def teardown(): Unit = {
    if (loggingService != null) {
      loggingService.teardown()
    }
  }

}
