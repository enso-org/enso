package org.enso.logging

import java.net.URI
import scala.util.{Success, Try}
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
    logFileSuffix: String
  )(implicit ec: ExecutionContext): Future[URI] = {
    if (loggingService != null) {
      throw new RuntimeException("logging service already setup")
    } else {
      currentLevel = logLevel
      val forwarder = new ForwardToServer(port)
      loggingService = forwarder
      Future {
        forwarder.logToFile(logLevel, logPath, logFileSuffix)
      }
    }
  }

  def fallbackToLocalConsole(logLevel: Level): Try[Unit] = {
    if (loggingService != null) {
      loggingService.teardown()
    }
    System.setProperty("logging-server.logLevel", logLevel.toString.toLowerCase)
    System.setProperty("logging-server.appender", "console")

    // TODO: start console appender
    Success(())
  }

  def teardown(): Unit = {
    if (loggingService != null) {
      loggingService.teardown()
    }
  }

}
