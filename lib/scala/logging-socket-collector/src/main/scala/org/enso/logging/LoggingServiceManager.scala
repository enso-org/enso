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

  def setupConnection(logLevel: Level, uri: URI)(implicit
    ec: ExecutionContext
  ): Future[Unit] = {
    if (loggingService != null) {
      throw new RuntimeException("logging service already setup")
    } else {
      currentLevel = logLevel
      val client = new ExternalLogger(uri)
      loggingService = client
      Future {
        client.connect()
      }
    }
  }

  def fallbackToLocalConsole(logLevel: Level, componentName: String): Unit = {
    if (loggingService != null) {
      loggingService.teardown()
    }
    val local = new Local(componentName)
    loggingService = local
    local.start(logLevel)
  }

  def teardown(): Unit = {
    if (loggingService != null) {
      loggingService.teardown()
    }
  }

}
