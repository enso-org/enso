package org.enso.loggingservice.internal

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, ZoneId}

import org.enso.loggingservice.LogLevel
import org.enso.loggingservice.internal.protocol.{
  SerializedException,
  WSLogMessage
}

class DefaultLogMessageRenderer(printStackTraces: Boolean)
    extends LogMessageRenderer {
  override def render(logMessage: WSLogMessage): String = {
    val level     = renderLevel(logMessage.logLevel)
    val timestamp = renderTimestamp(logMessage.timestamp)
    val base =
      s"[$level] [$timestamp] [${logMessage.group}] ${logMessage.message}"
    addStackTrace(base, logMessage.exception)
  }

  private val timestampZone = ZoneId.of("UTC")

  def renderTimestamp(timestamp: Instant): String =
    LocalDateTime
      .ofInstant(timestamp, timestampZone)
      .format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)

  /**
    * Adds attached exception's stack trace (if available) to the log if
    * printing stack traces is enabled.
    */
  def addStackTrace(
    message: String,
    exception: Option[SerializedException]
  ): String =
    exception match {
      case Some(e) if printStackTraces =>
        message + "\n" + renderException(e)
      case _ => message
    }

  def renderException(exception: SerializedException): String = {
    val head = s"${exception.name}: ${exception.message}"
    val trace = exception.stackTrace.map(elem =>
      s"  at ${elem.element}(${elem.location})"
    )
    val cause = exception.cause
      .map(e => s"\nCaused by: ${renderException(e)}")
      .getOrElse("")
    head + trace.map("\n" + _).mkString + cause
  }

  def renderLevel(logLevel: LogLevel): String =
    logLevel match {
      case LogLevel.Error   => "error"
      case LogLevel.Warning => "warn"
      case LogLevel.Info    => "info"
      case LogLevel.Debug   => "debug"
      case LogLevel.Trace   => "trace"
      case LogLevel.Off     => "off"
    }
}
