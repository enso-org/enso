package org.enso.loggingservice.internal

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, ZoneId}

import org.enso.loggingservice.internal.protocol.{
  SerializedException,
  WSLogMessage
}

class DefaultLogMessageRenderer extends LogMessageRenderer {
  override def render(logMessage: WSLogMessage): String = {
    val level     = logMessage.logLevel.toString
    val timestamp = renderTimestamp(logMessage.timestamp)
    val base =
      s"[$level] [$timestamp] [${logMessage.group}] ${logMessage.message}"
    logMessage.exception match {
      case Some(exception) =>
        base + "\n" + renderException(exception)
      case None => base
    }
  }

  private val timestampZone = ZoneId.of("UTC")

  def renderTimestamp(timestamp: Instant): String =
    LocalDateTime
      .ofInstant(timestamp, timestampZone)
      .format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)

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
}
