package org.enso.loggingservice.internal

import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneId}

import org.enso.loggingservice.internal.protocol.{
  SerializedException,
  WSLogMessage
}

object DefaultLogMessageRenderer extends LogMessageRenderer {
  // TODO [RW] colors
  override def render(logMessage: WSLogMessage): String = {
    val level = logMessage.logLevel.toString
    val timestamp = LocalDateTime
      .ofInstant(logMessage.timestamp, timestampZone)
      .format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
    val base =
      s"[$level] [$timestamp] [${logMessage.group}] ${logMessage.message}"
    logMessage.exception match {
      case Some(exception) =>
        base + "\n" + renderException(exception)
      case None => base
    }
  }

  private val timestampZone = ZoneId.of("UTC")

  private def renderException(exception: SerializedException): String = {
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
