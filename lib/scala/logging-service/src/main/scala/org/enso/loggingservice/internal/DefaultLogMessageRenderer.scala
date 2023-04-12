package org.enso.loggingservice.internal

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset, ZonedDateTime}

import org.enso.loggingservice.LogLevel
import org.enso.loggingservice.internal.protocol.{
  SerializedException,
  WSLogMessage
}

/** Renders the log message using the default format, including attached
  * exceptions if [[printExceptions]] is set.
  */
class DefaultLogMessageRenderer(printExceptions: Boolean)
    extends LogMessageRenderer {

  /** @inheritdoc
    */
  override def render(logMessage: WSLogMessage): String = {
    val level     = renderLevel(logMessage.level)
    val timestamp = renderTimestamp(logMessage.timestamp)
    val base =
      s"[$level] [$timestamp] [${logMessage.group}] ${logMessage.message}"
    addException(base, logMessage.exception)
  }

  private val timestampZone = ZoneOffset.UTC

  /** Renders the timestamp.
    */
  def renderTimestamp(timestamp: Instant): String =
    ZonedDateTime
      .ofInstant(timestamp, timestampZone)
      .format(DateTimeFormatter.ISO_ZONED_DATE_TIME)

  /** Adds attached exception's stack trace (if available) to the log if
    * printing stack traces is enabled.
    */
  def addException(
    message: String,
    exception: Option[SerializedException]
  ): String =
    exception match {
      case Some(e) if printExceptions =>
        message + "\n" + renderException(e)
      case _ => message
    }

  /** Renders an exception with its strack trace.
    */
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

  /** Renders a log level.
    */
  def renderLevel(logLevel: LogLevel): String =
    logLevel match {
      case LogLevel.Error   => "error"
      case LogLevel.Warning => "warn"
      case LogLevel.Info    => "info"
      case LogLevel.Debug   => "debug"
      case LogLevel.Trace   => "trace"
      case LogLevel.Off     => "off"
      case _                => "error";
    }
}
