package org.enso.loggingservice.internal

import java.time.Instant
import java.time.temporal.ChronoUnit

import org.enso.loggingservice.LogLevel
import org.enso.loggingservice.internal.protocol.{
  SerializedException,
  WSLogMessage
}

/**
  * The internal log message that is used for local logging.
  *
  * @param level log level
  * @param timestamp timestamp indicating when the message was created
  * @param group group associated with the message
  * @param message text message
  * @param exception optional attached exception
  */
case class InternalLogMessage(
  level: LogLevel,
  timestamp: Instant,
  group: String,
  message: String,
  exception: Option[Throwable]
) extends BaseLogMessage[Throwable] {

  /**
    * Converts to [[WSLogMessage]] by serializing the attached exception.
    */
  def toLogMessage: WSLogMessage =
    WSLogMessage(
      level     = level,
      timestamp = timestamp.truncatedTo(ChronoUnit.MILLIS),
      group     = group,
      message   = message,
      exception = exception.map(SerializedException.fromException)
    )
}

object InternalLogMessage {

  /**
    * Creates a log message with the timestamp set to the current instant.
    */
  def apply(
    level: LogLevel,
    group: String,
    message: String,
    exception: Option[Throwable]
  ): InternalLogMessage =
    InternalLogMessage(
      level     = level,
      timestamp = Instant.now(),
      group     = group,
      message   = message,
      exception = exception
    )
}
