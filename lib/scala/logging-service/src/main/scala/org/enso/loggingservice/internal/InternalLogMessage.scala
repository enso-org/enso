package org.enso.loggingservice.internal

import java.time.Instant

import org.enso.loggingservice.LogLevel

case class InternalLogMessage(
  level: LogLevel,
  timestamp: Instant,
  group: String,
  message: String,
  throwable: Option[Throwable]
) {

  def toLogMessage(): WSLogMessage =
    WSLogMessage(
      logLevel  = level,
      timestamp = timestamp,
      group     = group,
      message   = message,
      exception = throwable.map(SerializedException.fromException)
    )
}

object InternalLogMessage {
  def apply(
    level: LogLevel,
    group: String,
    message: String,
    throwable: Option[Throwable]
  ): InternalLogMessage =
    InternalLogMessage(
      level     = level,
      timestamp = Instant.now(),
      group     = group,
      message   = message,
      throwable = throwable
    )
}
