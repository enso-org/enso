package org.enso.loggingservice.internal

case class InternalLogMessage(
  level: Level,
  group: String,
  message: String,
  throwable: Option[Throwable]
) {
  def toLogMessage(): WSLogMessage =
    WSLogMessage(
      logLevel  = level,
      group     = group,
      message   = message,
      exception = throwable.map(SerializedException.fromException)
    )
}
