package org.enso.loggingservice.internal

case class InternalLogMessage(
  level: Level,
  group: String,
  message: String,
  throwable: Option[Throwable]
)
