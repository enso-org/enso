package org.enso.loggingservice.internal

import org.enso.loggingservice.LogLevel

trait LoggerConnection {
  def send(message: InternalLogMessage): Unit
  def logLevel:                          LogLevel

  def isEnabled(level: LogLevel): Boolean =
    implicitly[Ordering[LogLevel]].lteq(level, logLevel)
}
