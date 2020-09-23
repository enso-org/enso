package org.enso.loggingservice.internal

trait LoggerConnection {
  def send(message: InternalLogMessage): Unit
  def logLevel:                          Level

  def isEnabled(level: Level): Boolean =
    implicitly[Ordering[Level]].lteq(level, logLevel)
}
