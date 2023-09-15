package org.enso.logger

import org.slf4j.event.Level
import ch.qos.logback.classic.{Level => LogbackLevel}

case class TestLogMessage(level: Level, msg: String)
object TestLogMessage {
  def apply(level: LogbackLevel, msg: String): TestLogMessage = {
    val translatedLevel = level match {
      case LogbackLevel.INFO  => Level.INFO
      case LogbackLevel.DEBUG => Level.DEBUG
      case LogbackLevel.WARN  => Level.WARN
      case LogbackLevel.TRACE => Level.TRACE
    }
    TestLogMessage(translatedLevel, msg)
  }
}
