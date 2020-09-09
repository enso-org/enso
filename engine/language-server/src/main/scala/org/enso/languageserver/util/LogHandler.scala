package org.enso.languageserver.util

import java.util.logging.{Handler, Level, LogRecord}

import ch.qos.logback.classic
import org.slf4j.event

class LogHandler(logger: classic.Logger) extends Handler {

  private val level = logger.getLevel

  /** @inheritdoc */
  override def publish(record: LogRecord): Unit = {
    logger.log(
      null,
      record.getSourceClassName,
      LogHandler.toSlf4j(record.getLevel).toInt,
      record.getMessage,
      record.getParameters,
      record.getThrown
    )
  }

  /** @inheritdoc */
  override def flush(): Unit = ()

  /** @inheritdoc */
  override def close(): Unit = ()

  /** @inheritdoc */
  override def getLevel: Level =
    Logging.LogLevel.toJava(Logging.LogLevel.fromLogback(level))

}

object LogHandler {

  /** Convert java utils log level to slf4j. */
  private def toSlf4j(level: Level): event.Level =
    Logging.LogLevel.toSlf4j(Logging.LogLevel.fromJava(level))
}
