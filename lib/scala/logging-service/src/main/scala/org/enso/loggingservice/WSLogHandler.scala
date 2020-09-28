package org.enso.loggingservice

import java.util.logging.{Handler, Level, LogRecord}

import org.enso.loggingservice.internal.{InternalLogMessage, LoggerConnection}

/**
  * A [[Handler]] implementation that allows to use the logging service as a
  * backend for [[java.util.logging]].
  */
class WSLogHandler(
  levelMapping: Level => LogLevel,
  connection: LoggerConnection
) extends Handler {
  override def publish(record: LogRecord): Unit = {
    val level = levelMapping(record.getLevel)
    if (connection.isEnabled(level)) {
      val message = InternalLogMessage(
        level     = level,
        timestamp = record.getInstant,
        group     = record.getLoggerName,
        message   = record.getMessage,
        throwable = Option(record.getThrown)
      )
      connection.send(message)
    }
  }

  override def flush(): Unit = {}

  override def close(): Unit = {}
}

object WSLogHandler {
  def create(mapping: Level => LogLevel): WSLogHandler =
    new WSLogHandler(mapping, WSLoggerManager.Connection)

  private val truffleLevelDebugCutOff =
    Seq(Level.FINE.intValue, Level.CONFIG.intValue).min
  def defaultTruffleLevelMapping(truffleLevel: Level): LogLevel = {
    val level = truffleLevel.intValue
    if (level == Level.OFF.intValue) LogLevel.Off
    else if (level >= Level.SEVERE.intValue) LogLevel.Error
    else if (level >= Level.WARNING.intValue) LogLevel.Warning
    else if (level >= Level.INFO.intValue) LogLevel.Info
    else if (level >= truffleLevelDebugCutOff) LogLevel.Debug
    else LogLevel.Trace
  }

  def getTruffleLogLevelFor(logLevel: LogLevel): Level =
    logLevel match {
      case LogLevel.Off     => Level.OFF
      case LogLevel.Error   => Level.SEVERE
      case LogLevel.Warning => Level.WARNING
      case LogLevel.Info    => Level.INFO
      case LogLevel.Debug   => Level.FINE
      case LogLevel.Trace   => Level.ALL
    }
}
