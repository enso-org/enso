package org.enso.loggingservice

import java.util.logging.{Handler, Level, LogRecord}

import org.enso.loggingservice.internal.{InternalLogMessage, LoggerConnection}

/**
  * A [[Handler]] implementation that allows to use the logging service as a
  * backend for [[java.util.logging]].
  */
class JavaLoggingLogHandler(
  levelMapping: Level => LogLevel,
  connection: LoggerConnection
) extends Handler {

  /**
    * @inheritdoc
    */
  override def publish(record: LogRecord): Unit = {
    val level = levelMapping(record.getLevel)
    if (connection.isEnabled(level)) {
      val message = InternalLogMessage(
        level     = level,
        timestamp = record.getInstant,
        group     = record.getLoggerName,
        message   = record.getMessage,
        exception = Option(record.getThrown)
      )
      connection.send(message)
    }
  }

  /**
    * @inheritdoc
    */
  override def flush(): Unit = {}

  /**
    * @inheritdoc
    */
  override def close(): Unit = {}
}

object JavaLoggingLogHandler {

  /**
    * Creates a [[Handler]] with the provided mapping from Java's log levels to
    * our log levels.
    */
  def create(mapping: Level => LogLevel): JavaLoggingLogHandler =
    new JavaLoggingLogHandler(mapping, LoggingServiceManager.Connection)

  /**
    * Determines what is the smallest Java level that is still debug and not
    * trace.
    */
  private val defaultLevelDebugCutOff =
    Seq(Level.FINE.intValue, Level.CONFIG.intValue).min

  /**
    * Default mapping of Java log levels to our log levels based
    */
  def defaultLevelMapping(javaLevel: Level): LogLevel = {
    val level = javaLevel.intValue
    if (level == Level.OFF.intValue) LogLevel.Off
    else if (level >= Level.SEVERE.intValue) LogLevel.Error
    else if (level >= Level.WARNING.intValue) LogLevel.Warning
    else if (level >= Level.INFO.intValue) LogLevel.Info
    else if (level >= defaultLevelDebugCutOff) LogLevel.Debug
    else LogLevel.Trace
  }

  /**
    * Approximate-inverse of [[defaultLevelMapping]] that returns a Java log
    * level corresponding to the given log level.
    */
  def getJavaLogLevelFor(logLevel: LogLevel): Level =
    logLevel match {
      case LogLevel.Off     => Level.OFF
      case LogLevel.Error   => Level.SEVERE
      case LogLevel.Warning => Level.WARNING
      case LogLevel.Info    => Level.INFO
      case LogLevel.Debug   => Level.FINE
      case LogLevel.Trace   => Level.ALL
    }
}
