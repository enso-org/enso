package org.enso.languageserver.util

import java.util

import cats.syntax.either._
import ch.qos.logback.classic.{Level, Logger, LoggerContext}
import org.slf4j.{event, LoggerFactory}

object Logging {

  /** Application log level. */
  sealed trait LogLevel
  object LogLevel {

    case object Error   extends LogLevel
    case object Warning extends LogLevel
    case object Info    extends LogLevel
    case object Debug   extends LogLevel
    case object Trace   extends LogLevel

    /** Convert to logback log level. */
    def toLogback(level: LogLevel): Level =
      level match {
        case Error   => Level.ERROR
        case Warning => Level.WARN
        case Info    => Level.INFO
        case Debug   => Level.DEBUG
        case Trace   => Level.TRACE
      }

    /** Convert from logback log level. */
    def fromLogback(level: Level): LogLevel = {
      level match {
        case Level.`ERROR` => Error
        case Level.`WARN`  => Warning
        case Level.`INFO`  => Info
        case Level.`DEBUG` => Debug
        case Level.`TRACE` => Trace
      }
    }

    /** Convert to java util logging level. */
    def toJava(level: LogLevel): util.logging.Level =
      level match {
        case Error   => util.logging.Level.SEVERE
        case Warning => util.logging.Level.WARNING
        case Info    => util.logging.Level.INFO
        case Debug   => util.logging.Level.FINE
        case Trace   => util.logging.Level.FINEST
      }

    /** Convert from java util logging level. */
    def fromJava(level: util.logging.Level): LogLevel =
      level match {
        case util.logging.Level.`SEVERE`  => LogLevel.Error
        case util.logging.Level.`WARNING` => LogLevel.Warning
        case util.logging.Level.`INFO`    => LogLevel.Info
        case util.logging.Level.`CONFIG`  => LogLevel.Debug
        case util.logging.Level.`FINE`    => LogLevel.Debug
        case util.logging.Level.`FINER`   => LogLevel.Debug
        case util.logging.Level.`FINEST`  => LogLevel.Trace
      }

    /** Convert to slf4j logging level. */
    def toSlf4j(level: LogLevel): event.Level =
      level match {
        case Error   => event.Level.ERROR
        case Warning => event.Level.WARN
        case Info    => event.Level.INFO
        case Debug   => event.Level.DEBUG
        case Trace   => event.Level.TRACE
      }

    /** Convert to akka logging level. */
    def toAkka(level: LogLevel): akka.event.Logging.LogLevel =
      level match {
        case Error   => akka.event.Logging.ErrorLevel
        case Warning => akka.event.Logging.WarningLevel
        case Info    => akka.event.Logging.InfoLevel
        case Debug   => akka.event.Logging.DebugLevel
        case Trace   => akka.event.Logging.DebugLevel
      }
  }

  private val ROOT_LOGGER = "org.enso"

  /** Set log level for the application root logger.
    *
    * @param level the log level
    * @return the new log level
    */
  def setLogLevel(level: LogLevel): Either[Throwable, LogLevel] = {
    Either.catchNonFatal {
      val ctx = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
      ctx.getLogger(ROOT_LOGGER).setLevel(LogLevel.toLogback(level))
      level
    }
  }

  /** Get log level of the application root logger. */
  def getLogLevel: Either[Throwable, LogLevel] = {
    Either.catchNonFatal {
      val ctx   = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
      val level = ctx.getLogger(ROOT_LOGGER).getLevel
      LogLevel.fromLogback(level)
    }
  }

  /** Get the application logger.
    *
    * @param name the logger name
    * @return the application logger
    */
  def getLogger(name: String): Either[Throwable, Logger] = {
    Either.catchNonFatal {
      val ctx = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
      ctx.getLogger(name)
    }
  }

  /** Get the java log handler instance backed by the application logger.
    *
    * @param name the logger name
    * @return the application log handler
    */
  def getLogHandler(name: String): Either[Throwable, LogHandler] =
    for {
      level  <- Logging.getLogLevel
      logger <- Logging.getLogger(name)
    } yield {
      logger.setLevel(Logging.LogLevel.toLogback(level))
      new LogHandler(logger)
    }
}
