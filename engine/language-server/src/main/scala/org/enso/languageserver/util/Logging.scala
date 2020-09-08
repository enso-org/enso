package org.enso.languageserver.util

import java.util

import cats.syntax.either._
import ch.qos.logback.classic.{Level, LoggerContext}
import org.slf4j.LoggerFactory

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

    /** Convert to java util logging level. */
    def toJava(level: LogLevel): util.logging.Level =
      level match {
        case Error   => util.logging.Level.SEVERE
        case Warning => util.logging.Level.WARNING
        case Info    => util.logging.Level.INFO
        case Debug   => util.logging.Level.FINE
        case Trace   => util.logging.Level.FINEST
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
  }

  private val ROOT_LOGGER = "org.enso"

  /**
    * Set log level for the application root logger.
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

}
