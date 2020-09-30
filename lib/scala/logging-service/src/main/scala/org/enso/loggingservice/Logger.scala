package org.enso.loggingservice

import org.enso.loggingservice.internal.{InternalLogMessage, LoggerConnection}
import org.slf4j.helpers.MessageFormatter
import org.slf4j.{Logger => SLF4JLogger, Marker}

import scala.annotation.unused

/**
  * A [[SLF4JLogger]] instance for the SLF4J backend which passes all log
  * messages to a [[LoggerConnection]].
  *
  * @param name name of the logger
  * @param connection the connection to pass the log messages to
  */
class Logger(name: String, connection: LoggerConnection) extends SLF4JLogger {
  override def getName: String = name

  private def isEnabled(level: LogLevel): Boolean =
    connection.isEnabled(level)

  private def log(
    level: LogLevel,
    msg: String
  ): Unit = {
    if (isEnabled(level)) {
      connection.send(InternalLogMessage(level, name, msg, None))
    }
  }

  private def log(
    level: LogLevel,
    format: String,
    arg: AnyRef
  ): Unit = {
    if (isEnabled(level)) {
      val fp = MessageFormatter.format(format, arg)
      connection.send(
        InternalLogMessage(level, name, fp.getMessage, Option(fp.getThrowable))
      )
    }
  }

  private def log(
    level: LogLevel,
    format: String,
    arg1: AnyRef,
    arg2: AnyRef
  ): Unit = {
    if (isEnabled(level)) {
      val fp = MessageFormatter.format(format, arg1, arg2)
      connection.send(
        InternalLogMessage(level, name, fp.getMessage, Option(fp.getThrowable))
      )
    }
  }

  private def log(
    level: LogLevel,
    format: String,
    args: Seq[AnyRef]
  ): Unit = {
    if (isEnabled(level)) {
      val fp = MessageFormatter.arrayFormat(format, args.toArray)
      connection.send(
        InternalLogMessage(level, name, fp.getMessage, Option(fp.getThrowable))
      )
    }
  }

  private def log(
    level: LogLevel,
    msg: String,
    throwable: Throwable
  ): Unit = {
    if (isEnabled(level)) {
      connection.send(
        InternalLogMessage(level, name, msg, Some(throwable))
      )
    }
  }

  override def isTraceEnabled: Boolean = isEnabled(LogLevel.Trace)

  override def trace(msg: String): Unit = log(LogLevel.Trace, msg)

  override def trace(format: String, arg: AnyRef): Unit =
    log(LogLevel.Trace, format, arg)

  override def trace(format: String, arg1: AnyRef, arg2: AnyRef): Unit =
    log(LogLevel.Trace, format, arg1, arg2)

  override def trace(format: String, arguments: AnyRef*): Unit =
    log(LogLevel.Trace, format, arguments)

  override def trace(msg: String, t: Throwable): Unit =
    log(LogLevel.Trace, msg, t)

  override def isTraceEnabled(@unused marker: Marker): Boolean =
    isEnabled(LogLevel.Trace)

  override def trace(@unused marker: Marker, msg: String): Unit =
    log(LogLevel.Trace, msg)

  override def trace(
    @unused marker: Marker,
    format: String,
    arg: AnyRef
  ): Unit =
    log(LogLevel.Trace, format, arg)

  override def trace(
    @unused marker: Marker,
    format: String,
    arg1: AnyRef,
    arg2: AnyRef
  ): Unit = log(LogLevel.Trace, format, arg1, arg2)

  override def trace(
    @unused marker: Marker,
    format: String,
    argArray: AnyRef*
  ): Unit =
    log(LogLevel.Trace, format, argArray)

  override def trace(@unused marker: Marker, msg: String, t: Throwable): Unit =
    log(LogLevel.Trace, msg, t)

  override def isDebugEnabled: Boolean = isEnabled(LogLevel.Debug)

  override def debug(msg: String): Unit = log(LogLevel.Debug, msg)

  override def debug(format: String, arg: AnyRef): Unit =
    log(LogLevel.Debug, format, arg)

  override def debug(format: String, arg1: AnyRef, arg2: AnyRef): Unit =
    log(LogLevel.Debug, format, arg1, arg2)

  override def debug(format: String, arguments: AnyRef*): Unit =
    log(LogLevel.Debug, format, arguments)

  override def debug(msg: String, t: Throwable): Unit =
    log(LogLevel.Debug, msg, t)

  override def isDebugEnabled(@unused marker: Marker): Boolean =
    isEnabled(LogLevel.Debug)

  override def debug(@unused marker: Marker, msg: String): Unit =
    log(LogLevel.Debug, msg)

  override def debug(
    @unused marker: Marker,
    format: String,
    arg: AnyRef
  ): Unit =
    log(LogLevel.Debug, format, arg)

  override def debug(
    @unused marker: Marker,
    format: String,
    arg1: AnyRef,
    arg2: AnyRef
  ): Unit = log(LogLevel.Debug, format, arg1, arg2)

  override def debug(
    @unused marker: Marker,
    format: String,
    arguments: AnyRef*
  ): Unit =
    log(LogLevel.Debug, format, arguments)

  override def debug(@unused marker: Marker, msg: String, t: Throwable): Unit =
    log(LogLevel.Debug, msg, t)

  override def isInfoEnabled: Boolean = isEnabled(LogLevel.Info)

  override def info(msg: String): Unit = log(LogLevel.Info, msg)

  override def info(format: String, arg: AnyRef): Unit =
    log(LogLevel.Info, format, arg)

  override def info(format: String, arg1: AnyRef, arg2: AnyRef): Unit =
    log(LogLevel.Info, format, arg1, arg2)

  override def info(format: String, arguments: AnyRef*): Unit =
    log(LogLevel.Info, format, arguments)

  override def info(msg: String, t: Throwable): Unit =
    log(LogLevel.Info, msg, t)

  override def isInfoEnabled(@unused marker: Marker): Boolean =
    isEnabled(LogLevel.Info)

  override def info(@unused marker: Marker, msg: String): Unit =
    log(LogLevel.Info, msg)

  override def info(@unused marker: Marker, format: String, arg: AnyRef): Unit =
    log(LogLevel.Info, format, arg)

  override def info(
    @unused marker: Marker,
    format: String,
    arg1: AnyRef,
    arg2: AnyRef
  ): Unit = log(LogLevel.Info, format, arg1, arg2)

  override def info(
    @unused marker: Marker,
    format: String,
    arguments: AnyRef*
  ): Unit =
    log(LogLevel.Info, format, arguments)

  override def info(@unused marker: Marker, msg: String, t: Throwable): Unit =
    log(LogLevel.Info, msg, t)

  override def isWarnEnabled: Boolean = isEnabled(LogLevel.Warning)

  override def warn(msg: String): Unit = log(LogLevel.Warning, msg)

  override def warn(format: String, arg: AnyRef): Unit =
    log(LogLevel.Warning, format, arg)

  override def warn(format: String, arguments: AnyRef*): Unit =
    log(LogLevel.Warning, format, arguments)

  override def warn(format: String, arg1: AnyRef, arg2: AnyRef): Unit =
    log(LogLevel.Warning, format, arg1, arg2)

  override def warn(msg: String, t: Throwable): Unit =
    log(LogLevel.Warning, msg, t)

  override def isWarnEnabled(@unused marker: Marker): Boolean =
    isEnabled(LogLevel.Warning)

  override def warn(@unused marker: Marker, msg: String): Unit =
    log(LogLevel.Warning, msg)

  override def warn(@unused marker: Marker, format: String, arg: AnyRef): Unit =
    log(LogLevel.Warning, format, arg)

  override def warn(
    @unused marker: Marker,
    format: String,
    arg1: AnyRef,
    arg2: AnyRef
  ): Unit = log(LogLevel.Warning, format, arg1, arg2)

  override def warn(
    @unused marker: Marker,
    format: String,
    arguments: AnyRef*
  ): Unit =
    log(LogLevel.Warning, format, arguments)

  override def warn(@unused marker: Marker, msg: String, t: Throwable): Unit =
    log(LogLevel.Warning, msg, t)

  override def isErrorEnabled: Boolean = isEnabled(LogLevel.Error)

  override def error(msg: String): Unit = log(LogLevel.Error, msg)

  override def error(format: String, arg: AnyRef): Unit =
    log(LogLevel.Error, format, arg)

  override def error(format: String, arg1: AnyRef, arg2: AnyRef): Unit =
    log(LogLevel.Error, format, arg1, arg2)

  override def error(format: String, arguments: AnyRef*): Unit =
    log(LogLevel.Error, format, arguments)

  override def error(msg: String, t: Throwable): Unit =
    log(LogLevel.Error, msg, t)

  override def isErrorEnabled(@unused marker: Marker): Boolean =
    isEnabled(LogLevel.Error)

  override def error(@unused marker: Marker, msg: String): Unit =
    log(LogLevel.Error, msg)

  override def error(
    @unused marker: Marker,
    format: String,
    arg: AnyRef
  ): Unit =
    log(LogLevel.Error, format, arg)

  override def error(
    @unused marker: Marker,
    format: String,
    arg1: AnyRef,
    arg2: AnyRef
  ): Unit = log(LogLevel.Error, format, arg1, arg2)

  override def error(
    @unused marker: Marker,
    format: String,
    arguments: AnyRef*
  ): Unit =
    log(LogLevel.Error, format, arguments)

  override def error(@unused marker: Marker, msg: String, t: Throwable): Unit =
    log(LogLevel.Error, msg, t)
}
