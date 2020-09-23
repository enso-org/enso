package org.enso.loggingservice

import org.enso.loggingservice.internal.{InternalLogMessage, Level}
import org.slf4j.helpers.MessageFormatter
import org.slf4j.{Logger, Marker}

import scala.annotation.unused

class WSLogger(name: String, connection: internal.LoggerConnection)
    extends Logger {
  override def getName: String = name

  private def isEnabled(level: Level): Boolean =
    connection.isEnabled(level)

  private def log(
    level: Level,
    msg: String
  ): Unit = {
    if (isEnabled(level)) {
      connection.send(InternalLogMessage(level, name, msg, None))
    }
  }

  private def log(
    level: Level,
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
    level: Level,
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
    level: Level,
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
    level: Level,
    msg: String,
    throwable: Throwable
  ): Unit = {
    if (isEnabled(level)) {
      connection.send(
        InternalLogMessage(level, name, msg, Some(throwable))
      )
    }
  }

  override def isTraceEnabled: Boolean = isEnabled(Level.Trace)

  override def trace(msg: String): Unit = log(Level.Trace, msg)

  override def trace(format: String, arg: AnyRef): Unit =
    log(Level.Trace, format, arg)

  override def trace(format: String, arg1: AnyRef, arg2: AnyRef): Unit =
    log(Level.Trace, format, arg1, arg2)

  override def trace(format: String, arguments: AnyRef*): Unit =
    log(Level.Trace, format, arguments)

  override def trace(msg: String, t: Throwable): Unit = log(Level.Trace, msg, t)

  override def isTraceEnabled(@unused marker: Marker): Boolean =
    isEnabled(Level.Trace)

  override def trace(@unused marker: Marker, msg: String): Unit =
    log(Level.Trace, msg)

  override def trace(
    @unused marker: Marker,
    format: String,
    arg: AnyRef
  ): Unit =
    log(Level.Trace, format, arg)

  override def trace(
    @unused marker: Marker,
    format: String,
    arg1: AnyRef,
    arg2: AnyRef
  ): Unit = log(Level.Trace, format, arg1, arg2)

  override def trace(
    @unused marker: Marker,
    format: String,
    argArray: AnyRef*
  ): Unit =
    log(Level.Trace, format, argArray)

  override def trace(@unused marker: Marker, msg: String, t: Throwable): Unit =
    log(Level.Trace, msg, t)

  override def isDebugEnabled: Boolean = isEnabled(Level.Debug)

  override def debug(msg: String): Unit = log(Level.Debug, msg)

  override def debug(format: String, arg: AnyRef): Unit =
    log(Level.Debug, format, arg)

  override def debug(format: String, arg1: AnyRef, arg2: AnyRef): Unit =
    log(Level.Debug, format, arg1, arg2)

  override def debug(format: String, arguments: AnyRef*): Unit =
    log(Level.Debug, format, arguments)

  override def debug(msg: String, t: Throwable): Unit = log(Level.Debug, msg, t)

  override def isDebugEnabled(@unused marker: Marker): Boolean =
    isEnabled(Level.Debug)

  override def debug(@unused marker: Marker, msg: String): Unit =
    log(Level.Debug, msg)

  override def debug(
    @unused marker: Marker,
    format: String,
    arg: AnyRef
  ): Unit =
    log(Level.Debug, format, arg)

  override def debug(
    @unused marker: Marker,
    format: String,
    arg1: AnyRef,
    arg2: AnyRef
  ): Unit = log(Level.Debug, format, arg1, arg2)

  override def debug(
    @unused marker: Marker,
    format: String,
    arguments: AnyRef*
  ): Unit =
    log(Level.Debug, format, arguments)

  override def debug(@unused marker: Marker, msg: String, t: Throwable): Unit =
    log(Level.Debug, msg, t)

  override def isInfoEnabled: Boolean = isEnabled(Level.Info)

  override def info(msg: String): Unit = log(Level.Info, msg)

  override def info(format: String, arg: AnyRef): Unit =
    log(Level.Info, format, arg)

  override def info(format: String, arg1: AnyRef, arg2: AnyRef): Unit =
    log(Level.Info, format, arg1, arg2)

  override def info(format: String, arguments: AnyRef*): Unit =
    log(Level.Info, format, arguments)

  override def info(msg: String, t: Throwable): Unit = log(Level.Info, msg, t)

  override def isInfoEnabled(@unused marker: Marker): Boolean =
    isEnabled(Level.Info)

  override def info(@unused marker: Marker, msg: String): Unit =
    log(Level.Info, msg)

  override def info(@unused marker: Marker, format: String, arg: AnyRef): Unit =
    log(Level.Info, format, arg)

  override def info(
    @unused marker: Marker,
    format: String,
    arg1: AnyRef,
    arg2: AnyRef
  ): Unit = log(Level.Info, format, arg1, arg2)

  override def info(
    @unused marker: Marker,
    format: String,
    arguments: AnyRef*
  ): Unit =
    log(Level.Info, format, arguments)

  override def info(@unused marker: Marker, msg: String, t: Throwable): Unit =
    log(Level.Info, msg, t)

  override def isWarnEnabled: Boolean = isEnabled(Level.Warning)

  override def warn(msg: String): Unit = log(Level.Warning, msg)

  override def warn(format: String, arg: AnyRef): Unit =
    log(Level.Warning, format, arg)

  override def warn(format: String, arguments: AnyRef*): Unit =
    log(Level.Warning, format, arguments)

  override def warn(format: String, arg1: AnyRef, arg2: AnyRef): Unit =
    log(Level.Warning, format, arg1, arg2)

  override def warn(msg: String, t: Throwable): Unit =
    log(Level.Warning, msg, t)

  override def isWarnEnabled(@unused marker: Marker): Boolean =
    isEnabled(Level.Warning)

  override def warn(@unused marker: Marker, msg: String): Unit =
    log(Level.Warning, msg)

  override def warn(@unused marker: Marker, format: String, arg: AnyRef): Unit =
    log(Level.Warning, format, arg)

  override def warn(
    @unused marker: Marker,
    format: String,
    arg1: AnyRef,
    arg2: AnyRef
  ): Unit = log(Level.Warning, format, arg1, arg2)

  override def warn(
    @unused marker: Marker,
    format: String,
    arguments: AnyRef*
  ): Unit =
    log(Level.Warning, format, arguments)

  override def warn(@unused marker: Marker, msg: String, t: Throwable): Unit =
    log(Level.Warning, msg, t)

  override def isErrorEnabled: Boolean = isEnabled(Level.Error)

  override def error(msg: String): Unit = log(Level.Error, msg)

  override def error(format: String, arg: AnyRef): Unit =
    log(Level.Error, format, arg)

  override def error(format: String, arg1: AnyRef, arg2: AnyRef): Unit =
    log(Level.Error, format, arg1, arg2)

  override def error(format: String, arguments: AnyRef*): Unit =
    log(Level.Error, format, arguments)

  override def error(msg: String, t: Throwable): Unit = log(Level.Error, msg, t)

  override def isErrorEnabled(@unused marker: Marker): Boolean =
    isEnabled(Level.Error)

  override def error(@unused marker: Marker, msg: String): Unit =
    log(Level.Error, msg)

  override def error(
    @unused marker: Marker,
    format: String,
    arg: AnyRef
  ): Unit =
    log(Level.Error, format, arg)

  override def error(
    @unused marker: Marker,
    format: String,
    arg1: AnyRef,
    arg2: AnyRef
  ): Unit = log(Level.Error, format, arg1, arg2)

  override def error(
    @unused marker: Marker,
    format: String,
    arguments: AnyRef*
  ): Unit =
    log(Level.Error, format, arguments)

  override def error(@unused marker: Marker, msg: String, t: Throwable): Unit =
    log(Level.Error, msg, t)
}
