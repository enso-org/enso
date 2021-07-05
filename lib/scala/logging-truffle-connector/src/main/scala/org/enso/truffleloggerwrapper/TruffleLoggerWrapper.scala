package org.enso.truffleloggerwrapper

import com.oracle.truffle.api.TruffleLogger
import org.enso.logger.masking.Masking
import org.enso.polyglot.LanguageInfo
import org.slf4j.helpers.MessageFormatter
import org.slf4j.{Logger, Marker}

import java.util.logging.Level
import scala.annotation.unused

/** A wrapper around [[TruffleLogger]] that abides by the SLF4J's [[Logger]]
  * interface.
  *
  * It is used so that libraries which are used both inside and outside of the
  * runtime can simply use the SLF4J API and the log messages are passed to the
  * correct backend (in the case of the runtime, they are forwarded to the
  * [[TruffleLogger]]).
  */
class TruffleLoggerWrapper(name: String, masking: Masking) extends Logger {
  final private val underlying = TruffleLogger.getLogger(LanguageInfo.ID, name)

  override def getName: String = underlying.getName

  private def isEnabled(level: Level): Boolean =
    underlying.isLoggable(level)

  private def log(
    level: Level,
    msg: String
  ): Unit = {
    if (isEnabled(level)) {
      underlying.log(level, msg)
    }
  }

  private def log(
    level: Level,
    format: String,
    arg: AnyRef
  ): Unit = {
    if (isEnabled(level)) {
      val maskedArg = masking.mask(arg)
      val fp        = MessageFormatter.format(format, maskedArg)
      if (fp.getThrowable == null) {
        underlying.log(level, fp.getMessage)
      } else {
        underlying.log(level, fp.getMessage, fp.getThrowable)
      }
    }
  }

  private def log(
    level: Level,
    format: String,
    arg1: AnyRef,
    arg2: AnyRef
  ): Unit = {
    if (isEnabled(level)) {
      val maskedArg1 = masking.mask(arg1)
      val maskedArg2 = masking.mask(arg2)
      val fp         = MessageFormatter.format(format, maskedArg1, maskedArg2)
      if (fp.getThrowable == null) {
        underlying.log(level, fp.getMessage)
      } else {
        underlying.log(level, fp.getMessage, fp.getThrowable)
      }
    }
  }

  private def log(
    level: Level,
    format: String,
    args: Seq[AnyRef]
  ): Unit = {
    if (isEnabled(level)) {
      val maskedArgs = args.map(masking.mask)
      val fp         = MessageFormatter.arrayFormat(format, maskedArgs.toArray)
      if (fp.getThrowable == null) {
        underlying.log(level, fp.getMessage)
      } else {
        underlying.log(level, fp.getMessage, fp.getThrowable)
      }
    }
  }

  private def log(
    level: Level,
    msg: String,
    throwable: Throwable
  ): Unit = {
    if (isEnabled(level)) {
      underlying.log(level, msg, throwable)
    }
  }

  override def isTraceEnabled: Boolean = isEnabled(Level.FINER)

  override def trace(msg: String): Unit = log(Level.FINER, msg)

  override def trace(format: String, arg: AnyRef): Unit =
    log(Level.FINER, format, arg)

  override def trace(format: String, arg1: AnyRef, arg2: AnyRef): Unit =
    log(Level.FINER, format, arg1, arg2)

  override def trace(format: String, arguments: AnyRef*): Unit =
    log(Level.FINER, format, arguments)

  override def trace(msg: String, t: Throwable): Unit = log(Level.FINER, msg, t)

  override def isTraceEnabled(@unused marker: Marker): Boolean =
    isEnabled(Level.FINER)

  override def trace(@unused marker: Marker, msg: String): Unit =
    log(Level.FINER, msg)

  override def trace(
    @unused marker: Marker,
    format: String,
    arg: AnyRef
  ): Unit =
    log(Level.FINER, format, arg)

  override def trace(
    @unused marker: Marker,
    format: String,
    arg1: AnyRef,
    arg2: AnyRef
  ): Unit = log(Level.FINER, format, arg1, arg2)

  override def trace(
    @unused marker: Marker,
    format: String,
    argArray: AnyRef*
  ): Unit =
    log(Level.FINER, format, argArray)

  override def trace(@unused marker: Marker, msg: String, t: Throwable): Unit =
    log(Level.FINER, msg, t)

  override def isDebugEnabled: Boolean = isEnabled(Level.FINE)

  override def debug(msg: String): Unit = log(Level.FINE, msg)

  override def debug(format: String, arg: AnyRef): Unit =
    log(Level.FINE, format, arg)

  override def debug(format: String, arg1: AnyRef, arg2: AnyRef): Unit =
    log(Level.FINE, format, arg1, arg2)

  override def debug(format: String, arguments: AnyRef*): Unit =
    log(Level.FINE, format, arguments)

  override def debug(msg: String, t: Throwable): Unit = log(Level.FINE, msg, t)

  override def isDebugEnabled(@unused marker: Marker): Boolean = isEnabled(
    Level.FINE
  )

  override def debug(@unused marker: Marker, msg: String): Unit =
    log(Level.FINE, msg)

  override def debug(
    @unused marker: Marker,
    format: String,
    arg: AnyRef
  ): Unit =
    log(Level.FINE, format, arg)

  override def debug(
    @unused marker: Marker,
    format: String,
    arg1: AnyRef,
    arg2: AnyRef
  ): Unit = log(Level.FINE, format, arg1, arg2)

  override def debug(
    @unused marker: Marker,
    format: String,
    arguments: AnyRef*
  ): Unit =
    log(Level.FINE, format, arguments)

  override def debug(@unused marker: Marker, msg: String, t: Throwable): Unit =
    log(Level.FINE, msg, t)

  override def isInfoEnabled: Boolean = isEnabled(Level.INFO)

  override def info(msg: String): Unit = log(Level.INFO, msg)

  override def info(format: String, arg: AnyRef): Unit =
    log(Level.INFO, format, arg)

  override def info(format: String, arg1: AnyRef, arg2: AnyRef): Unit =
    log(Level.INFO, format, arg1, arg2)

  override def info(format: String, arguments: AnyRef*): Unit =
    log(Level.INFO, format, arguments)

  override def info(msg: String, t: Throwable): Unit = log(Level.INFO, msg, t)

  override def isInfoEnabled(@unused marker: Marker): Boolean = isEnabled(
    Level.INFO
  )

  override def info(@unused marker: Marker, msg: String): Unit =
    log(Level.INFO, msg)

  override def info(@unused marker: Marker, format: String, arg: AnyRef): Unit =
    log(Level.INFO, format, arg)

  override def info(
    @unused marker: Marker,
    format: String,
    arg1: AnyRef,
    arg2: AnyRef
  ): Unit = log(Level.INFO, format, arg1, arg2)

  override def info(
    @unused marker: Marker,
    format: String,
    arguments: AnyRef*
  ): Unit =
    log(Level.INFO, format, arguments)

  override def info(@unused marker: Marker, msg: String, t: Throwable): Unit =
    log(Level.INFO, msg, t)

  override def isWarnEnabled: Boolean = isEnabled(Level.WARNING)

  override def warn(msg: String): Unit = log(Level.WARNING, msg)

  override def warn(format: String, arg: AnyRef): Unit =
    log(Level.WARNING, format, arg)

  override def warn(format: String, arguments: AnyRef*): Unit =
    log(Level.WARNING, format, arguments)

  override def warn(format: String, arg1: AnyRef, arg2: AnyRef): Unit =
    log(Level.WARNING, format, arg1, arg2)

  override def warn(msg: String, t: Throwable): Unit =
    log(Level.WARNING, msg, t)

  override def isWarnEnabled(@unused marker: Marker): Boolean = isEnabled(
    Level.WARNING
  )

  override def warn(@unused marker: Marker, msg: String): Unit =
    log(Level.WARNING, msg)

  override def warn(@unused marker: Marker, format: String, arg: AnyRef): Unit =
    log(Level.WARNING, format, arg)

  override def warn(
    @unused marker: Marker,
    format: String,
    arg1: AnyRef,
    arg2: AnyRef
  ): Unit = log(Level.WARNING, format, arg1, arg2)

  override def warn(
    @unused marker: Marker,
    format: String,
    arguments: AnyRef*
  ): Unit =
    log(Level.WARNING, format, arguments)

  override def warn(@unused marker: Marker, msg: String, t: Throwable): Unit =
    log(Level.WARNING, msg, t)

  override def isErrorEnabled: Boolean = isEnabled(Level.SEVERE)

  override def error(msg: String): Unit = log(Level.SEVERE, msg)

  override def error(format: String, arg: AnyRef): Unit =
    log(Level.SEVERE, format, arg)

  override def error(format: String, arg1: AnyRef, arg2: AnyRef): Unit =
    log(Level.SEVERE, format, arg1, arg2)

  override def error(format: String, arguments: AnyRef*): Unit =
    log(Level.SEVERE, format, arguments)

  override def error(msg: String, t: Throwable): Unit =
    log(Level.SEVERE, msg, t)

  override def isErrorEnabled(@unused marker: Marker): Boolean = isEnabled(
    Level.SEVERE
  )

  override def error(@unused marker: Marker, msg: String): Unit =
    log(Level.SEVERE, msg)

  override def error(
    @unused marker: Marker,
    format: String,
    arg: AnyRef
  ): Unit =
    log(Level.SEVERE, format, arg)

  override def error(
    @unused marker: Marker,
    format: String,
    arg1: AnyRef,
    arg2: AnyRef
  ): Unit = log(Level.SEVERE, format, arg1, arg2)

  override def error(
    @unused marker: Marker,
    format: String,
    arguments: AnyRef*
  ): Unit =
    log(Level.SEVERE, format, arguments)

  override def error(@unused marker: Marker, msg: String, t: Throwable): Unit =
    log(Level.SEVERE, msg, t)
}
