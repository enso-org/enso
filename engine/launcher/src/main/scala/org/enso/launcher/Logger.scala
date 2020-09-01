package org.enso.launcher

import java.io.PrintStream

/**
  * This is a temporary object that should be at some point replaced with the
  * actual logging service.
  *
  * TODO [RW] this should be replaced with the proper logging service once it
  *  is implemented in #1031
  */
object Logger {

  // TODO [RW] this stream closure is not very efficient, but it allows the
  //  Logger to respect stream redirection from Console.withErr. Ideally, the
  //  new logging service should allow some way to capture logs for use in the
  //  tests.
  private case class Level(name: String, level: Int, stream: () => PrintStream)
  private val Debug   = Level("debug", 1, () => Console.err)
  private val Info    = Level("info", 2, () => Console.out)
  private val Warning = Level("warn", 3, () => Console.err)
  private val Error   = Level("error", 4, () => Console.err)

  private var logLevel = Info
  private def log(level: Level, msg: => String): Unit =
    if (level.level >= logLevel.level) {
      val stream = level.stream()
      stream.println(s"[${level.name}] $msg")
      stream.flush()
    }

  /**
    * Logs a debug level message.
    */
  def debug(msg: => String): Unit = log(Debug, msg)

  /**
    * Logs a debug level message and attaches a stack trace.
    */
  def debug(msg: => String, throwable: => Throwable): Unit = {
    log(Debug, msg)
    trace(throwable)
  }

  /**
    * Logs an info level message.
    */
  def info(msg: => String): Unit = log(Info, msg)

  /**
    * Logs a warning level message.
    */
  def warn(msg: => String): Unit = log(Warning, msg)

  /**
    * Logs an error level message.
    */
  def error(msg: => String): Unit = log(Error, msg)

  /**
    * Logs an error level message and attaches an optional, debug-level stack
    * trace.
    */
  def error(msg: => String, throwable: => Throwable): Unit = {
    log(Error, msg)
    trace(throwable)
  }

  /**
    * Logs a stack trace of an exception.
    */
  def trace(throwable: => Throwable): Unit =
    if (Debug.level >= logLevel.level)
      throwable.printStackTrace()

  /**
    * Runs the provided action with a log level that will allow only for errors
    * and returns its result.
    *
    * Warning: This function is not thread safe, so using it in tests that are
    * run in parallel without forking may lead to an inconsistency in logging.
    * This is just a *temporary* solution until a fully-fledged logging service
    * is developed #1031.
    */
  def suppressWarnings[R](action: => R): R = {
    val oldLevel = logLevel
    try {
      logLevel = Error
      action
    } finally {
      logLevel = oldLevel
    }
  }
}
