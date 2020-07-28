package org.enso.launcher

/**
  * This is a temporary object that should be at some point replaced with the
  * actual logging service.
  *
  * TODO [RW] this should be replaced with the proper logging service once it
  *  is implemented in #1031
  */
object Logger {
  private case class Level(name: String, level: Int)
  private val Debug   = Level("debug", 1)
  private val Info    = Level("info", 2)
  private val Warning = Level("warn", 3)
  private val Error   = Level("error", 4)

  private val logLevel = Warning
  private def log(level: Level, msg: => String): Unit =
    if (level.level >= logLevel.level)
      System.err.println(s"[${level.name}] $msg")

  /**
    * Logs a debug level message.
    */
  def debug(msg: => String): Unit = log(Debug, msg)

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
}
