package org.enso.launcher

/**
  * This is a temporary object that should be at some point replace with the
  * actual logging service.
  */
object Logger {
  // TODO [RW] this should be replaced with the proper logging service once it
  //  is implemented in #1031

  private case class Level(name: String, level: Int)
  private val Debug   = Level("debug", 1)
  private val Info    = Level("info", 2)
  private val Warning = Level("warn", 3)
  private val Error   = Level("error", 4)

  private val logLevel = Warning
  private def log(level: Level, msg: => String): Unit =
    if (level.level >= logLevel.level)
      System.err.println(s"[${level.name}] $msg")

  def debug(msg: => String): Unit = log(Debug, msg)
  def info(msg: => String): Unit  = log(Info, msg)
  def warn(msg: => String): Unit  = log(Warning, msg)
  def error(msg: => String): Unit = log(Error, msg)
}
