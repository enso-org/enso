package org.enso.launcher

/**
  * This is a temporary object that should be at some point replace with the
  * actual logging service.
  */
object Logger {
  // TODO [RW] this should be replaced with the proper logging service once it
  //  is implemented

  private val enabled: Boolean = true

  def debug(msg: => String): Unit =
    if (enabled) System.err.println("[debug] " + msg)
  def info(msg: => String): Unit =
    if (enabled) System.err.println("[info] " + msg)
  def warn(msg: => String): Unit =
    if (enabled) System.err.println("[warn] " + msg)
  def error(msg: => String): Unit =
    if (enabled) System.err.println("[error] " + msg)
}
