package org.enso.launcher

/**
  * This is a temporary object that should be at some point replace with the
  * actual logging service.
  */
object Logger {
  // TODO [RW] logging
  def debug(msg: String): Unit = System.err.println("[debug] " + msg)
  def info(msg: String): Unit  = System.err.println("[info] " + msg)
  def warn(msg: String): Unit  = System.err.println("[warn] " + msg)
  def error(msg: String): Unit = System.err.println("[error] " + msg)
}
