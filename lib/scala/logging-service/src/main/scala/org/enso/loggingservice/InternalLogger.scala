package org.enso.loggingservice

object InternalLogger {
  def error(msg: String): Unit = {
    System.err.println(s"[internal-logger-error] $msg")
  }
}
