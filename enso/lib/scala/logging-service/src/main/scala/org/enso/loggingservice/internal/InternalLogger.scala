package org.enso.loggingservice.internal

/** An internal logger used for reporting errors within the logging service
  * itself.
  *
  * As the logging service cannot be used to report its own errors (because a
  * logging service error likely means that it is in a unusable state), its
  * errors are printed to the standard error output.
  */
object InternalLogger {

  /** Reports an internal logging service error with the given message.
    */
  def error(message: String): Unit = {
    System.err.println(s"[internal-logger-error] $message")
  }
}
