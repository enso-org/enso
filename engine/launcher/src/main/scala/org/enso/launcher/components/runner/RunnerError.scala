package org.enso.launcher.components.runner

case class RunnerError(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause) {
  override def toString: String = message
}
