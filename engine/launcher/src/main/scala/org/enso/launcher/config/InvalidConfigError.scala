package org.enso.launcher.config

case class InvalidConfigError(message: String, cause: Throwable)
    extends RuntimeException(message, cause) {
  override def toString: String = message
}
