package org.enso.runtimeversionmanager.config

/** Indicates that the config is invalid.
  */
case class InvalidConfigError(message: String, cause: Throwable)
    extends RuntimeException(message, cause) {

  /** @inheritdoc
    */
  override def toString: String = message
}
