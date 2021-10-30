package org.enso.distribution.config

/** Indicates that the config is invalid.
  */
case class InvalidConfigError(message: String, cause: Throwable)
    extends RuntimeException(message, cause) {

  /** @inheritdoc
    */
  override def toString: String = message
}
