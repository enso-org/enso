package org.enso.launcher.upgrade

/** Indicates an error during an upgrade.
  */
case class UpgradeError(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause) {

  /** @inheritdoc
    */
  override def toString: String = message
}
