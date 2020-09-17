package org.enso.launcher.upgrade

/**
  * Indicates an error during an upgrade.
  */
class UpgradeError(message: String, cause: Throwable)
    extends RuntimeException(message, cause) {

  /**
    * @inheritdoc
    */
  override def toString: String = message
}

object UpgradeError {
  def apply(message: String, throwable: Throwable = null): UpgradeError =
    new UpgradeError(message, throwable)
}

case class AnotherUpgradeInProgressError()
    extends UpgradeError(
      "Another upgrade is in progress. Please wait for it to finish.",
      null
    )
