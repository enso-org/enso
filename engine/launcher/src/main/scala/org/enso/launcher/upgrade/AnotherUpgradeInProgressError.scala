package org.enso.launcher.upgrade

/**
  * Error thrown when trying to upgrade, but another upgrade is in progress and
  * this one cannot continue.
  */
case class AnotherUpgradeInProgressError()
    extends RuntimeException(
      "Another upgrade is in progress. Please wait for it to finish."
    ) {

  /**
    * @inheritdoc
    */
  override def toString: String = getMessage
}
