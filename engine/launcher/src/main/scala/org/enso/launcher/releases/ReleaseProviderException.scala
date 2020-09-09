package org.enso.launcher.releases

/**
  * Indicates a release provider failure.
  */
case class ReleaseProviderException(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause) {

  /**
    * @inheritdoc
    */
  override def toString: String =
    s"A problem occurred when trying to find the release: $message"
}
