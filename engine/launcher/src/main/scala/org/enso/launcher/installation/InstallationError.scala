package org.enso.launcher.installation

/** Indicates an installation failure.
  *
  * Possibly installation being manually cancelled.
  */
case class InstallationError(message: String)
    extends RuntimeException(message) {
  override def toString: String = message
}
