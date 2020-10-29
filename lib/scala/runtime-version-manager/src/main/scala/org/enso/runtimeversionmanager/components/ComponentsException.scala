package org.enso.runtimeversionmanager.components

import nl.gn0s1s.bump.SemVer

/** A base class for exceptions caused by [[RuntimeVersionManager]] logic.
  */
sealed class ComponentsException(
  message: String,
  cause: Throwable = null
) extends RuntimeException(message, cause) {

  /** @inheritdoc
    */
  override def toString: String = {
    val causeMessage = if (cause != null) s" (Caused by: $cause)" else ""
    message + causeMessage
  }
}

/** Represents an installation failure.
  */
case class InstallationError(message: String, cause: Throwable = null)
    extends ComponentsException(message, cause) {

  /** @inheritdoc
    */
  override def toString: String = s"Installation failed: $message"
}

/** Indicates a component is not recognized.
  */
case class UnrecognizedComponentError(message: String, cause: Throwable = null)
    extends ComponentsException(message, cause)

/** Indicates that the component is installed, but its installation is
  * corrupted.
  *
  * Most common reason for this exception is that some critical files are
  * missing.
  */
case class CorruptedComponentError(message: String, cause: Throwable = null)
    extends ComponentsException(message, cause)

/** Indicates the requested component is not installed.
  */
case class ComponentMissingError(message: String, cause: Throwable = null)
    extends ComponentsException(message, cause)

/** Indicates that a requested engine version requires a newer launcher version.
  *
  * @param expectedLauncherVersion the minimum launcher version that is required
  */
case class UpgradeRequiredError(
  expectedLauncherVersion: SemVer
) extends ComponentsException(
      s"Minimum version required to use this engine is " +
      s"$expectedLauncherVersion."
    )
