package org.enso.launcher.components

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.Launcher

/**
  * A base class for exceptions caused by [[ComponentsManager]] logic.
  */
sealed class ComponentsException(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause) {

  /**
    * @inheritdoc
    */
  override def toString: String = {
    val causeMessage = if (cause != null) s" (Caused by: $cause)" else ""
    message + causeMessage
  }
}

/**
  * Represents an installation failure.
  */
case class InstallationError(message: String, cause: Throwable = null)
    extends ComponentsException(message, cause) {

  /**
    * @inheritdoc
    */
  override def toString: String = s"Installation failed: $message"
}

/**
  * Indicates that a requested engine version requires a newer launcher version.
  */
case class LauncherUpgradeRequiredError(expectedVersion: SemVer)
    extends ComponentsException(
      s"Minimum launcher version required to use this engine is " +
      s"$expectedVersion"
    ) {

  /**
    * @inheritdoc
    */
  override def toString: String =
    s"This launcher version is ${Launcher.version}, but $expectedVersion " +
    s"is required to run this engine. If you want to use it, upgrade the " +
    s"launcher with `enso upgrade`."
}

/**
  * Indicates a component is not recognized.
  */
case class UnrecognizedComponentError(message: String, cause: Throwable = null)
    extends ComponentsException(message, cause)

case class ComponentMissingError(message: String, cause: Throwable = null)
    extends ComponentsException(message, cause)
