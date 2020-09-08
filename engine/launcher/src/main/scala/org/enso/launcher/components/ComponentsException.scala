package org.enso.launcher.components

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.CurrentVersion
import org.enso.launcher.cli.GlobalCLIOptions

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
  *
  * This error can be recovered by
  * [[org.enso.launcher.upgrade.LauncherUpgrader.recoverUpgradeRequiredErrors]]
  * which can perform the upgrade and re-run the requested command with the
  * newer version.
  *
  * @param expectedLauncherVersion the minimum launcher version that is required
  * @param globalCLIOptions the CLI options that should be passed to an upgrader
  *                         if an upgrade is requested
  */
case class LauncherUpgradeRequiredError(
  expectedLauncherVersion: SemVer,
  globalCLIOptions: GlobalCLIOptions
) extends ComponentsException(
      s"Minimum launcher version required to use this engine is " +
      s"$expectedLauncherVersion"
    ) {

  /**
    * @inheritdoc
    */
  override def toString: String =
    s"This launcher version is ${CurrentVersion.version}, but " +
    s"$expectedLauncherVersion is required to run this engine. If you want " +
    s"to use it, upgrade the launcher with `enso upgrade`."
}

/**
  * Indicates a component is not recognized.
  */
case class UnrecognizedComponentError(message: String, cause: Throwable = null)
    extends ComponentsException(message, cause)

/**
  * Indicates that the component is installed, but its installation is
  * corrupted.
  *
  * Most common reason for this exception is that some critical files are
  * missing.
  */
case class CorruptedComponentError(message: String, cause: Throwable = null)
    extends ComponentsException(message, cause)

/**
  * Indicates the requested component is not installed.
  */
case class ComponentMissingError(message: String, cause: Throwable = null)
    extends ComponentsException(message, cause)
