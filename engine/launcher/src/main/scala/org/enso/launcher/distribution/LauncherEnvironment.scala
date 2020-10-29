package org.enso.launcher.distribution

import java.nio.file.Path

import com.typesafe.scalalogging.Logger
import org.enso.runtimeversionmanager.Environment

/** Default [[Environment]] to use in the launcher.
  *
  * In non-release mode, it allows internal test scripts to override the
  * executable location.
  */
object LauncherEnvironment extends Environment {

  /** @inheritdoc */
  override def getPathToRunningExecutable: Path =
    executablePathOverride.getOrElse(super.getPathToRunningExecutable)

  private var executablePathOverride: Option[Path] = None

  /** Overrides the return value of [[getPathToRunningExecutable]] with the
    * provided path.
    *
    * Internal method used for testing. It should be called as early as
    * possible, before [[getPathToRunningExecutable]] is called.
    */
  def internalOverrideExecutableLocation(newLocation: Path): Unit =
    if (buildinfo.Info.isRelease) {
      throw new IllegalStateException(
        "Internal testing function internalOverrideExecutableLocation used " +
        "in a release build."
      )
    } else {
      Logger("TEST").debug(s"Overriding location to $newLocation.")
      executablePathOverride = Some(newLocation)
    }
}
