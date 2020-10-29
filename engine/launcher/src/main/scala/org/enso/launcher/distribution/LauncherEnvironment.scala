package org.enso.launcher.distribution

import java.nio.file.Path

import com.typesafe.scalalogging.Logger
import org.enso.componentmanager.Environment

object LauncherEnvironment extends Environment {
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
