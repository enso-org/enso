package org.enso.launcher

import buildinfo.Info
import nl.gn0s1s.bump.SemVer

object CurrentVersion {

  private var currentVersion: SemVer = SemVer(Info.ensoVersion).getOrElse {
    throw new IllegalStateException("Cannot parse the built-in version.")
  }

  /**
    * Version of the launcher.
    */
  def version: SemVer = currentVersion

  /**
    * Override launcher version with the provided one.
    *
    * Internal helper method used for testing.
    */
  def internalOverrideVersion(newVersion: SemVer): Unit =
    if (Info.isRelease)
      throw new IllegalStateException(
        "Internal testing function used in a release build."
      )
    else {
      Logger.debug(s"[TEST] Overriding version to $newVersion.")
      currentVersion = newVersion
    }

}
