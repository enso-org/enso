package org.enso.launcher

import buildinfo.Info
import nl.gn0s1s.bump.SemVer

object CurrentVersion {

  /**
    * Version of the launcher.
    */
  val version: SemVer = SemVer(Info.ensoVersion).getOrElse {
    throw new IllegalStateException("Cannot parse the built-in version.")
  }
}
