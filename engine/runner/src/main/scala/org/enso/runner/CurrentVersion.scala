package org.enso.runner

import nl.gn0s1s.bump.SemVer

object CurrentVersion {
  lazy val version: SemVer = computeVersion()

  private def computeVersion(): SemVer = {
    val buildVersion = SemVer(buildinfo.Info.ensoVersion).getOrElse {
      throw new IllegalStateException(
        "Fatal error: Enso version included in buildinfo is not a valid " +
        "semver string, this should never happen."
      )
    }
    if (buildinfo.Info.isRelease) buildVersion
    else
      sys.props
        .get("enso.version.override")
        .flatMap(SemVer(_))
        .getOrElse(buildVersion)
  }
}
