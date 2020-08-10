package org.enso.launcher.project

import nl.gn0s1s.bump.SemVer
import org.enso.pkg.Package

class Project(pkg: Package[_]) {
  def getVersion: SemVer =
    SemVer(pkg.config.version).getOrElse {
      throw new IllegalArgumentException(
        s"Error loading project ${pkg.name} - the version string is not a " +
        s"valid Enso version."
      )
    }
}
