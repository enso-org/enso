package org.enso.launcher.project

import java.io.File
import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.GlobalConfigurationManager
import org.enso.pkg.{Config, Package}

class Project(
  pkg: Package[File],
  globalConfigurationManager: GlobalConfigurationManager
) {
  def version: SemVer =
    if (pkg.config.ensoVersion == Config.defaultEnsoVersion)
      globalConfigurationManager.defaultVersion
    else
      SemVer(pkg.config.ensoVersion).getOrElse {
        throw new IllegalArgumentException(
          s"Error loading project ${pkg.name} - the version string is not a " +
          s"valid Enso version."
        )
      }

  def path: Path = pkg.root.toPath

}
