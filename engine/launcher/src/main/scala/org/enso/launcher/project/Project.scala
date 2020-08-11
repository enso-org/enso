package org.enso.launcher.project

import java.io.File
import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.GlobalConfigurationManager
import org.enso.pkg.{Config, Package}

/**
  * Represents an Enso project.
  *
  * @param pkg the package associated with the project
  * @param globalConfigurationManager the configuration manager that is
  *                                   necessary when the project version is set
  *                                   to `default`
  */
class Project(
  pkg: Package[File],
  globalConfigurationManager: GlobalConfigurationManager
) {

  /**
    * The Enso engine version associated with the project.
    *
    * If the version in the configuration is set to `default`, the locally
    * default Enso version is used.
    */
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

  /**
    * The path to the content root of the project.
    */
  def path: Path = pkg.root.toPath
}
