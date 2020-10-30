package org.enso.runtimeversionmanager.runner

import java.io.File
import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.runtimeversionmanager.config.GlobalConfigurationManager
import org.enso.pkg.{Config, DefaultEnsoVersion, Package, SemVerEnsoVersion}

/** Represents an Enso project.
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

  /** The Enso engine version associated with the project.
    *
    * If the version in the configuration is set to `default`, the locally
    * default Enso version is used.
    */
  def version: SemVer =
    pkg.config.ensoVersion match {
      case DefaultEnsoVersion =>
        globalConfigurationManager.defaultVersion
      case SemVerEnsoVersion(version) => version
    }

  /** The package name of the project.
    */
  def name: String = pkg.name

  /** The path to the content root of the project.
    */
  def path: Path = pkg.root.toPath

  /** Project configuration.
    */
  def config: Config = pkg.config
}
