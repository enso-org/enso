package org.enso.runtimeversionmanager.runner

import org.enso.editions.Editions
import org.enso.pkg.{Config, Package}

import java.io.File
import java.nio.file.Path

/** Represents an Enso project.
  *
  * @param pkg the package associated with the project
  */
class Project(
  pkg: Package[File]
) {

  /** The edition associated with the project. */
  def edition: Editions.RawEdition = pkg.config.edition

  /** The package name of the project. */
  def name: String = pkg.name

  /** The path to the content root of the project. */
  def path: Path = pkg.root.toPath

  /** Project configuration. */
  def config: Config = pkg.config
}
