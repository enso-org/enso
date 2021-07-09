package org.enso.distribution

import java.nio.file.Path

/** A helper class that provides paths for bundled components inside of an
  * engine distribution from which the runtime is being run.
  */
case class LanguageHome(languageHome: Path) {
  private val rootPath = languageHome.getParent.toAbsolutePath.normalize

  /** The path to editions bundled with the engine. */
  def editions: Path =
    rootPath.resolve(DistributionManager.EDITIONS_DIRECTORY)

  /** The path to libraries bundled with the engine.
    *
    * The libraries there comply to the directory structure of an auxiliary
    * library cache.
    */
  def libraries: Path =
    rootPath.resolve(DistributionManager.LIBRARIES_DIRECTORY)
}
