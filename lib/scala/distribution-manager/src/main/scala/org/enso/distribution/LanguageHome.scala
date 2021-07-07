package org.enso.distribution

import java.nio.file.Path

case class LanguageHome(languageHome: Path) {
  private val rootPath = languageHome.getParent.toAbsolutePath.normalize
  def editions: Path =
    rootPath.resolve(DistributionManager.EDITIONS_DIRECTORY)
  def libraries: Path =
    rootPath.resolve(DistributionManager.LIBRARIES_DIRECTORY)
}
