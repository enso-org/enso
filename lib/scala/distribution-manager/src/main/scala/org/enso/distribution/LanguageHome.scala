package org.enso.distribution

import java.nio.file.Path

case class LanguageHome(languageHome: Path) {
  def editions: Path =
    languageHome.resolve(DistributionManager.EDITIONS_DIRECTORY)
  def libraries: Path =
    languageHome.resolve(DistributionManager.LIBRARIES_DIRECTORY)
}
