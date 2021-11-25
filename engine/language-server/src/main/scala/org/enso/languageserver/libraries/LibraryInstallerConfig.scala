package org.enso.languageserver.libraries

import org.enso.distribution.locking.ResourceManager
import org.enso.distribution.{DistributionManager, LanguageHome}
import org.enso.libraryupload.DependencyExtractor

import java.io.File

/** Gathers configuration needed by the library installer used in the
  * `library/preinstall` endpoint.
  *
  * @param distributionManager the distribution manager
  * @param resourceManager a resource manager instance
  * @param languageHome language home, if detected / applicable
  * @param dependencyExtractor a dependency extractor
  */
case class LibraryInstallerConfig(
  distributionManager: DistributionManager,
  resourceManager: ResourceManager,
  languageHome: Option[LanguageHome],
  dependencyExtractor: DependencyExtractor[File]
)
