package org.enso.languageserver.libraries

import org.enso.distribution.{DistributionManager, LanguageHome}
import org.enso.distribution.locking.ResourceManager

/** Gathers configuration needed by the library installer used in the
  * `library/preinstall` endpoint.
  *
  * @param distributionManager the distribution manager
  * @param resourceManager a resource manager instance
  * @param languageHome language home, if detected / applicable
  */
case class LibraryInstallerConfig(
  distributionManager: DistributionManager,
  resourceManager: ResourceManager,
  languageHome: Option[LanguageHome]
)
