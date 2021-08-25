package org.enso.languageserver.libraries

import org.enso.distribution.{DistributionManager, LanguageHome}
import org.enso.distribution.locking.ResourceManager

/** Gathers configuration needed by the library installer used in the `library/preinstall` endpoint. */
case class LibraryInstallerConfig(
  distributionManager: DistributionManager,
  resourceManager: ResourceManager,
  languageHome: Option[LanguageHome]
)
