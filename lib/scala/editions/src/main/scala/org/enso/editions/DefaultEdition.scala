package org.enso.editions

import buildinfo.Info

/** Allows to get the default edition associated with the current Enso version.
  *
  * In the future this will need to depend on the
  * [[org.enso.distribution.DistributionManager]] and provide the default based
  * on the settings, but for now the default edition is saved in the build
  * metadata.
  */
object DefaultEdition {

  /** Returns the default edition associated with the current Enso version. */
  def getDefaultEdition: Editions.RawEdition =
    Editions.Raw.Edition(parent = Some(Info.currentEdition))
}
