package org.enso.librarymanager

import org.enso.distribution.DistributionManager
import org.enso.editions.provider.FileSystemEditionProvider
import org.enso.editions.{EditionResolver, Editions}

import scala.util.Try

case class EditionManager(distributionManager: DistributionManager) {
  val editionProvider = FileSystemEditionProvider(
    distributionManager.paths.editionSearchPaths.toList
  )

  val editionResolver = EditionResolver(editionProvider)

  def resolveEdition(
    edition: Editions.RawEdition
  ): Try[Editions.ResolvedEdition] =
    editionResolver.resolve(edition).toTry
}
