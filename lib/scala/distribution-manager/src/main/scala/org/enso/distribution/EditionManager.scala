package org.enso.distribution

import org.enso.editions.provider.FileSystemEditionProvider
import org.enso.editions.{EditionResolver, Editions, EnsoVersion}
import org.enso.pkg.EngineVersionResolver

import scala.util.Try

case class EditionManager(distributionManager: DistributionManager) {
  val editionProvider = FileSystemEditionProvider(
    distributionManager.paths.editionSearchPaths.toList
  )

  val editionResolver       = EditionResolver(editionProvider)
  val engineVersionResolver = EngineVersionResolver(editionProvider)

  def resolveEdition(
    edition: Editions.RawEdition
  ): Try[Editions.ResolvedEdition] =
    editionResolver.resolve(edition).toTry

  def resolveEngineVersion(edition: Editions.RawEdition): Try[EnsoVersion] =
    engineVersionResolver.resolveEnsoVersion(edition).toTry
}
