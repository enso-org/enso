package org.enso.distribution

import org.enso.editions
import org.enso.editions.provider.FileSystemEditionProvider
import org.enso.editions.{
  DefaultEnsoVersion,
  EditionResolver,
  Editions,
  EnsoVersion
}

import scala.util.{Success, Try}

/** A helper class for resolving editions backed by the Edition storage managed
  * by the DistributionManager.
  */
case class EditionManager(distributionManager: DistributionManager) {
  private val editionProvider = FileSystemEditionProvider(
    distributionManager.paths.editionSearchPaths.toList
  )

  private val editionResolver = EditionResolver(editionProvider)
  private val engineVersionResolver =
    editions.EngineVersionResolver(editionProvider)

  /** Resolves a raw edition, loading its parents from the edition search path.
    * @param edition the edition to resolve
    * @return the resolved edition
    */
  def resolveEdition(
    edition: Editions.RawEdition
  ): Try[Editions.ResolvedEdition] =
    editionResolver.resolve(edition).toTry

  /** Resolves the engine version that should be used based on the provided raw
    * edition configuration.
    * @param edition the edition configuration to base the selected version on;
    *                if it is not specified, it will fallback to the default
    *                engine version
    * @return the resolved engine version
    */
  def resolveEngineVersion(
    edition: Option[Editions.RawEdition]
  ): Try[EnsoVersion] =
    edition
      .map(engineVersionResolver.resolveEnsoVersion(_).toTry)
      .getOrElse(Success(DefaultEnsoVersion))
}
