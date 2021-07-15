package org.enso.distribution

import nl.gn0s1s.bump.SemVer
import org.enso.editions
import org.enso.editions.provider.{EditionProvider, FileSystemEditionProvider}
import org.enso.editions.{EditionResolver, Editions}

import scala.util.Try

/** A helper class for resolving editions. */
class EditionManager(editionProvider: EditionProvider) {
  private val editionResolver = EditionResolver(editionProvider)
  private val engineVersionResolver =
    editions.EngineVersionResolver(editionProvider)

  /** Resolves a raw edition, loading its parents from the edition search path.
    *
    * @param edition the edition to resolve
    * @return the resolved edition
    */
  def resolveEdition(
    edition: Editions.RawEdition
  ): Try[Editions.ResolvedEdition] =
    editionResolver.resolve(edition).toTry

  /** Resolves the engine version that should be used based on the provided raw
    * edition configuration.
    *
    * @param edition the edition configuration to base the selected version on;
    *                if it is not specified, it will fallback to the default
    *                engine version
    * @return the resolved engine version
    */
  def resolveEngineVersion(edition: Editions.RawEdition): Try[SemVer] =
    engineVersionResolver.resolveEnsoVersion(edition).toTry
}

object EditionManager {
  def makeEditionProvider(
    distributionManager: DistributionManager,
    languageHome: Option[LanguageHome]
  ): EditionProvider = {
    val searchPaths = languageHome.map(_.editions).toList ++
      distributionManager.paths.editionSearchPaths
    new FileSystemEditionProvider(searchPaths)
  }

  def apply(
    distributionManager: DistributionManager,
    languageHome: Option[LanguageHome]
  ): EditionManager = new EditionManager(
    makeEditionProvider(distributionManager, languageHome)
  )
}
