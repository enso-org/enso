package org.enso.editions.updater

import nl.gn0s1s.bump.SemVer
import org.enso.distribution.config.GlobalConfigurationManager
import org.enso.distribution.{DistributionManager, LanguageHome}
import org.enso.editions
import org.enso.editions.{EditionResolver, Editions}

import java.nio.file.Path
import scala.util.Try

/** A helper class for resolving editions. */
class EditionManager private (editionProvider: UpdatingEditionProvider) {
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

  /** Find all editions available in the [[searchPaths]], performing an update
    * if asked to.
    */
  def findAllAvailableEditions(update: Boolean): Seq[String] =
    editionProvider.findAvailableEditions(update)
}

object EditionManager {

  /** Create an [[EditionProvider]] that can locate editions from the
    * distribution and the language home.
    */
  def makeEditionProvider(
    distributionManager: DistributionManager,
    languageHome: Option[LanguageHome]
  ): UpdatingEditionProvider = {
    val config = new GlobalConfigurationManager(distributionManager).getConfig
    new UpdatingEditionProvider(
      getSearchPaths(distributionManager, languageHome),
      distributionManager.paths.cachedEditions,
      config.editionProviders
    )
  }

  /** Get search paths associated with the distribution and language home. */
  private def getSearchPaths(
    distributionManager: DistributionManager,
    languageHome: Option[LanguageHome]
  ): List[Path] = {
    val paths = languageHome.map(_.editions).toList ++
      distributionManager.paths.editionSearchPaths
    paths.distinct
  }

  /** Create an [[EditionManager]] that can locate editions from the
    * distribution and the language home.
    */
  def apply(
    distributionManager: DistributionManager,
    languageHome: Option[LanguageHome] = None
  ): EditionManager = new EditionManager(
    makeEditionProvider(distributionManager, languageHome)
  )
}
