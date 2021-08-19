package org.enso.librarymanager

import com.typesafe.scalalogging.Logger
import org.enso.distribution.{DistributionManager, LanguageHome}
import org.enso.logger.masking.MaskedPath

import java.nio.file.Path

/** Organizes locations which may hold libraries.
  *
  * @param localLibrarySearchPaths search paths of local (unpublished) libraries
  * @param primaryCacheRoot the primary cache, which is the location to which
  *                         new libraries will be downloaded
  * @param additionalCacheRoots additional caches, for example libraries
  *                             bundled with an engine release
  */
case class LibraryLocations(
  localLibrarySearchPaths: List[Path],
  primaryCacheRoot: Path,
  additionalCacheRoots: List[Path]
)

object LibraryLocations {
  private lazy val logger = Logger[LibraryLocations]

  /** Resolves the [[LibraryLocations]] based on the [[DistributionManager]]
    * which provides paths to the distribution and an optional [[LanguageHome]]
    * which can provide paths to libraries bundled with the current language
    * version.
    */
  def resolve(
    distributionManager: DistributionManager,
    languageHome: Option[LanguageHome]
  ): LibraryLocations = {
    val localLibrarySearchPaths =
      distributionManager.paths.localLibrariesSearchPaths.toList
    val cacheRoot = distributionManager.paths.cachedLibraries
    val additionalCacheLocations = {
      val engineBundleRoot = languageHome.map(_.libraries)
      val locations =
        engineBundleRoot.toList ++ distributionManager.auxiliaryLibraryCaches()
      locations.distinct
    }

    def mask(path: Path): String = MaskedPath(path).applyMasking()

    logger.trace(
      s"Local library search paths = ${localLibrarySearchPaths.map(mask)}"
    )
    logger.trace(
      s"Primary library cache = ${mask(cacheRoot)}"
    )
    logger.trace(
      s"Auxiliary (bundled) library caches = " +
      s"${additionalCacheLocations.map(mask)}"
    )

    LibraryLocations(
      localLibrarySearchPaths = localLibrarySearchPaths,
      primaryCacheRoot        = cacheRoot,
      additionalCacheRoots    = additionalCacheLocations
    )
  }
}
