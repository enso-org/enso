package org.enso.librarymanager

import com.typesafe.scalalogging.Logger
import org.enso.cli.task.ProgressReporter
import org.enso.distribution.locking.{LockUserInterface, ResourceManager}
import org.enso.distribution.{
  DistributionManager,
  LanguageHome,
  TemporaryDirectoryManager
}
import org.enso.editions.{Editions, LibraryName, LibraryVersion}
import org.enso.librarymanager.local.{
  DefaultLocalLibraryProvider,
  LocalLibraryProvider
}
import org.enso.librarymanager.published.bundles.LocalReadOnlyRepository
import org.enso.librarymanager.published.cache.DownloadingLibraryCache
import org.enso.librarymanager.published.{
  DefaultPublishedLibraryProvider,
  PublishedLibraryCache,
  PublishedLibraryProvider
}

import java.nio.file.Path

/** A helper class for loading libraries.
  *
  * @param localLibraryProvider     provider of local (unpublished) libraries
  * @param publishedLibraryProvider provider of published libraries
  * @param edition                  the edition used in the project
  * @param preferLocalLibraries     project setting whether to use local
  *                                 libraries
  */
class DefaultLibraryProvider(
  localLibraryProvider: LocalLibraryProvider,
  publishedLibraryProvider: PublishedLibraryProvider,
  edition: Editions.ResolvedEdition,
  preferLocalLibraries: Boolean
) extends ResolvingLibraryProvider {
  private val logger   = Logger[DefaultLibraryProvider]
  private val resolver = LibraryResolver(localLibraryProvider)

  /** Resolves the library version that should be used based on the
    * configuration and returns its location on the filesystem.
    *
    * If the library is not available, this operation may download it.
    */
  override def findLibrary(
    libraryName: LibraryName
  ): Either[ResolvingLibraryProvider.Error, ResolvedLibrary] = {
    val resolvedVersion = resolver
      .resolveLibraryVersion(libraryName, edition, preferLocalLibraries)
    logger.trace(s"Resolved $libraryName to [$resolvedVersion].")

    resolvedVersion match {
      case Left(reason) =>
        Left(ResolvingLibraryProvider.Error.NotResolved(reason))

      case Right(version) =>
        findSpecificLibraryVersion(libraryName, version)
    }
  }

  /** @inheritdoc */
  override def findSpecificLibraryVersion(
    libraryName: LibraryName,
    version: LibraryVersion
  ): Either[ResolvingLibraryProvider.Error, ResolvedLibrary] = version match {
    case LibraryVersion.Local =>
      localLibraryProvider
        .findLibrary(libraryName)
        .map(ResolvedLibrary(libraryName, LibraryVersion.Local, _))
        .toRight {
          ResolvingLibraryProvider.Error.NotResolved(
            LibraryResolutionError(
              s"Edition configuration forces to use the local version, but " +
              s"the `$libraryName` library is not present among local " +
              s"libraries."
            )
          )
        }

    case version @ LibraryVersion.Published(semver, repository) =>
      publishedLibraryProvider
        .findLibrary(libraryName, semver, repository)
        .map(ResolvedLibrary(libraryName, version, _))
        .toEither
        .left
        .map(ResolvingLibraryProvider.Error.DownloadFailed(version, _))
  }
}

object DefaultLibraryProvider {

  /** Creates a [[ResolvingLibraryProvider]] that can download new libraries.
    *
    * @param distributionManager  a distribution manager
    * @param resourceManager      a resource manager
    * @param lockUserInterface    an interface that will handle notifications
    *                             about waiting on locks
    * @param progressReporter     an interface that will handle progress
    *                             notifications
    * @param languageHome         a language home which may contain bundled libraries
    * @param edition              the edition used in the project
    * @param preferLocalLibraries project setting whether to use local libraries
    */
  def make(
    distributionManager: DistributionManager,
    resourceManager: ResourceManager,
    lockUserInterface: LockUserInterface,
    progressReporter: ProgressReporter,
    languageHome: Option[LanguageHome],
    projectRoot: Option[Path],
    edition: Editions.ResolvedEdition,
    preferLocalLibraries: Boolean
  ): ResolvingLibraryProvider = {
    val (localLibraryProvider, publishedLibraryProvider) = makeProviders(
      distributionManager,
      resourceManager,
      lockUserInterface,
      progressReporter,
      languageHome,
      projectRoot
    )

    new DefaultLibraryProvider(
      localLibraryProvider,
      publishedLibraryProvider,
      edition,
      preferLocalLibraries
    )
  }

  /** Creates a pair of local and published library providers. */
  def makeProviders(
    distributionManager: DistributionManager,
    resourceManager: ResourceManager,
    lockUserInterface: LockUserInterface,
    progressReporter: ProgressReporter,
    languageHome: Option[LanguageHome],
    projectRoot: Option[Path]
  ): (
    LocalLibraryProvider,
    PublishedLibraryProvider with PublishedLibraryCache
  ) = {
    val locations =
      LibraryLocations.resolve(distributionManager, languageHome, projectRoot)
    val primaryCache = new DownloadingLibraryCache(
      locations.primaryCacheRoot,
      TemporaryDirectoryManager(distributionManager, resourceManager),
      resourceManager,
      lockUserInterface,
      progressReporter
    )
    val additionalCaches =
      locations.additionalCacheRoots.map(new LocalReadOnlyRepository(_))

    val localLibraryProvider =
      new DefaultLocalLibraryProvider(locations.localLibrarySearchPaths)
    val publishedLibraryProvider =
      new DefaultPublishedLibraryProvider(primaryCache, additionalCaches)
    (localLibraryProvider, publishedLibraryProvider)
  }
}
