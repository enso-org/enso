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
  PublishedLibraryProvider
}

/** A helper class for loading libraries.
  *
  * @param localLibraryProvider     provider of local (unpublished) libraries
  * @param publishedLibraryProvider provider of published libraries
  * @param edition                  the edition used in the project
  * @param preferLocalLibraries     project setting whether to use local
  *                                 libraries
  */
class DefaultLibraryProvider private (
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

      case Right(LibraryVersion.Local) =>
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

      case Right(version @ LibraryVersion.Published(semver, repository)) =>
        publishedLibraryProvider
          .findLibrary(libraryName, semver, repository)
          .map(ResolvedLibrary(libraryName, version, _))
          .toEither
          .left
          .map(ResolvingLibraryProvider.Error.DownloadFailed)
    }
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
    edition: Editions.ResolvedEdition,
    preferLocalLibraries: Boolean
  ): ResolvingLibraryProvider = {
    val locations = LibraryLocations.resolve(distributionManager, languageHome)
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

    new DefaultLibraryProvider(
      localLibraryProvider,
      publishedLibraryProvider,
      edition,
      preferLocalLibraries
    )
  }
}
