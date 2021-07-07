package org.enso.librarymanager

import org.enso.distribution.{DistributionManager, LanguageHome}
import org.enso.editions.{Editions, LibraryName, LibraryVersion}
import org.enso.librarymanager.local.LocalLibraryProvider
import org.enso.librarymanager.published.bundles.LocalReadOnlyRepository
import org.enso.librarymanager.published.cache.NoOpCache
import org.enso.librarymanager.published.{
  DefaultPublishedLibraryProvider,
  PublishedLibraryProvider
}

/** A helper class for loading libraries.
  *
  * @param distributionManager  a distribution manager
  * @param languageHome         a language home which may contain bundled libraries
  * @param edition              the edition used in the project
  * @param preferLocalLibraries project setting whether to use local libraries
  */
class DefaultLibraryProvider(
  distributionManager: DistributionManager,
  languageHome: Option[LanguageHome],
  edition: Editions.ResolvedEdition,
  preferLocalLibraries: Boolean
) extends ResolvingLibraryProvider {
  private val localLibraryProvider =
    LocalLibraryProvider.make(distributionManager)
  private val resolver = LibraryResolver(localLibraryProvider)

  // TODO [RW] actual cache that can download libraries will be implemented in #1772
  private val primaryCache = new NoOpCache

  private val additionalCaches = {
    val engineBundleRoot = languageHome.map(_.libraries)
    val locations =
      engineBundleRoot.toList ++ distributionManager.auxiliaryLibraryCaches()
    locations.map(new LocalReadOnlyRepository(_))
  }

  private val publishedLibraryProvider: PublishedLibraryProvider =
    new DefaultPublishedLibraryProvider(primaryCache, additionalCaches)

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
        val dependencyResolver = { name: LibraryName =>
          resolver
            .resolveLibraryVersion(name, edition, preferLocalLibraries)
            .toOption
        }

        publishedLibraryProvider
          .findLibrary(
            libraryName,
            semver,
            repository,
            dependencyResolver
          )
          .map(ResolvedLibrary(libraryName, version, _))
          .toEither
          .left
          .map(ResolvingLibraryProvider.Error.DownloadFailed)
    }
  }
}
