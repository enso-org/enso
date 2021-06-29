package org.enso.librarymanager

import org.enso.distribution.DistributionManager
import org.enso.editions.{Editions, LibraryName, LibraryVersion}
import org.enso.librarymanager.local.LocalLibraryProvider
import org.enso.librarymanager.published.{
  DefaultPublishedLibraryProvider,
  PublishedLibraryProvider
}

import java.nio.file.Path
import scala.util.{Failure, Success, Try}

/** A helper class for loading libraries. */
case class DefaultLibraryProvider(
  distributionManager: DistributionManager,
  edition: Editions.ResolvedEdition,
  preferLocalLibraries: Boolean
) {
  private val localLibraryProvider =
    LocalLibraryProvider.make(distributionManager)
  private val resolver = LibraryResolver(localLibraryProvider)
  private val publishedLibraryProvider: PublishedLibraryProvider =
    new DefaultPublishedLibraryProvider(distributionManager)

  /** Resolves the library version that should be used based on the
    * configuration and returns its location on the filesystem.
    *
    * If the library is not available, this operation may download it.
    */
  def findLibrary(
    libraryName: LibraryName
  ): Try[Path] = {
    val resolvedVersion = resolver
      .resolveLibraryVersion(libraryName, edition, preferLocalLibraries)
    resolvedVersion match {
      case Left(error) =>
        Failure(error)
      case Right(LibraryVersion.Local) =>
        localLibraryProvider
          .findLibrary(libraryName)
          .toRight {
            LibraryResolutionError(
              s"Edition configuration forces to use the local version, but " +
              s"the `$libraryName` library is not present among local " +
              s"libraries."
            )
          }
          .toTry
      case Right(LibraryVersion.Published(version, repository)) =>
        val dependencyResolver = { name: LibraryName =>
          resolver
            .resolveLibraryVersion(name, edition, preferLocalLibraries)
            .toOption
        }
        publishedLibraryProvider.findLibrary(
          libraryName,
          version,
          repository,
          dependencyResolver
        )
    }
  }
}
