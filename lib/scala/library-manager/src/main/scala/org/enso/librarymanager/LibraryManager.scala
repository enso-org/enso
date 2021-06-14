package org.enso.librarymanager

import org.enso.distribution.DistributionManager
import org.enso.editions.{Editions, LibraryName}
import org.enso.librarymanager.local.LocalLibraryProvider
import org.enso.librarymanager.published.{
  DefaultLibraryProvider,
  LibraryProvider
}

case class LibraryManager(distributionManager: DistributionManager) {
  val localLibraryProvider = LocalLibraryProvider.make(distributionManager)
  val resolver             = LibraryResolver(localLibraryProvider)
  val publishedLibraryProvider: LibraryProvider =
    new DefaultLibraryProvider(distributionManager)

  def findLibrary(
    libraryName: LibraryName,
    edition: Editions.ResolvedEdition,
    preferLocalLibraries: Boolean
  ): LibraryResolutionResult = {
    val resolvedVersion = resolver
      .resolveLibraryVersion(libraryName, edition, preferLocalLibraries)
    resolvedVersion match {
      case Left(error) =>
        LibraryResolutionResult.ResolutionFailure(error)
      case Right(LibraryVersion.Local) =>
        localLibraryProvider
          .findLibrary(libraryName)
          .map(LibraryResolutionResult.ResolvedImmediately)
          .getOrElse {
            LibraryResolutionResult.ResolutionFailure(
              LibraryResolutionError(
                s"Edition configuration forces to use the local version, but " +
                s"the `$libraryName` library is not present among local " +
                s"libraries."
              )
            )
          }
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
