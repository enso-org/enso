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
    edition: Editions.ResolvedEdition
  ): LibraryResolutionResult = {
    resolver.resolveLibraryVersion(libraryName, edition) match {
      case Left(error) =>
        LibraryResolutionResult.ResolutionFailure(error)
      case Right(LocalVersion(path)) =>
        LibraryResolutionResult.ResolvedImmediately(path)
      case Right(PublishedVersion(version, recommendedRepository)) =>
        val dependencyResolver = { name: LibraryName =>
          resolver.resolveLibraryVersion(name, edition).toOption
        }
        publishedLibraryProvider.findLibrary(
          libraryName,
          version,
          recommendedRepository,
          dependencyResolver
        )
    }

  }
}
