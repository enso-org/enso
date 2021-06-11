package org.enso.librarymanager.published

import nl.gn0s1s.bump.SemVer
import org.enso.distribution.DistributionManager
import org.enso.editions.{Editions, LibraryName}
import org.enso.librarymanager.{LibraryResolutionResult, LibraryVersion}

import scala.annotation.nowarn

class DefaultLibraryProvider(
  @nowarn("msg=never used") distributionManager: DistributionManager
) extends LibraryProvider {
  override def findLibrary(
    libraryName: LibraryName,
    version: SemVer,
    recommendedRepository: Editions.Repository,
    dependencyResolver: LibraryName => Option[LibraryVersion]
  ): LibraryResolutionResult = throw new NotImplementedError(
    "TODO library management is not yet implemented"
  )
}
