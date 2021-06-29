package org.enso.librarymanager.published

import nl.gn0s1s.bump.SemVer
import org.enso.distribution.DistributionManager
import org.enso.editions.{Editions, LibraryName, LibraryVersion}
import org.enso.librarymanager.LibraryResolutionResult

import java.nio.file.Path
import scala.annotation.nowarn
import scala.util.{Failure, Try}

/** A default implementation of [[PublishedLibraryProvider]]. */
class DefaultPublishedLibraryProvider(
  @nowarn("msg=never used") distributionManager: DistributionManager
) extends PublishedLibraryProvider {

  // TODO [RW] This is just a stub and will be properly implemented in #1772.

  /** @inheritdoc */
  override def findLibrary(
    libraryName: LibraryName,
    version: SemVer,
    recommendedRepository: Editions.Repository,
    dependencyResolver: LibraryName => Option[LibraryVersion]
  ): Try[Path] = Failure(
    new NotImplementedError("TODO library management is not yet implemented")
  )
}
