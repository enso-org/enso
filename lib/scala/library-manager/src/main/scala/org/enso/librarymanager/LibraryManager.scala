package org.enso.librarymanager

import org.enso.cli.task.TaskProgress
import org.enso.distribution.DistributionManager
import org.enso.editions.{Editions, LibraryName}
import org.enso.librarymanager.local.LocalLibraryProvider
import org.enso.librarymanager.published.LibraryProvider

import java.nio.file.Path
import scala.annotation.nowarn

case class LibraryManager(distributionManager: DistributionManager) {
  val localLibraryProvider = LocalLibraryProvider.make(distributionManager)
  val resolver             = LibraryResolver(localLibraryProvider)
  @nowarn("msg=dead code")
  val publishedLibraryProvider: LibraryProvider = ???

  // TODO I think I don't like this return type
  def findLibrary(
    libraryName: LibraryName,
    edition: Editions.ResolvedEdition
  ): Either[TaskProgress[Path], Path] = {
    resolver.resolveLibraryVersion(libraryName, edition) match {
      case Left(error) =>
        Left(TaskProgress.immediateFailure(error))
      case Right(LocalVersion(path)) =>
        Right(path)
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
