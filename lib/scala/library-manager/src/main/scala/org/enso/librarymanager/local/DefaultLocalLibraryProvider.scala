package org.enso.librarymanager.local

import org.enso.distribution.DistributionManager
import org.enso.editions.LibraryName
import org.enso.distribution.FileSystem.PathSyntax

import java.nio.file.{Files, Path}

class DefaultLocalLibraryProvider(distributionManager: DistributionManager)
    extends LocalLibraryProvider {
  override def findLibrary(libraryName: LibraryName): Option[Path] =
    findLibraryHelper(
      libraryName,
      distributionManager.paths.localLibrariesSearchPaths.toList
    )

  private def findLibraryHelper(
    libraryName: LibraryName,
    searchPaths: List[Path]
  ): Option[Path] = searchPaths match {
    case head :: tail =>
      val potentialPath = head / libraryName.prefix / libraryName.name
      if (Files.exists(potentialPath) && Files.isDirectory(potentialPath))
        Some(potentialPath)
      else findLibraryHelper(libraryName, tail)
    case Nil => None
  }
}
