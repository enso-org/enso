package org.enso.librarymanager.local

import com.typesafe.scalalogging.Logger
import org.enso.distribution.DistributionManager
import org.enso.editions.LibraryName
import org.enso.distribution.FileSystem.PathSyntax
import org.enso.logger.masking.MaskedPath

import java.nio.file.{Files, Path}
import scala.annotation.tailrec

/** A default implementation of [[LocalLibraryProvider]]. */
class DefaultLocalLibraryProvider(distributionManager: DistributionManager)
    extends LocalLibraryProvider {

  private val logger = Logger[DefaultLocalLibraryProvider]

  /** @inheritdoc */
  override def findLibrary(libraryName: LibraryName): Option[Path] =
    findLibraryHelper(
      libraryName,
      distributionManager.paths.localLibrariesSearchPaths.toList
    )

  /** Searches through the available library paths, checking if any one of them contains the requested library.
    *
    * The first path on the list takes precedence.
    */
  @tailrec
  private def findLibraryHelper(
    libraryName: LibraryName,
    searchPaths: List[Path]
  ): Option[Path] = searchPaths match {
    case head :: tail =>
      val potentialPath = head / libraryName.prefix / libraryName.name
      if (Files.exists(potentialPath) && Files.isDirectory(potentialPath)) {
        logger.trace(
          s"Found a local $libraryName at " +
          s"[${MaskedPath(potentialPath).applyMasking()}]."
        )
        Some(potentialPath)
      } else {
        logger.trace(
          s"Local library $libraryName not found at " +
          s"[${MaskedPath(potentialPath).applyMasking()}]."
        )
        findLibraryHelper(libraryName, tail)
      }
    case Nil => None
  }
}
