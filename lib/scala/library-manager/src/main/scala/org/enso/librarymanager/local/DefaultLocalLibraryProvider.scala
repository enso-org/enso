package org.enso.librarymanager.local

import com.typesafe.scalalogging.Logger
import org.enso.editions.LibraryName
import org.enso.librarymanager.LibraryLocations
import org.enso.librarymanager.resolved.LibraryRoot
import org.enso.logger.masking.MaskedPath
import org.enso.pkg.PackageManager

import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.ListHasAsScala
import scala.util.{Failure, Success}

/** A default implementation of [[LocalLibraryProvider]]. */
class DefaultLocalLibraryProvider(searchPaths: List[Path])
    extends LocalLibraryProvider {

  private val logger = Logger[DefaultLocalLibraryProvider]

  /** @inheritdoc */
  override def findLibrary(libraryName: LibraryName): Option[LibraryRoot] = {
    findLibraryHelper(libraryName, searchPaths)
      .map(LibraryRoot(_))
  }

  /** Searches through the available library paths, checking if any one of them
    * contains the requested library.
    *
    * The first path on the list takes precedence.
    */
  @tailrec
  private def findLibraryHelper(
    libraryName: LibraryName,
    searchPaths: List[Path]
  ): Option[Path] = searchPaths match {
    case potentialPath :: tail =>
      val candidates = findCandidates(libraryName, potentialPath)
      if (candidates.isEmpty) {
        logger.trace(
          s"Local library $libraryName not found at " +
          s"[${MaskedPath(potentialPath).applyMasking()}]."
        )
        findLibraryHelper(libraryName, tail)
      } else {
        if (candidates.size > 1) {
          logger.warn(
            s"Found multiple libraries with the same name and namespace in a single directory: " +
            s"${candidates.map(_.getFileName.toString).mkString(", ")}. " +
            s"Choosing the first one."
          )
          Some(candidates.minBy(_.getFileName.toString))
        } else {
          val found = candidates.head
          logger.trace(
            s"Resolved library [$libraryName] at [${MaskedPath(found).applyMasking()}]."
          )
          Some(found)
        }
      }
    case Nil => None
  }

  private def findCandidates(
    libraryName: LibraryName,
    librariesPath: Path
  ): List[Path] = {
    val subdirectories = Files.list(librariesPath).filter(Files.isDirectory(_))
    subdirectories
      .filter { potentialPath =>
        val isGood =
          PackageManager.Default.loadPackage(potentialPath.toFile) match {
            case Failure(exception) =>
              logger.trace(
                s"Failed to load the candidate library package description at [${MaskedPath(potentialPath)
                  .applyMasking()}].",
                exception
              )
              false
            case Success(pkg) => pkg.libraryName == libraryName
          }
        if (isGood) {
          logger.trace(
            s"Found candidate library [$libraryName] at [${MaskedPath(potentialPath).applyMasking()}]."
          )
        }
        isGood
      }
      .toList
      .asScala
      .toList
  }
}

object DefaultLocalLibraryProvider {

  /** Creates a [[DefaultLocalLibraryProvider]] from the [[LibraryLocations]]
    * configuration.
    */
  def make(locations: LibraryLocations): DefaultLocalLibraryProvider =
    new DefaultLocalLibraryProvider(locations.localLibrarySearchPaths)
}
