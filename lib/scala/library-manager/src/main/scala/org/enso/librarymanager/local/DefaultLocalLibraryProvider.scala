package org.enso.librarymanager.local

import com.typesafe.scalalogging.Logger
import org.enso.editions.LibraryName
import org.enso.librarymanager.LibraryLocations
import org.enso.librarymanager.resolved.LibraryRoot
import org.enso.logger.masking.MaskedPath
import org.enso.pkg.PackageManager

import java.io.IOException
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
          val firstCandidate = candidates.minBy(_.getFileName.toString)
          logger.warn(
            s"Found multiple libraries with the same name and namespace in a single directory: " +
            s"${candidates.map(_.getFileName.toString).mkString(", ")}. " +
            s"Choosing the first one (${firstCandidate.getFileName})."
          )
          Some(firstCandidate)
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
  ): List[Path] = try {
    if (!Files.isDirectory(librariesPath)) {
      warnAboutMissingSearchPath(librariesPath)
      return Nil
    }

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
  } catch {
    case ex @ (_: IOException | _: RuntimeException) =>
      val maskedPath = MaskedPath(librariesPath).applyMasking()
      logger.warn(
        s"Exception occurred when scanning library path [$maskedPath]: $ex"
      )
      Nil
  }

  /** Finds all currently available local libraries. */
  override def findAvailableLocalLibraries(): List[LibraryName] = {
    val libraries: List[LibraryName] = searchPaths.flatMap { path =>
      try {
        if (!Files.isDirectory(path)) {
          warnAboutMissingSearchPath(path)
          Nil
        } else {
          val subdirectories =
            Files.list(path).filter(Files.isDirectory(_)).toList.asScala
          subdirectories
            .map { potentialPath =>
              val pkg = PackageManager.Default.loadPackage(
                potentialPath.toFile
              )
              pkg.map(_.libraryName)
            }
            .collect { case Success(name) => name }
        }
      } catch {
        case ex @ (_: IOException | _: RuntimeException) =>
          val maskedPath = MaskedPath(path).applyMasking()
          logger.warn(
            s"Exception occurred when scanning library path [$maskedPath]: $ex"
          )
          Nil
      }
    }
    libraries.distinct
  }

  private def warnAboutMissingSearchPath(path: Path): Unit = {
    val exists = Files.exists(path)
    val suffix = if (exists) "is not a directory" else "does not exist"
    val warning =
      s"Local library search path [${MaskedPath(path).applyMasking()}] $suffix."
    if (alreadyWarned.get(path).contains(suffix)) {
      // If we already warned about this path, further warnings get degraded to trace level.
      // Only one warning at warning level is emitted.
      logger.trace(warning)
    } else {
      logger.warn(warning)
      alreadyWarned.put(path, suffix)
    }
  }

  private val alreadyWarned =
    scala.collection.concurrent.TrieMap.empty[Path, String]
}

object DefaultLocalLibraryProvider {

  /** Creates a [[DefaultLocalLibraryProvider]] from the [[LibraryLocations]]
    * configuration.
    */
  def make(locations: LibraryLocations): DefaultLocalLibraryProvider =
    new DefaultLocalLibraryProvider(locations.localLibrarySearchPaths)
}
