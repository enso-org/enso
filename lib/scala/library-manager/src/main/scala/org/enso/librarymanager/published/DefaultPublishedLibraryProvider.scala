package org.enso.librarymanager.published

import nl.gn0s1s.bump.SemVer
import org.enso.editions.{Editions, LibraryName, LibraryVersion}
import org.enso.librarymanager.published.cache.{
  LibraryCache,
  ReadOnlyLibraryCache
}

import java.nio.file.Path
import scala.annotation.tailrec
import scala.util.{Success, Try}

/** A default implementation of [[PublishedLibraryProvider]] which uses one
  * primary cache to which it will download missing packages and auxiliary
  * read-only caches which may provide additional libraries.
  */
class DefaultPublishedLibraryProvider(
  primaryCache: LibraryCache,
  auxiliaryCaches: List[ReadOnlyLibraryCache]
) extends PublishedLibraryProvider {

  private val caches: List[ReadOnlyLibraryCache] =
    primaryCache :: auxiliaryCaches

  @tailrec
  private def findCached(
    libraryName: LibraryName,
    version: SemVer,
    caches: List[ReadOnlyLibraryCache]
  ): Option[Path] = caches match {
    case head :: tail =>
      head.findCachedLibrary(libraryName, version) match {
        case Some(found) => Some(found)
        case None        => findCached(libraryName, version, tail)
      }
    case Nil => None
  }

  /** @inheritdoc */
  override def findLibrary(
    libraryName: LibraryName,
    version: SemVer,
    recommendedRepository: Editions.Repository,
    dependencyResolver: LibraryName => Option[LibraryVersion]
  ): Try[Path] = {
    val cached = findCached(libraryName, version, caches)
    cached.map(Success(_)).getOrElse {
      primaryCache.findOrInstallLibrary(
        libraryName,
        version,
        recommendedRepository,
        dependencyResolver
      )
    }
  }
}
