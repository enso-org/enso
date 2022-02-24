package org.enso.librarymanager.published

import nl.gn0s1s.bump.SemVer
import org.enso.editions.LibraryName
import org.enso.librarymanager.published.cache.ReadOnlyLibraryCache
import org.enso.librarymanager.resolved.LibraryRoot

import scala.annotation.tailrec

/** A [[PublishedLibraryCache]] that just provides libraries which are
  * already available in the cache.
  */
class CachedLibraryProvider(caches: List[ReadOnlyLibraryCache])
    extends PublishedLibraryCache {

  @tailrec
  private def findCachedHelper(
    libraryName: LibraryName,
    version: SemVer,
    caches: List[ReadOnlyLibraryCache]
  ): Option[LibraryRoot] = caches match {
    case head :: tail =>
      head.findCachedLibrary(libraryName, version) match {
        case Some(found) => Some(found)
        case None        => findCachedHelper(libraryName, version, tail)
      }
    case Nil => None
  }

  /** Looks for the library in the known caches. */
  override def findCachedLibrary(
    libraryName: LibraryName,
    version: SemVer
  ): Option[LibraryRoot] =
    findCachedHelper(libraryName, version, caches)

  /** @inheritdoc */
  override def isLibraryCached(
    libraryName: LibraryName,
    version: SemVer
  ): Boolean = findCachedLibrary(libraryName, version).isDefined
}
