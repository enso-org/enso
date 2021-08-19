package org.enso.librarymanager.published

import nl.gn0s1s.bump.SemVer
import org.enso.editions.LibraryName
import org.enso.librarymanager.LibraryLocations
import org.enso.librarymanager.published.bundles.LocalReadOnlyRepository

import java.nio.file.Path

/** An interface that allows to check if a given published library version is
  * cached.
  */
trait PublishedLibraryCache {

  /** Checks if the library at the specific version is already available in the
    * caches.
    */
  def isLibraryCached(libraryName: LibraryName, version: SemVer): Boolean
}

object PublishedLibraryCache {

  /** Creates a read only [[PublishedLibraryCache]] which can be used to check
    * which libraries are already cached.
    */
  def makeReadOnlyCache(cacheLocations: List[Path]): PublishedLibraryCache =
    new CachedLibraryProvider(
      cacheLocations.map(new LocalReadOnlyRepository(_))
    )

  /** Creates a read only [[PublishedLibraryCache]] which can be used to check
    * which libraries are already cached.
    */
  def makeReadOnlyCache(locations: LibraryLocations): PublishedLibraryCache =
    makeReadOnlyCache(
      locations.primaryCacheRoot :: locations.additionalCacheRoots
    )
}
