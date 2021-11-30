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

  /** Tries to locate a cached version of the requested library. */
  def findCachedLibrary(libraryName: LibraryName, version: SemVer): Option[Path]
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
    *
    * This function creates a [[LocalReadOnlyRepository]] at the primary cache
    * location, which, as described in the documentation of that class, is
    * usually not recommended. This situation is however an exception - the
    * [[PublishedLibraryCache]] is only used to check if a library is cached or
    * not - so even if it responds with 'true' regarding a library that is still
    * being installed, any actual access of the library (an attempt to load it)
    * will use a proper synchronized cache instance and so will have to wait
    * until that installation is complete.
    */
  def makeReadOnlyCache(locations: LibraryLocations): PublishedLibraryCache =
    makeReadOnlyCache(
      locations.primaryCacheRoot :: locations.additionalCacheRoots
    )
}
