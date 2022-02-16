package org.enso.librarymanager.published

import com.typesafe.scalalogging.Logger
import nl.gn0s1s.bump.SemVer
import org.enso.editions.{Editions, LibraryName}
import org.enso.librarymanager.published.cache.{
  LibraryCache,
  ReadOnlyLibraryCache
}
import org.enso.librarymanager.resolved.LibraryRoot

import scala.util.{Success, Try}

/** A default implementation of [[PublishedLibraryProvider]] which uses one
  * primary cache to which it will download missing packages and auxiliary
  * read-only caches which may provide additional libraries.
  */
class DefaultPublishedLibraryProvider(
  primaryCache: LibraryCache,
  auxiliaryCaches: List[ReadOnlyLibraryCache]
) extends CachedLibraryProvider(caches = primaryCache :: auxiliaryCaches)
    with PublishedLibraryProvider {
  private val logger = Logger[DefaultPublishedLibraryProvider]

  /** @inheritdoc */
  override def findLibrary(
    libraryName: LibraryName,
    version: SemVer,
    recommendedRepository: Editions.Repository
  ): Try[LibraryRoot] = {
    val cachedLibrary = findCachedLibrary(libraryName, version)
    cachedLibrary.map(Success(_)).getOrElse {
      logger.trace(
        s"$libraryName was not found in any caches, it will need to be " +
        s"downloaded."
      )
      primaryCache
        .findOrInstallLibrary(libraryName, version, recommendedRepository)
    }
  }
}
