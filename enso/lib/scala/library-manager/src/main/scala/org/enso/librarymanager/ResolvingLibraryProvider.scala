package org.enso.librarymanager

import org.enso.editions.{LibraryName, LibraryVersion}

/** A helper class for resolving libraries. */
trait ResolvingLibraryProvider {

  /** Resolves which library version should be used and finds its path within
    * local libraries or the cache.
    *
    * If the library is not cached, it attempts to download it.
    *
    * @param name name of the library
    * @return the resolved library containing the resulting version and path
    */
  def findLibrary(
    name: LibraryName
  ): Either[ResolvingLibraryProvider.Error, ResolvedLibrary]
}

object ResolvingLibraryProvider {

  /** Indicates a failure to find a library. */
  sealed trait Error
  object Error {

    /** Indicates that the library could not be resolved in the edition
      * (for example it is not defined in that edition).
      */
    case class NotResolved(details: LibraryResolutionError) extends Error

    /** Indicates that a local library version was requested, but it did not
      * exist on the library path.
      */
    case object RequestedLocalLibraryDoesNotExist extends Error

    /** Indicates that the library version was missing and had to be downloaded,
      * but the download has failed.
      */
    case class DownloadFailed(
      version: LibraryVersion.Published,
      reason: Throwable
    ) extends Error
  }
}
