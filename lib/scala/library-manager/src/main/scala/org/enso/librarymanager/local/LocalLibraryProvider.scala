package org.enso.librarymanager.local

import org.enso.editions.LibraryName
import org.enso.librarymanager.resolved.LibraryRoot

/** A provider for local libraries. */
trait LocalLibraryProvider {

  /** Find the local library by name.
    *
    * @param libraryName the library name
    * @return the location of the requested library, if it is available.
    */
  def findLibrary(libraryName: LibraryName): Option[LibraryRoot]

  /** Finds all currently available local libraries. */
  def findAvailableLocalLibraries(): List[LibraryName]
}
