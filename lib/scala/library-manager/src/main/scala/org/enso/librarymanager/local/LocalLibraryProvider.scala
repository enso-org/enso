package org.enso.librarymanager.local

import org.enso.editions.LibraryName

import java.nio.file.Path

/** A provider for local libraries. */
trait LocalLibraryProvider {

  /** Returns the path to a local instance of the requested library, if it is
    * available.
    */
  def findLibrary(libraryName: LibraryName): Option[Path]
}
