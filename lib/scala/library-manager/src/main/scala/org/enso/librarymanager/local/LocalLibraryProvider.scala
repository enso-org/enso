package org.enso.librarymanager.local

import org.enso.distribution.FileSystem.PathSyntax
import org.enso.editions.LibraryName
import org.enso.librarymanager.resolved.LibraryRoot

import java.nio.file.Path

/** A provider for local libraries. */
trait LocalLibraryProvider {

  /** Find the local library by name.
    *
    * @param libraryName the library name
    * @return the location of the requested library, if it is available.
    */
  def findLibrary(libraryName: LibraryName): Option[LibraryRoot]
}

object LocalLibraryProvider {

  /** Resolve a path to the package root of a particular library located in one
    * of the local library roots.
    */
  def resolveLibraryPath(root: Path, libraryName: LibraryName): Path =
    root / libraryName.namespace / libraryName.name
}
