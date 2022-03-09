package org.enso.librarymanager

import org.enso.editions.{LibraryName, LibraryVersion}
import org.enso.librarymanager.resolved.{
  FilesystemLibraryReadAccess,
  LibraryReadAccess,
  LibraryRoot
}

/** Represents a resolved library that is located somewhere on the filesystem.
  *
  * @param name the library name
  * @param version the library version
  * @param root the library location on the filesystem
  */
case class ResolvedLibrary(
  name: LibraryName,
  version: LibraryVersion,
  root: LibraryRoot
)
object ResolvedLibrary {

  /** Extension methods of [[ResolvedLibrary]]. */
  implicit class ResolvedLibraryMethods(val resolvedLibrary: ResolvedLibrary)
      extends AnyVal {

    /** Provides read methods to access the library files. */
    def getReadAccess: LibraryReadAccess =
      new FilesystemLibraryReadAccess(resolvedLibrary.root)
  }

}
