package org.enso.librarymanager.resolved

import java.nio.file.Path

/** The path to the library on a filesystem.
  *
  * @param location the library location on a filesystem
  */
case class LibraryRoot(location: Path)
object LibraryRoot {

  /** Extension methods of [[LibraryRoot]]. */
  implicit class LibraryRootMethods(val libraryPath: LibraryRoot)
      extends AnyVal {

    /** Get the read access to the library files. */
    def getReadAccess: LibraryReadAccess =
      new FilesystemLibraryReadAccess(libraryPath)
  }

  /** Syntax allowing to write nested paths in a more readable and concise way.
    */
  implicit class LibraryRootSyntax(val libraryRoot: LibraryRoot)
      extends AnyVal {
    def /(other: String): Path = libraryRoot.location.resolve(other)
    def /(other: Path): Path   = libraryRoot.location.resolve(other)
  }
}
