package org.enso.librarymanager.resolved

import java.nio.file.Path

/** The path to the library on a filesystem.
  *
  * @param location the library location on a filesystem
  */
case class LibraryPath(location: Path)
object LibraryPath {

  /** Extension methods of [[LibraryPath]]. */
  implicit class LibraryPathMethods(val libraryPath: LibraryPath)
      extends AnyVal {

    /** Get the read access to the library files. */
    def getReadAccess: LibraryReadAccess =
      new FilesystemLibraryReadAccess(libraryPath)
  }

  /** Syntax allowing to write nested paths in a more readable and concise way.
    */
  implicit class LibraryPathSyntax(val libraryPath: LibraryPath)
      extends AnyVal {
    def /(other: String): Path = libraryPath.location.resolve(other)
    def /(other: Path): Path   = libraryPath.location.resolve(other)
  }
}
