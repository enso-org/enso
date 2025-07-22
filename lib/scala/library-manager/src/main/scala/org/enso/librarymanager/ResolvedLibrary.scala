package org.enso.librarymanager

import org.enso.pkg.PackageManager
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
) {
  override def toString() = toString(false)

  def toString(aotCheck: Boolean) = {
    if (aotCheck) {
      val aotReady = PackageManager.Default
        .loadPackage(root.location.toFile)
        .toOption
        .map(_.isAotReady())
        .getOrElse(false)
      s"${name} @ ${version} is ${if (!aotReady) "not" else ""} AOT ready"
    } else {
      s"${name} @ ${version}"
    }
  }
}
object ResolvedLibrary {

  /** Extension methods of [[ResolvedLibrary]]. */
  implicit class ResolvedLibraryMethods(val resolvedLibrary: ResolvedLibrary)
      extends AnyVal {

    /** Provides read methods to access the library files. */
    def getReadAccess: LibraryReadAccess =
      new FilesystemLibraryReadAccess(resolvedLibrary.root)
  }

}
