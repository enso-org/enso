package org.enso.librarymanager.dependencies

import org.enso.editions.{LibraryName, LibraryVersion}

/** Represents a resolved dependency.
  *
  * @param libraryName name of the library
  * @param version version of the library
  * @param isCached whether the library is already present in one of the caches
  */
case class Dependency(
  libraryName: LibraryName,
  version: LibraryVersion,
  isCached: Boolean
)
