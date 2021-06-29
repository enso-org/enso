package org.enso.librarymanager.published.cache

import nl.gn0s1s.bump.SemVer
import org.enso.editions.LibraryName

import java.nio.file.Path

/** A read-only cache may contain some pre-defined set of libraries, but it does
  * not necessarily install any additional libraries.
  *
  * An example of a read-only cache are libraries bundled with the engine.
  */
trait ReadOnlyLibraryCache {
  def findCachedLibrary(libraryName: LibraryName, version: SemVer): Option[Path]
}
