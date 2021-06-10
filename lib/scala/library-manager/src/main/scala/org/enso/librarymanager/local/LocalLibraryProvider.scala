package org.enso.librarymanager.local

import org.enso.editions.LibraryName

import java.nio.file.Path

trait LocalLibraryProvider {
  def findLibrary(libraryName: LibraryName): Option[Path]
}
