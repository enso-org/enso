package org.enso.librarymanager

import org.enso.editions.LibraryName

import java.nio.file.Path
import scala.util.Try

trait LibraryProvider {
  def findLibrary(name: LibraryName): Try[Path]
}
