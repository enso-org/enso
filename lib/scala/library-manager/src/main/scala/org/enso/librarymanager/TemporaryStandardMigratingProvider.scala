package org.enso.librarymanager
import org.enso.editions.LibraryName

import java.nio.file.Path
import scala.util.Try

/** This is a temporary [[LibraryProvider]] until the Standard library is
  * shipped from the Marketplace.
  *
  * TODO [RW] may need to figure out how to bundle the stdlib
  *
  * Also how to handle generating editions for nightly
  */
class TemporaryStandardMigratingProvider(fallback: LibraryProvider)
    extends LibraryProvider {
  override def findLibrary(name: LibraryName): Try[Path] = if (
    name.prefix == "Standard"
  ) stdlibPathFor(name.name)
  else fallback.findLibrary(name)

  private def stdlibPathFor(str: String): Try[Path] = ???
}
