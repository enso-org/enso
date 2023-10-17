package org.enso.librarymanager.published.repository
import org.enso.editions.Editions.RawEdition

import java.nio.file.Path

class EmptyRepository(toolsRootDirectory: Path)
    extends DummyRepository(toolsRootDirectory) {
  override def libraries: Seq[DummyLibrary]        = Seq.empty
  override def editions: Seq[(String, RawEdition)] = Seq.empty
}
