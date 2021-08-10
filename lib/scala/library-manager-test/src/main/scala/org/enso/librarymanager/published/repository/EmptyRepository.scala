package org.enso.librarymanager.published.repository
import org.enso.editions.Editions.RawEdition

object EmptyRepository extends DummyRepository {
  override def libraries: Seq[EmptyRepository.DummyLibrary] = Seq.empty
  override def editions: Seq[(String, RawEdition)]          = Seq.empty
}
