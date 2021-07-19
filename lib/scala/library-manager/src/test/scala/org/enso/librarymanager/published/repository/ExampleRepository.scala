package org.enso.librarymanager.published.repository

import nl.gn0s1s.bump.SemVer
import org.enso.editions.LibraryName

class ExampleRepository extends DummyRepository {
  val testLib = DummyLibrary(
    LibraryName("Foo", "Bar"),
    SemVer(1, 0, 0),
    """baz = 42
      |
      |quux = "foobar"
      |""".stripMargin
  )

  override def libraries: Seq[DummyLibrary] = Seq(testLib)
}
