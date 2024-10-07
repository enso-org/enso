package org.enso.librarymanager.test.published.repository

import org.enso.editions.Editions.RawEdition
import org.enso.editions.{Editions, LibraryName}
import org.enso.semver.SemVer

import java.nio.file.Path

/** A simple [[DummyRepository]] containing a single library for testing
  * downloads.
  */
class ExampleRepository(serverRepoPath: Path)
    extends DummyRepository(serverRepoPath) {

  /** The library provided by this repository. */
  val testLib: DummyLibrary = DummyLibrary(
    LibraryName("Foo", "Bar"),
    SemVer.of(1, 0, 0),
    """baz = 42
      |
      |quux = "foobar"
      |""".stripMargin
  )

  override def libraries: Seq[DummyLibrary] = Seq(testLib)

  val testlocalEdition: RawEdition =
    Editions.Raw.Edition(engineVersion = Some(SemVer.of(0, 0, 0)))

  override def editions: Seq[(String, RawEdition)] = Seq(
    "testlocal" -> testlocalEdition
  )
}
