package org.enso.interpreter.test.instrument

import nl.gn0s1s.bump.SemVer
import org.enso.editions.{Editions, LibraryName}

object TestEdition {

  val stdlibLibraries: Seq[LibraryName] = Seq(
    LibraryName("Standard", "Base"),
    LibraryName("Standard", "Test"),
    LibraryName("Standard", "Table"),
    LibraryName("Standard", "Database"),
    LibraryName("Standard", "Image"),
    LibraryName("Standard", "Geo"),
    LibraryName("Standard", "Visualization"),
    LibraryName("Standard", "Examples"),
    LibraryName("Standard", "Searcher")
  )

  val testLibraryVersion: SemVer = SemVer(0, 0, 0, Some("dev"))

  val empty: Editions.RawEdition =
    Editions.Raw.Edition(
      engineVersion = Some(SemVer(buildinfo.Info.ensoVersion).get),
      repositories =
        Map("main" -> Editions.Repository("main", "http://example.com/")),
      libraries = Map()
    )

  val stdlib: Editions.RawEdition =
    mkEdition(stdlibLibraries)

  private def mkEdition(libraries: Seq[LibraryName]): Editions.RawEdition =
    empty.copy(
      libraries = Map.from {
        libraries.map { name =>
          (
            name,
            Editions.Raw.PublishedLibrary(name, testLibraryVersion, "main")
          )
        }
      }
    )
}
