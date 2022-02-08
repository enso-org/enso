package org.enso.interpreter.test.instrument

import nl.gn0s1s.bump.SemVer
import org.enso.editions.{Editions, LibraryName}

object TestEdition {
  val librariesRequiredByTests: Seq[LibraryName] = Seq(
    LibraryName("Standard", "Base")
  )

  val testLibraryVersion: SemVer = SemVer(0, 0, 0, Some("dev"))

  val edition: Editions.RawEdition =
    Editions.Raw.Edition(
      engineVersion = Some(SemVer(buildinfo.Info.ensoVersion).get),
      repositories =
        Map("main" -> Editions.Repository("main", "http://example.com/")),
      libraries = Map.from {
        librariesRequiredByTests.map { name =>
          (
            name,
            Editions.Raw.PublishedLibrary(name, testLibraryVersion, "main")
          )
        }
      }
    )
}
