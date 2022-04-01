package org.enso.interpreter.test.instrument

import nl.gn0s1s.bump.SemVer
import org.enso.editions.{Editions, LibraryName}

import java.io.File

object TestEdition {

  val testLibraryVersion: SemVer = SemVer(0, 0, 0, Some("dev"))

  val empty: Editions.RawEdition =
    Editions.Raw.Edition(
      engineVersion = Some(SemVer(buildinfo.Info.ensoVersion).get),
      repositories =
        Map("main" -> Editions.Repository("main", "http://example.com/")),
      libraries = Map()
    )

  def readStdlib(path: File): Editions.RawEdition =
    mkEdition(readStdlibLibraries(path))

  def readStdlibLibraries(path: File): Seq[LibraryName] =
    for {
      namespace <- path.listFiles.toSeq
      if namespace.isDirectory
      name <- namespace.listFiles
      if name.isDirectory
    } yield LibraryName(namespace.getName, name.getName)

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
