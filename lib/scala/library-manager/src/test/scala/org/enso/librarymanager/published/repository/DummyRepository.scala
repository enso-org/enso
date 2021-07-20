package org.enso.librarymanager.published.repository

import nl.gn0s1s.bump.SemVer
import org.enso.distribution.FileSystem
import org.enso.editions.Editions.RawEdition
import org.enso.editions.{Editions, LibraryName}
import org.enso.pkg.{Package, PackageManager}

import java.io.File
import java.nio.file.{Files, Path}

abstract class DummyRepository {

  case class DummyLibrary(
    libraryName: LibraryName,
    version: SemVer,
    mainContent: String
  )

  def repoName: String = "test_repo"

  def libraries: Seq[DummyLibrary]

  def createRepository(root: Path): Unit = {
    for (lib <- libraries) {
      val libraryRoot = root
        .resolve("libraries")
        .resolve(lib.libraryName.namespace)
        .resolve(lib.libraryName.name)
        .resolve(lib.version.toString)
      Files.createDirectories(libraryRoot)
      createLibraryProject(libraryRoot, lib)
      val files = Seq(
        ArchiveWriter.TextFile("src/Main.enso", lib.mainContent)
      )
      ArchiveWriter.writeTarArchive(libraryRoot.resolve("main.tgz"), files)
      createManifest(libraryRoot)
    }
  }

  private def createLibraryProject(
    path: Path,
    lib: DummyLibrary
  ): Package[File] = {
    val pkg = PackageManager.Default.create(
      path.toFile,
      name      = lib.libraryName.name,
      namespace = lib.libraryName.namespace,
      version   = lib.version.toString()
    )
    pkg.save().get
    pkg
  }

  private def createManifest(path: Path): Unit = {
    FileSystem.writeTextFile(
      path.resolve("manifest.yaml"),
      s"""archives:
         | - main.tgz
         |""".stripMargin
    )
  }

  def createEdition(repoUrl: String): RawEdition = {
    Editions.Raw.Edition(
      parent       = Some(buildinfo.Info.currentEdition),
      repositories = Map(repoName -> Editions.Repository(repoName, repoUrl)),
      libraries = Map.from(libraries.map { lib =>
        lib.libraryName -> Editions.Raw
          .PublishedLibrary(lib.libraryName, lib.version, repoName)
      })
    )
  }

  def startServer(port: Int, root: Path): Process = {
    val serverDirectory =
      Path.of("tools/simple-library-server").toAbsolutePath.normalize
    (new ProcessBuilder)
      .command(
        "node",
        "main.js",
        "--port",
        port.toString,
        "--root",
        root.toAbsolutePath.normalize.toString
      )
      .directory(serverDirectory.toFile)
      .start()
  }
}
