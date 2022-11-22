package org.enso.librarymanager.published.repository

import nl.gn0s1s.bump.SemVer
import org.enso.cli.OS
import org.enso.distribution.FileSystem
import org.enso.downloader.archive.TarGzWriter
import org.enso.editions.EditionSerialization.editionEncoder
import org.enso.editions.Editions.RawEdition
import org.enso.editions.{Editions, LibraryName}
import org.enso.pkg.{Package, PackageManager}
import org.enso.testkit.process.WrappedProcess
import org.enso.yaml.YamlHelper

import java.io.File
import java.nio.file.{Files, Path}
import scala.util.Using
import scala.util.control.NonFatal

/** A helper class managing a library repository for testing purposes. */
abstract class DummyRepository {

  /** A library used for testing.
    *
    * @param libraryName name of the library
    * @param version version of the library
    * @param mainContent contents of the `Main.enso` file
    * @param dependencies libraries that this library directly depends on, to be
    *                     included in the manifest
    */
  case class DummyLibrary(
    libraryName: LibraryName,
    version: SemVer,
    mainContent: String,
    dependencies: Seq[LibraryName] = Seq.empty
  )

  /** Name of the repository, as it will be indicated in the generated edition.
    */
  def repoName: String = "test_repo"

  /** Sequence of libraries to create in the repository and include in the
    * edition.
    */
  def libraries: Seq[DummyLibrary]

  /** Sequence of downloadable editions and their names to create in the repository. */
  def editions: Seq[(String, Editions.RawEdition)]

  /** Creates a directory structure for the repository at the given root and
    * populates it with [[libraries]] and [[editions]].
    */
  def createRepository(root: Path): Unit = {
    for (lib <- libraries) {
      val libraryRoot = root
        .resolve("libraries")
        .resolve(lib.libraryName.namespace)
        .resolve(lib.libraryName.name)
        .resolve(lib.version.toString)
      Files.createDirectories(libraryRoot)
      createLibraryProject(libraryRoot, lib)

      TarGzWriter
        .createArchive(libraryRoot.resolve("main.tgz")) { writer =>
          writer.writeTextFile("src/Main.enso", lib.mainContent)
        }
        .get

      createManifest(libraryRoot, lib)
    }

    val editionsRoot = root.resolve("editions")
    Files.createDirectories(editionsRoot)
    for ((name, edition) <- editions) {
      FileSystem.writeTextFile(
        editionsRoot.resolve(name + ".yaml"),
        YamlHelper.toYaml(edition)
      )
    }

    if (editions.nonEmpty) {
      val content = "editions:\n" + editions.map { case (name, _) =>
        s"""- "$name"\n"""
      }.mkString
      FileSystem.writeTextFile(editionsRoot.resolve("manifest.yaml"), content)
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

  private def createManifest(path: Path, lib: DummyLibrary): Unit = {
    val dependencies =
      if (lib.dependencies.isEmpty) ""
      else
        lib.dependencies
          .map(name => s""" - "${name.qualifiedName}"""")
          .mkString("dependencies:\n", "\n", "\n")

    val content =
      s"""archives:
         | - main.tgz
         |""".stripMargin + dependencies

    FileSystem.writeTextFile(
      path.resolve("manifest.yaml"),
      content
    )
  }

  /** Creates an edition which contains libraries defined in this repository.
    *
    * @param repoUrl the URL where the repository is going to be accessible; the
    *                URL should include the `libraries` prefix
    */
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

  private def commandPrefix: Seq[String] =
    if (OS.isWindows) Seq("cmd.exe", "/c") else Seq.empty

  private def npmCommand: String  = if (OS.isWindows) "npm.cmd" else "npm"
  private def nodeCommand: String = if (OS.isWindows) "node.exe" else "node"

  case class Server(process: WrappedProcess) extends AutoCloseable {
    override def close(): Unit = {
      process.kill(killDescendants    = true)
      process.join(waitForDescendants = true)
    }
  }

  /** Starts the server and runs the action while it is running, and shuts it
    * down afterwards.
    *
    * @param port port to listen on
    * @param root root of the library repository, the same as the argument to
    *             [[createRepository]]
    * @param uploads specifies whether to enable uploads in the server
    * @param action the action to perform while the server is running
    */
  def withServer[R](port: Int, root: Path, uploads: Boolean = false)(
    action: => R
  ): R = Using(startServer(port, root, uploads)) { _ =>
    action
  }.get

  /** Starts a server for the library repository.
    *
    * @param port port to listen on
    * @param root root of the library repository, the same as the argument to
    *             [[createRepository]]
    * @param uploads specifies whether to enable uploads in the server
    */
  private def startServer(
    port: Int,
    root: Path,
    uploads: Boolean
  ): Server = DummyRepository.lock.synchronized {
    val serverDirectory =
      Path.of("tools/simple-library-server").toAbsolutePath.normalize

    // We can ommit installation step on CI because there is a separate step
    // executing `npm install` command before the tests.
    if (!DummyRepository.isCI) {
      val preinstallCommand =
        commandPrefix ++ Seq(npmCommand, "install")
      val preinstallExitCode = new ProcessBuilder()
        .command(preinstallCommand: _*)
        .directory(serverDirectory.toFile)
        .inheritIO()
        .start()
        .waitFor()

      if (preinstallExitCode != 0)
        throw new RuntimeException(
          s"Failed to preinstall the Library Repository Server dependencies: " +
          s"npm exited with code $preinstallCommand."
        )
    }

    val uploadsArgs = if (uploads) Seq("--upload", "no-auth") else Seq()
    val command = commandPrefix ++ Seq(
      nodeCommand,
      "main.js",
      "--port",
      port.toString,
      "--root",
      root.toAbsolutePath.normalize.toString
    ) ++ uploadsArgs
    val rawProcess = (new ProcessBuilder)
      .command(command: _*)
      .directory(serverDirectory.toFile)
      .start()
    val process = new WrappedProcess(command, rawProcess)
    try {
      process.printIO()
      process.waitForMessage(
        "Serving the repository",
        timeoutSeconds = 15,
        process.StdOut
      )
    } catch {
      case NonFatal(e) =>
        process.kill()
        throw e
    }
    Server(process)
  }
}
object DummyRepository {
  private val lock = new Object

  private def isCI = sys.env.contains("CI")
}
