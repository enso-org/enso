import sbt._

object Editions {

  /** List of libraries that are shipped with the engine and reside in the
    * engine repository.
    *
    * They currently all share the version number.
    */
  val standardLibraries: Seq[String] = Seq(
    "Standard.Base",
    "Standard.Test",
    "Standard.Table",
    "Standard.Database",
    "Standard.AWS",
    "Standard.Image",
    "Standard.Geo",
    "Standard.Visualization",
    "Standard.Examples",
    "Standard.Searcher"
  )

  case class ContribLibrary(name: String, version: String)

  /** A list of additional libraries from external sources that are published in
    * the main repository and should be available in the default edition.
    */
  val contribLibraries: Seq[ContribLibrary] = Seq()

  /** The URL to the main library repository. */
  val mainLibraryRepositoryUrl = "https://libraries.release.enso.org/libraries"

  private val extension = ".yaml"

  /** Generates a base edition file for the engine release that contains the
    * Standard library and is associated with the current Enso version.
    */
  def writeEditionConfig(
    editionsRoot: File,
    ensoVersion: String,
    editionName: String,
    libraryVersion: String,
    log: Logger
  ): Unit = {
    IO.createDirectory(editionsRoot)
    val edition = editionsRoot / (editionName + extension)

    for (file <- IO.listFiles(editionsRoot)) {
      if (file.getName != edition.getName) {
        IO.delete(file)
        log.warn(s"Removed spurious file in editions directory: $file")
      }
    }

    val editionConfigContent = {
      val standardLibrariesConfigs = standardLibraries.map { libName =>
        s"""  - name: $libName
           |    repository: main
           |    version: $libraryVersion""".stripMargin
      }

      val contribLibrariesConfigs = contribLibraries.map {
        case ContribLibrary(name, version) =>
          s"""  - name: $name
             |    repository: main
             |    version: $version""".stripMargin
      }

      val librariesConfigs = standardLibrariesConfigs ++ contribLibrariesConfigs

      val editionConfig =
        s"""engine-version: $ensoVersion
           |repositories:
           |  - name: main
           |    url: $mainLibraryRepositoryUrl
           |libraries:
           |${librariesConfigs.mkString("\n")}
           |""".stripMargin
      editionConfig
    }

    val currentContent = if (edition.exists()) Some(IO.read(edition)) else None
    if (currentContent.contains(editionConfigContent)) {
      log.debug(s"Edition config [$edition] is already up-to-date.")
    } else {
      IO.write(edition, editionConfigContent)
      log.info(s"Written edition config to [$edition].")
    }
  }
}
