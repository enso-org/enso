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
    "Standard.Image",
    "Standard.Geo",
    "Standard.Visualization",
    "Standard.Examples"
  )

  case class ContribLibrary(name: String, version: String)

  /** A list of additional libraries from external sources that are published in
    * the main repository and should be available in the default edition.
    */
  val contribLibraries: Seq[ContribLibrary] = Seq()

  private val editionsRoot = file("distribution") / "editions"
  private val extension    = ".yaml"

  /** Generates a base edition file for the engine release that contains the
    * Standard library and is associated with the current Enso version.
    */
  def writeEditionConfig(
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

    if (!edition.exists()) {
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
           |    url: n/a # Library repository is still a work in progress.
           |libraries:
           |${librariesConfigs.mkString("\n")}
           |""".stripMargin
      IO.write(edition, editionConfig)
      log.info(s"Written edition config to $edition")
    } else {
      log.debug(
        "The edition file did already exist, not regenerating. " +
        "Clean to force a rebuild."
      )
    }
  }
}
