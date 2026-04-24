import org.yaml.snakeyaml.Yaml
import sbt._

object Editions {

  private val extension = ".yaml"

  /** Generates a base edition file for the engine release that contains the
    * Standard library and is associated with the current Enso version.
    * @return Parsed edition configuration.
    */
  def writeEditionConfig(
    editionsRoot: File,
    editionTemplate: File,
    ensoVersion: String,
    editionName: String,
    libraryVersion: String,
    log: Logger
  ): EditionTemplate = {
    if (!editionTemplate.exists()) {
      log.error(
        s"Edition template file [${editionTemplate.getAbsolutePath}] does " +
        s"not exist. Skipping edition generation."
      )
    }
    IO.createDirectory(editionsRoot)
    val edition = editionsRoot / (editionName + extension)

    for (file <- IO.listFiles(editionsRoot)) {
      if (file.getName != edition.getName) {
        IO.delete(file)
        log.warn(s"Removed spurious file in editions directory: $file")
      }
    }

    val templateContent = IO.read(editionTemplate)
    val comment =
      """
        |# This file was generated automatically by `project/Editions.scala`.
        |# Do not edit it directly.
        |""".stripMargin
    val editionConfigContent = {
      val replaced = templateContent
        .replaceAll("\\{\\{ENGINE_VERSION}}", ensoVersion)
        .replaceAll("\\{\\{LIBS_VERSION}}", libraryVersion)
      val hasUnreplacedVars = replaced.contains("{{")
      if (hasUnreplacedVars) {
        log.error(
          s"Not all template variables were replaced in edition template. " +
          "i.e., there is still some unreplaced template variable in " +
          s"[${editionTemplate.getAbsolutePath}]."
        )
      }
      comment + System.lineSeparator() + replaced
    }

    val currentContent = if (edition.exists()) Some(IO.read(edition)) else None
    if (currentContent.contains(editionConfigContent)) {
      log.debug(s"Edition config [$edition] is already up-to-date.")
    } else {
      IO.write(edition, editionConfigContent)
      log.info(s"Written edition config to [$edition].")
    }
    parseEditionConfig(editionConfigContent, log)
  }

  /** Parses the edition configuration from the template file content.
    */
  private def parseEditionConfig(
    yamlContent: String,
    log: Logger
  ): EditionTemplate = {
    import scala.collection.JavaConverters.*
    try {
      val yaml = new Yaml()
      val obj  = yaml.loadAs(yamlContent, classOf[java.util.Map[String, Object]])
      val reposMap = obj
        .get("repositories")
        .asInstanceOf[java.util.List[java.util.Map[String, String]]]
      val libsMap = obj
        .get("libraries")
        .asInstanceOf[java.util.List[java.util.Map[String, String]]]
      val repos = reposMap.asScala.map { repoMap =>
        Repository(
          repoMap.get("name"),
          repoMap.get("url")
        )
      }
      val libs = libsMap.asScala.map { libMap =>
        Library(
          libMap.get("name"),
          libMap.get("repository"),
          libMap.get("version")
        )
      }
      EditionTemplate(
        repositories = repos.toSeq,
        libraries    = libs.toSeq
      )
    } catch {
      case t: Throwable =>
        log.error(s"Failed to parse edition config YAML: ${t.getMessage}")
        t.printStackTrace(System.err)
        EditionTemplate(Seq.empty, Seq.empty)
    }
  }

  /** Model of <a href="https://github.com/enso-org/enso/blob/bbd03ed9508f0e86da5869f3a14d605dde489974/distribution/edition.template.yaml">edition.template.yaml</a> file
    */
  case class EditionTemplate(
    private val repositories: Seq[Repository],
    private val libraries: Seq[Library]
  ) {

    /** A library that should be uploaded is one that is hosted in a "jar:" repository.
      * See <a href="https://github.com/enso-org/enso/blob/2873e4d654fa836808afce40a28df44d07336177/docs/libraries/sharing.md">docs/libraries/sharing.md</a> for details.
      * @return
      */
    def libsToUpload(): Seq[String] = {
      val zipRepos = repositories.filter { _.url.startsWith("jar:") }
      val libs = libraries.filter { lib =>
        zipRepos.exists { repo =>
          repo.name == lib.repository
        }
      }
      libs.map { _.name }
    }
  }

  case class Repository(
    name: String,
    url: String
  )

  case class Library(
    name: String,
    repository: String,
    version: String
  )
}
