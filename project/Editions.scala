import sbt._

object Editions {

  private val extension = ".yaml"

  /** Generates a base edition file for the engine release that contains the
    * Standard library and is associated with the current Enso version.
    */
  def writeEditionConfig(
    editionsRoot: File,
    editionTemplate: File,
    ensoVersion: String,
    editionName: String,
    libraryVersion: String,
    log: Logger
  ): Unit = {
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
      val allVarsReplaced = !replaced.contains("{{")
      if (!allVarsReplaced) {
        log.error(
          s"Not all template variables were replaced in edition template " +
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
  }
}
