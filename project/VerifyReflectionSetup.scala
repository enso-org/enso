import sbt._

object VerifyReflectionSetup {
  private val zipRequiredClass =
    "org.apache.commons.compress.archivers.zip.X5455_ExtendedTimestamp"
  private val unwantedPointerInfix = "/0x00"

  /** A task that checks if the reflection configuration is set-up properly.
    *
    * It checks if the configuration contains entries for handling ZIP archives
    * and does not contain ephemeral classes.
    */
  def run = Def.task {
    val root = Keys.baseDirectory.value
    val name = Keys.name.value
    val log  = Keys.streams.value.log

    def fail(message: String): Nothing = {
      log.error(message)
      throw new IllegalStateException(message)
    }

    val configPath =
      root / "src" / "main" / "resources" / "META-INF" / "native-image"
    val reflectConfigs = (configPath ** "reflect-config.json").get()
    if (reflectConfigs.isEmpty) {
      fail(s"Could not locate reflect config for $name.")
    }

    val content = reflectConfigs.map(IO.read(_)).mkString("\n")

    if (!content.contains(zipRequiredClass)) {
      fail(
        s"Required classes for ZIP archive handling are not present in " +
        s"reflection config for $name. This may result in ZIP handling not " +
        s"working in native builds. Please add these missing configurations " +
        s"as described in `docs/infrastructure/native-image.md` or remove " +
        s"this check if ZIP support is no longer needed for this project."
      )
    }

    if (content.contains(unwantedPointerInfix)) {
      fail(
        s"Reflection configuration for $name seems to contain unnecessary " +
        s"ephemeral classes. Please make sure that you have run " +
        s"`cd tools/native-image-config-cleanup && npm start` after updating " +
        s"the configuration. Please refer to " +
        s"`docs/infrastructure/native-image.md` for more details."
      )
    }
  }
}
