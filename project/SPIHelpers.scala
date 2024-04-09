import sbt.*
import sbt.Keys.*

object SPIHelpers {

  /** A helper task that ensures consistency of SPI services definitions.
    *
    * It should be attached as a dependency to `Compile / compile / compileInputs` of a given library.
    *
    * It detects any unknown classes in the `services` definitions and forces a recompilation if needed, to ensure the
    * consistency of the definitions. Without this helper task, the incremental compiler did not detect removed service
    * classes, thus after such a class was removed, it still stayed in the SPI configuration - crashing at runtime when
    * the missing class was attempted to be instantiated. Only a `clean` allowed to regenerate the SPI configuration.
    * This was causing issues on the CI when switching between PRs that have some new SPI configurations - they were
    * leaking and crashing unrelated PRs.
    *
    * This task is created with the `std-*` Java helper libraries in mind and is aimed primarily at Java-only projects.
    * Additional tweaks may be needed to get it working for mixed Java/Scala projects, if ever needed.
    *
    * @see https://github.com/enso-org/enso/pull/8129
    */
  def ensureSPIConsistency = Def.task {
    val log            = streams.value.log
    val classDir       = (Compile / compile / classDirectory).value
    val javaSourcesDir = (Compile / compile / javaSource).value
    val serviceDir     = classDir / "META-INF" / "services"
    log.debug(s"Scanning $serviceDir for SPI definitions.")

    val files: Array[File] =
      if (serviceDir.exists()) IO.listFiles(serviceDir) else Array()

    files.foreach { serviceConfig =>
      log.debug(s"Processing service definitions: $serviceConfig")
      val definedClasses =
        IO.readLines(serviceConfig).map { qualifiedClassName =>
          val subPath        = qualifiedClassName.replace('.', '/')
          val classFilePath  = classDir / (subPath + ".class")
          val sourceFilePath = javaSourcesDir / (subPath + ".java")

          // We check existence of the source file - because at pre-compile the .class file may still be there even if the
          // source is gone - it will only get deleted _after_ the compilation takes place - but that may be too late.
          // However, we return the path to the class file - so that we will be able to delete it to trigger the
          // recompilation for _existing_ sources.
          val hasSource = sourceFilePath.exists()

          if (!hasSource) {
            log.debug(
              s"The source file [$sourceFilePath] for class [$qualifiedClassName] does not exist."
            )
          }

          (classFilePath, hasSource)
        }

      val (kept, removed)     = definedClasses.partition(_._2)
      val needsForceRecompile = removed.nonEmpty
      if (needsForceRecompile) {
        val removedNames = removed.map(_._1).map(_.getName)
        val keptNames    = kept.map(_._1).map(_.getName)
        log.warn(s"No Java sources detected for classes: $removedNames.")
        log.warn(
          s"Removing $serviceConfig and forcing recompilation of $keptNames " +
          s"to ensure that the SPI definition is up-to-date."
        )
        IO.delete(serviceConfig)
        kept.foreach { case (path, _) => IO.delete(path) }
      } else {
        log.debug(s"No missing classes detected in $serviceConfig.")
      }
    }
  }
}
