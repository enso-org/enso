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
    * @see https://github.com/enso-org/enso/pull/8129
    */
  def ensureSPIConsistency = Def.task {
    val log        = streams.value.log
    val classDir   = (Compile / compile / classDirectory).value
    val serviceDir = classDir / "META-INF" / "services"
    log.debug(s"Scanning $serviceDir for SPI definitions.")

    val files: Array[File] =
      if (serviceDir.exists()) IO.listFiles(serviceDir) else Array()

    files.foreach { serviceConfig =>
      log.debug(s"Processing service definitions: $serviceConfig")
      val definedClasses = IO.readLines(serviceConfig).map { cls =>
        val path = classDir / (cls.replace('.', '/') + ".class")
        (path, path.exists())
      }

      val (kept, removed)     = definedClasses.partition(_._2)
      val needsForceRecompile = removed.nonEmpty
      if (needsForceRecompile) {
        val removedNames = removed.map(_._1).map(_.getName)
        val keptNames    = kept.map(_._1).map(_.getName)
        log.warn(s"The classes $removedNames have been removed.")
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
