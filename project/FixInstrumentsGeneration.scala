import sbt.Keys._
import sbt._

object FixInstrumentsGeneration {

  /** This task detects any changes in source files of Instruments and forces
    * recompilation of all instruments on any change. This is to ensure that the
    * Annotation Processor registers all of the instruments.
    *
    * Without that fix, incremental compilation would not register unchanged
    * instruments, leading to runtime errors.
    *
    * It should be added as a dependency of Compile / compile / compileInputs.
    */
  lazy val preCompileTask = Def.task {
    val log                 = streams.value.log
    val root                = baseDirectory.value
    val classFilesDirectory = (Compile / classDirectory).value
    val FragileFiles(fragileSources, fragileClassFiles) =
      getFragileFiles(root, classFilesDirectory)

    val fragileSourcesStore =
      streams.value.cacheStoreFactory.make("instruments_fixer")

    Tracked.diffInputs(fragileSourcesStore, FileInfo.hash)(
      fragileSources.toSet
    ) { sourcesDiff: ChangeReport[File] =>
      if (sourcesDiff.modified.nonEmpty && sourcesDiff.unmodified.nonEmpty) {
        val others =
          if (sourcesDiff.modified.size >= 2)
            s" and ${sourcesDiff.modified.size - 1} others"
          else ""
        val firstInstrument = sourcesDiff.modified.head
        val sourcesMessage  = firstInstrument.toString + others
        log.warn(
          s"Instruments sources ($sourcesMessage) have been changed.\n" +
          s"Forcing recompilation of all instruments to maintain " +
          s"consistency of generated services files."
        )

        fragileClassFiles.foreach(_.delete())
      }
    }
  }

  /** This task detects if just a subset of the Instruments has been recompiled
    * (right now we did not find a way of detecting this before compilation). If
    * the Instrumentation state is detected to be inconsistent, current
    * compilation is aborted and classfiles are deleted to ensure that when
    * re-run Instrumentation will be brought back to a consistent state.
    *
    * Without that fix, incremental compilation would not register unchanged
    * instruments, leading to runtime errors.
    *
    * It should replace the default `Compile / compile` task in a project.
    */
  lazy val patchedCompile = Def.task {
    val compilationResult = (Compile / compile).value

    val log                 = streams.value.log
    val root                = baseDirectory.value
    val classFilesDirectory = (Compile / classDirectory).value
    val FragileFiles(_, fragileClassFiles) =
      getFragileFiles(root, classFilesDirectory)

    val fragileClassFilesStore =
      streams.value.cacheStoreFactory.make("instruments_classfiles")

    Tracked.diffInputs(fragileClassFilesStore, FileInfo.lastModified)(
      fragileClassFiles.toSet
    ) { sourcesDiff: ChangeReport[File] =>
      if (sourcesDiff.modified.nonEmpty && sourcesDiff.unmodified.nonEmpty) {
        fragileClassFiles.foreach(_.delete())

        val projectName = name.value
        log.error(
          "Truffle Instrumentation is not up to date, " +
          "which will lead to runtime errors\n" +
          "Fixes have been applied to ensure consistent Instrumentation state, " +
          "but compilation has to be triggered again.\n" +
          "Please re-run the previous command.\n" +
          "(If this for some reason fails, " +
          s"please do a clean build of the $projectName project)"
        )

        throw new RuntimeException("Please re-run last command")
      }
    }

    compilationResult
  }

  /** Deletes the compiled instrumentation class files, forcing all of them to
    * be recompiled.
    *
    * Since all instruments are recompiled at once, the service state should be
    * consistent as all of them will be re-registered.
    */
  def cleanInstruments = Def.task {
    val log                 = streams.value.log
    val root                = baseDirectory.value
    val classFilesDirectory = (Compile / classDirectory).value
    val FragileFiles(_, fragileClassFiles) =
      getFragileFiles(root, classFilesDirectory)
    fragileClassFiles.foreach { file =>
      if (file.exists()) {
        log.info(s"[clean-instruments] Removing $file")
        file.delete()
      } else {
        log.info(s"[clean-instruments] $file was already missing")
      }
    }
    log.info(
      "All fragile class files have been deleted. The next compilation " +
      "should be forced to recompile all of them, preserving instrumentation " +
      "configuration consistency."
    )
  }

  private case class FragileFiles(sources: Seq[File], classFiles: Seq[File])

  private def getFragileFiles(
    root: File,
    classFilesDirectory: File
  ): FragileFiles = {
    val fragileSources =
      (file(s"$root/src/main/java/") ** "*Instrument.java").get ++
      Seq(
        file(s"$root/src/main/java/org/enso/interpreter/Language.java"),
        file(s"$root/src/main/java/org/enso/interpreter/epb/EpbLanguage.java")
      )
    val fragileClassFiles =
      (classFilesDirectory ** "*Instrument.class").get ++
      Seq(
        file(s"$classFilesDirectory/org/enso/interpreter/Language.class"),
        file(s"$classFilesDirectory/org/enso/interpreter/epb/EpbLanguage.class")
      )
    FragileFiles(fragileSources, fragileClassFiles)
  }
}
