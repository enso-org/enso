import java.io.IOException

import sbt.io.IO
import sbt.Keys._
import sbt._
import sbt.librarymanagement.{ConfigurationFilter, DependencyFilter}

import scala.sys.process._

object StdBits {
  def preparePackage =
    Def.task {
      val cmd = Seq("mvn", "package", "-f", "std-bits")
      val exitCode =
        try {
          if (Platform.isWindows) {
            (Seq("cmd", "/c") ++ cmd).!
          } else {
            cmd.!
          }
        } catch {
          case e @ (_: RuntimeException | _: IOException) =>
            throw new RuntimeException(
              "Cannot run `mvn`, " +
              "make sure that it is installed and present on your PATH.",
              e
            )
        }

      if (exitCode != 0) {
        throw new RuntimeException("std-bits build failed.")
      }
    }

  def copyDependencies(
    destination: File,
    baseJarName: String,
    ignoreScalaLibrary: Boolean
  ): Def.Initialize[Task[Unit]] =
    Def.task {
      val libraryUpdates = (Compile / update).value
      val log            = streams.value.log

      val ignoredConfigurations: NameFilter =
        if (ignoreScalaLibrary)
          new ExactFilter(Configurations.ScalaTool.name)
        else NothingFilter
      val filter: ConfigurationFilter =
        DependencyFilter.configurationFilter(-ignoredConfigurations)
      val relevantFiles = libraryUpdates.select(filter)

      val dependencyStore =
        streams.value.cacheStoreFactory.make("std-bits-dependencies")
      Tracked.diffInputs(dependencyStore, FileInfo.hash)(relevantFiles.toSet) {
        report =>
          val expectedFileNames = report.checked.map(_.getName) + baseJarName
          for (existing <- IO.listFiles(destination)) {
            if (!expectedFileNames.contains(existing.getName)) {
              log.info(
                s"Removing outdated std-bits dependency ${existing.getName}."
              )
              IO.delete(existing)
            }
          }
          for (changed <- report.modified -- report.removed) {
            log.info(
              s"Updating changed std-bits dependency ${changed.getName}."
            )
            IO.copyFile(changed, destination / changed.getName)
          }
          for (file <- report.checked) {
            val dest = destination / file.getName
            if (!dest.exists()) {
              log.info(s"Adding missing std-bits dependency ${file.getName}.")
              IO.copyFile(file, dest)
            }
          }
      }
    }
}
