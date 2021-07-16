import sbt.Keys._
import sbt._
import sbt.io.IO
import sbt.librarymanagement.{ConfigurationFilter, DependencyFilter}

import scala.sys.process.Process

object StdBits {

  /** Discovers dependencies of a project and copies them into the destination
    * directory.
    *
    * @param destination location where to put the dependencies
    * @param baseJarName name of the base generated JAR (if any); unexpected
    *                    (old) files are removed, so this task needs to know
    *                    this file's name to avoid removing it
    * @param ignoreScalaLibrary whether to ignore Scala dependencies that are
    *                           added by default be SBT and are not relevant in
    *                           pure-Java projects
    */
  def copyDependencies(
    destination: File,
    baseJarName: Option[String],
    ignoreScalaLibrary: Boolean,
    unpackedDeps: Set[String] = Set()
  ): Def.Initialize[Task[Unit]] =
    Def.task {
      val libraryUpdates = (Compile / update).value
      val log            = streams.value.log

      val ignoredConfigurations: NameFilter =
        if (ignoreScalaLibrary)
          new ExactFilter(Configurations.ScalaTool.name)
        else NothingFilter
      val configFilter: ConfigurationFilter =
        DependencyFilter.configurationFilter(-ignoredConfigurations)

      val graalOrg = new ExactFilter("org.graalvm.sdk")
      val relevantFiles =
        libraryUpdates
          .select(
            configuration = configFilter,
            module        = DependencyFilter.moduleFilter(organization = -graalOrg),
            artifact      = DependencyFilter.artifactFilter()
          )
//          .map { file =>
//            if (unpackedDeps.contains(file.getParentFile.getParentFile.getName)) {
//              val name = file.getName
//              file.getParentFile / name.substring(0, name.lastIndexOf('.'))
//            } else file
//          }

      val dependencyStore =
        streams.value.cacheStoreFactory.make("std-bits-dependencies")
      Tracked.diffInputs(dependencyStore, FileInfo.hash)(relevantFiles.toSet) {
        report =>
          val expectedFileNames =
            report.checked.map(
              getDestinationFileName(_, unpackedDeps)
            ) ++ baseJarName.toSeq
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
            updateDependency(changed, unpackedDeps, destination)
          }
          for (file <- report.unmodified) {
            val dest = destination / getDestinationFileName(file, unpackedDeps)
            if (!dest.exists()) {
              log.info(s"Adding missing std-bits dependency ${file.getName}.")
              updateDependency(file, unpackedDeps, destination)
            }
          }
      }
    }

  private def shouldUnpack(jar: File, unpacked: Set[String]): Boolean =
    unpacked.contains(jar.getParentFile.getParentFile.getName)

  private def updateDependency(
    jar: File,
    unpacked: Set[String],
    destinationDir: File
  ): Unit = {
    val destination = destinationDir / getDestinationFileName(jar, unpacked)
    if (shouldUnpack(jar, unpacked)) {
      destination.mkdirs()
      Process(s"jar xf ${jar.getAbsolutePath}", destination)!
    } else {
      IO.copyFile(jar, destination)
    }
  }

  private def getDestinationFileName(
    file: File,
    unpacked: Set[String]
  ): String = {
    if (shouldUnpack(file, unpacked)) {
      val name = file.getName
      name.substring(0, name.lastIndexOf('.'))
    } else file.getName
  }
}
