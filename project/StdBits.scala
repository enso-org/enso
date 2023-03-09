import sbt.Keys._
import sbt._
import sbt.internal.util.ManagedLogger
import sbt.io.IO
import sbt.librarymanagement.{ConfigurationFilter, DependencyFilter}

import java.io.File

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
    ignoreScalaLibrary: Boolean
  ): Def.Initialize[Task[Unit]] =
    Def.task {
      val libraryUpdates = (Compile / update).value
      val log            = streams.value.log

      val baseFilter: NameFilter = new ExactFilter(Configurations.Runtime.name)
      val validConfig =
        if (ignoreScalaLibrary)
          baseFilter - new ExactFilter(Configurations.ScalaTool.name)
        else baseFilter
      val configFilter: ConfigurationFilter =
        DependencyFilter.configurationFilter(name = validConfig)

      val graalOrg = new ExactFilter("org.graalvm.sdk")
      val relevantFiles =
        libraryUpdates
          .select(
            configuration = configFilter,
            module        = DependencyFilter.moduleFilter(organization = -graalOrg),
            artifact      = DependencyFilter.artifactFilter()
          )

      val dependencyStore =
        streams.value.cacheStoreFactory.make("std-bits-dependencies")
      Tracked.diffInputs(dependencyStore, FileInfo.hash)(relevantFiles.toSet) {
        report =>
          val expectedFileNames =
            report.checked.map(file => file.getName) ++ baseJarName.toSeq
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
            updateDependency(changed, destination, log)
          }
          for (file <- report.unmodified) {
            val dest = destination / file.getName
            if (!dest.exists()) {
              log.info(s"Adding missing std-bits dependency ${file.getName}.")
              updateDependency(file, destination, log)
            }
          }
      }
    }

  private def updateDependency(
    jar: File,
    destinationDir: File,
    logger: ManagedLogger
  ): Unit = {
    val destination = destinationDir / jar.getName
    IO.copyFile(jar, destination)
  }

  /** Builds a single standard library package `name`. Should only be used
    * in tasks used in local development.
    *
    * @param name name of the package, see `stdBitsProjects` in build.sbt
    * @param root top directory where distribution is being built
    * @param cache used for persisting the cached information
    * @param log logger used in the task
    * @param defaultDevEnsoVersion default `dev` version
    */
  def buildStdLibPackage(
    name: String,
    root: File,
    cacheFactory: sbt.util.CacheStoreFactory,
    log: sbt.Logger,
    defaultDevEnsoVersion: String
  ) = {
    log.info(s"Building standard library package for '$name'")
    val prefix        = "Standard"
    val targetPkgRoot = root / "lib" / prefix / name / defaultDevEnsoVersion
    val sourceDir = file(
      s"distribution/lib/$prefix/$name/$defaultDevEnsoVersion"
    )
    if (!sourceDir.exists) {
      throw new RuntimeException("Invalid standard library package " + name)
    }
    val result = DistributionPackage.copyDirectoryIncremental(
      source      = file(s"distribution/lib/$prefix/$name/$defaultDevEnsoVersion"),
      destination = targetPkgRoot,
      cache       = cacheFactory.sub("engine-libraries").make(s"$prefix.$name")
    )
    if (result) {
      log.info(s"Package '$name' has been updated")
    } else {
      log.info(s"No changes detected for '$name' package")
    }
  }
}
