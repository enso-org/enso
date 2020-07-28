package org.enso.launcher

import java.nio.file.Path

import org.enso.launcher.installation.DistributionManager
import org.enso.pkg.PackageManager
import org.enso.version.{VersionDescription, VersionDescriptionParameter}

object Launcher {
  private val packageManager = PackageManager.Default

  private def workingDirectory: Path = Path.of(".")

  /**
    * Creates a new project with the given `name` in the given `path`.
    *
    * If `path` is not set, the project is created in a directory called `name`
    * in the current directory.
    *
    * TODO [RW] this is not the final implementation, it will be finished in
    *  #977
    */
  def newProject(name: String, path: Option[Path]): Unit = {
    val actualPath = path.getOrElse(workingDirectory.resolve(name))
    packageManager.create(actualPath.toFile, name)
    println(s"Project created in $actualPath")
  }

  /**
    * Displays the version string of the launcher.
    *
    * @param useJSON specifies whether the output should use JSON or a
    *                human-readable format
    */
  def displayVersion(useJSON: Boolean): Unit = {
    val runtimeVersionParameter = VersionDescriptionParameter(
      humanReadableName = "Currently selected Enso version",
      humandReadableValue =
        "\nRuntime component is not yet implemented in the launcher.",
      jsonName  = "runtime",
      jsonValue = "\"<not implemented yet>\"" // TODO [RW] add with #976
    )

    val versionDescription = VersionDescription.make(
      "Enso Launcher",
      includeRuntimeJVMInfo = false,
      additionalParameters  = Seq(runtimeVersionParameter)
    )

    println(versionDescription.asString(useJSON))
  }

  /**
    * Checks if the launcher is running in portable mode and exits if it is not.
    */
  def ensurePortable(): Unit = {
    if (!DistributionManager.isRunningPortable) {
      Logger.error(
        "`--ensure-portable` is set, but the launcher is not running in " +
        "portable mode. Terminating."
      )
      sys.exit(1)
    }
  }

}
