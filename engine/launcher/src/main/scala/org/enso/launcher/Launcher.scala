package org.enso.launcher

import java.nio.file.Path

import org.enso.pkg.PackageManager
import org.enso.version.{VersionDescription, VersionDescriptionParameter}

object Launcher {
  private val packageManager = PackageManager.Default

  private def workingDirectory: Path = Path.of(".")

  def newProject(name: String, path: Option[Path]): Unit = {
    // TODO [RW] this is not the final implementation
    val actualPath = path.getOrElse(workingDirectory.resolve(name))
    packageManager.create(actualPath.toFile, name)
    println(s"Project created in $actualPath")
  }

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
}
