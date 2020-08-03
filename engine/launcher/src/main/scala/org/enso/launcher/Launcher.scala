package org.enso.launcher

import java.nio.file.Path

import org.enso.launcher.installation.DistributionManager
import org.enso.pkg.PackageManager
import org.enso.version.{VersionDescription, VersionDescriptionParameter}
import buildinfo.Info
import nl.gn0s1s.bump.SemVer
import org.enso.launcher.cli.GlobalCLIOptions
import org.enso.launcher.components.DefaultComponentsManager

case class Launcher(cliOptions: GlobalCLIOptions) {
  private lazy val componentsManager = DefaultComponentsManager(cliOptions)

  def listEngines(): Unit = {
    for (engine <- componentsManager.listInstalledEngines()) {
      println(engine.version.toString)
    }
  }

  def listRuntimes(): Unit = {
    for (runtime <- componentsManager.listInstalledRuntimes()) {
      val engines = componentsManager.findEnginesUsingRuntime(runtime)
      val usedBy = {
        val plural =
          if (engines.length != 1) "s"
          else ""
        s"(used by ${engines.length} Enso installation$plural)"
      }
      println(s"$runtime $usedBy")
    }
  }

  def listSummary(): Unit = {
    for (engine <- componentsManager.listInstalledEngines()) {
      val runtime = componentsManager.findRuntime(engine)
      val runtimeName = runtime
        .map(_.toString)
        .getOrElse("no runtime found for this distribution")
      println(s"Enso ${engine.version} -> $runtimeName")
    }
  }

  def installEngine(version: SemVer): Unit = {
    val existing = componentsManager.findEngine(version)
    if (existing.isDefined) {
      Logger.info(s"Engine $version is already installed.")
    } else {
      componentsManager.installEngine(version)
    }
  }

  def installEngineLatest(): Unit = {
    val latest = componentsManager.fetchLatestEngineVersion()
    Logger.info(s"Installing Enso engine $latest")
    installEngine(latest)
  }
}

object Launcher {
  val version: SemVer = SemVer(Info.ensoVersion).getOrElse {
    throw new IllegalStateException("Cannot parse the built-in version.")
  }

  private val packageManager         = PackageManager.Default
  private val workingDirectory: Path = Path.of(".")

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
