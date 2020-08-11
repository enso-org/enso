package org.enso.launcher

import java.nio.file.Path

import buildinfo.Info
import nl.gn0s1s.bump.SemVer
import org.enso.launcher.Launcher.workingDirectory
import org.enso.launcher.cli.GlobalCLIOptions
import org.enso.launcher.components.DefaultComponentsManager
import org.enso.launcher.components.runner.{
  JVMSettings,
  LanguageServerOptions,
  Runner
}
import org.enso.launcher.installation.DistributionManager
import org.enso.launcher.project.ProjectManager
import org.enso.version.{VersionDescription, VersionDescriptionParameter}

/**
  * Implements launcher commands that are run from CLI and can be affected by
  * the global CLI options.
  *
  * @param cliOptions the global CLI options to use for the commands
  */
case class Launcher(cliOptions: GlobalCLIOptions) {
  private lazy val componentsManager = DefaultComponentsManager(cliOptions)
  private lazy val configurationManager =
    new GlobalConfigurationManager(componentsManager)
  private lazy val projectManager = new ProjectManager(configurationManager)
  private lazy val runner =
    new Runner(projectManager, configurationManager, componentsManager)

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
    projectManager.newProject(name, actualPath)
  }

  /**
    * Prints a list of installed engines.
    */
  def listEngines(): Unit = {
    for (engine <- componentsManager.listInstalledEngines()) {
      println(engine.version.toString)
    }
  }

  /**
    * Prints a list of installed runtimes.
    */
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

  /**
    * Prints a summary of installed components and their dependencies.
    */
  def listSummary(): Unit = {
    for (engine <- componentsManager.listInstalledEngines()) {
      val runtime = componentsManager.findRuntime(engine)
      val runtimeName = runtime
        .map(_.toString)
        .getOrElse("no runtime found for this distribution")
      println(s"Enso ${engine.version} -> $runtimeName")
    }
  }

  /**
    * Installs the specified engine `version`.
    *
    * Also installs the required runtime if it wasn't already installed.
    */
  def installEngine(version: SemVer): Unit = {
    val existing = componentsManager.findEngine(version)
    if (existing.isDefined) {
      Logger.info(s"Engine $version is already installed.")
    } else {
      componentsManager.findOrInstallEngine(version, complain = false)
    }
  }

  /**
    * Installs the latest available version of the engine.
    *
    * Also installs the required runtime if it wasn't already installed.
    */
  def installLatestEngine(): Unit = {
    val latest = componentsManager.fetchLatestEngineVersion()
    Logger.info(s"Installing Enso engine $latest")
    installEngine(latest)
  }

  /**
    * Uninstalls the specified engine `version`.
    *
    * If a runtime is not used by any engines anymore, it is also removed.
    */
  def uninstallEngine(version: SemVer): Unit =
    componentsManager.uninstallEngine(version)

  def runRepl(
    projectPath: Option[Path],
    versionOverride: Option[SemVer],
    useSystemJVM: Boolean,
    jvmOpts: Seq[(String, String)],
    additionalArguments: Seq[String]
  ): Unit = {
    val exitCode = runner
      .createCommand(
        runner.repl(projectPath, versionOverride, additionalArguments).get,
        JVMSettings(useSystemJVM, jvmOpts)
      )
      .run()
    sys.exit(exitCode)
  }

  def runRun(
    path: Option[Path],
    versionOverride: Option[SemVer],
    useSystemJVM: Boolean,
    jvmOpts: Seq[(String, String)],
    additionalArguments: Seq[String]
  ): Unit = {
    val exitCode = runner
      .createCommand(
        runner.run(path, versionOverride, additionalArguments).get,
        JVMSettings(useSystemJVM, jvmOpts)
      )
      .run()
    sys.exit(exitCode)
  }

  def runLanguageServer(
    options: LanguageServerOptions,
    versionOverride: Option[SemVer],
    useSystemJVM: Boolean,
    jvmOpts: Seq[(String, String)],
    additionalArguments: Seq[String]
  ): Unit = {
    val exitCode = runner
      .createCommand(
        runner
          .languageServer(options, versionOverride, additionalArguments)
          .get,
        JVMSettings(useSystemJVM, jvmOpts)
      )
      .run()
    sys.exit(exitCode)
  }

  def setDefaultVersion(version: SemVer): Unit = {
    val _ = version
    Logger.error("This feature is not implemented yet.")
    sys.exit(1)
  }

  def printDefaultVersion(): Unit = {
    println(configurationManager.defaultVersion)
  }
}

/**
  * Gathers launcher commands which do not depend on the global CLI options.
  */
object Launcher {

  /**
    * Version of the launcher.
    */
  val version: SemVer = SemVer(Info.ensoVersion).getOrElse {
    throw new IllegalStateException("Cannot parse the built-in version.")
  }

  private val workingDirectory: Path = Path.of(".")

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
