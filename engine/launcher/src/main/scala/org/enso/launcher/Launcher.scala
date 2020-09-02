package org.enso.launcher

import java.nio.file.Path

import buildinfo.Info
import io.circe.Json
import nl.gn0s1s.bump.SemVer
import org.enso.launcher.cli.GlobalCLIOptions
import org.enso.launcher.components.DefaultComponentsManager
import org.enso.launcher.components.runner.{
  JVMSettings,
  LanguageServerOptions,
  Runner,
  WhichEngine
}
import org.enso.launcher.config.{DefaultVersion, GlobalConfigurationManager}
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
    new GlobalConfigurationManager(componentsManager, DistributionManager)
  private lazy val projectManager = new ProjectManager(configurationManager)
  private lazy val runner =
    new Runner(
      projectManager,
      configurationManager,
      componentsManager,
      Environment
    )

  /**
    * Creates a new project with the given `name` in the given `path`.
    *
    * If `path` is not set, the project is created in a directory called `name`
    * in the current directory.
    *
    * If `author.name` or `author.email` are set in the global config, their
    * values are used to set a default author and maintainer for the created
    * project.
    */
  def newProject(
    name: String,
    path: Option[Path],
    versionOverride: Option[SemVer],
    useSystemJVM: Boolean,
    jvmOpts: Seq[(String, String)],
    additionalArguments: Seq[String]
  ): Unit = {
    val actualPath = path.getOrElse(Launcher.workingDirectory.resolve(name))
    val version =
      versionOverride.getOrElse(configurationManager.defaultVersion)
    val globalConfig = configurationManager.getConfig

    val exitCode = runner
      .createCommand(
        runner
          .newProject(
            path                = actualPath,
            name                = name,
            version             = version,
            authorName          = globalConfig.authorName,
            authorEmail         = globalConfig.authorEmail,
            additionalArguments = additionalArguments
          )
          .get,
        JVMSettings(useSystemJVM, jvmOpts)
      )
      .run()
      .get

    if (exitCode == 0) {
      Logger.info(s"Project created in `$actualPath` using version $version.")
    } else {
      Logger.error("Project creation failed.")
      sys.exit(exitCode)
    }
  }

  /**
    * Prints a list of installed engines.
    */
  def listEngines(): Unit = {
    for (engine <- componentsManager.listInstalledEngines()) {
      val broken = if (engine.isMarkedBroken) " (broken)" else ""
      println(engine.version.toString + broken)
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
      val broken = if (engine.isMarkedBroken) " (broken)" else ""
      println(s"Enso ${engine.version}$broken -> $runtimeName")
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
    Logger.info(s"Installing Enso engine $latest.")
    installEngine(latest)
  }

  /**
    * Uninstalls the specified engine `version`.
    *
    * If a runtime is not used by any engines anymore, it is also removed.
    */
  def uninstallEngine(version: SemVer): Unit =
    componentsManager.uninstallEngine(version)

  /**
    * Runs the Enso REPL.
    *
    * If ran outside of a project, uses the default configured version. If run
    * inside a project or provided with an explicit projectPath, the Enso
    * version associated with the project is run.
    *
    * @param projectPath if provided, the REPL is run in context of that project
    * @param versionOverride if provided, overrides the default engine version
    *                        that would have been used
    * @param useSystemJVM if set, forces to use the default configured JVM,
    *                     instead of the JVM associated with the engine version
    * @param jvmOpts additional options to pass to the launched JVM
    * @param additionalArguments additional arguments to pass to the runner
    */
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
      .get
    sys.exit(exitCode)
  }

  /**
    * Runs an Enso script or project.
    *
    * If ran inside a project without a path, or with a path pointing to a
    * project, runs that project. If the provided path points to a file, that
    * file is executed as an Enso script. If the file is located inside of a
    * project, it is executed in the context of that project. Otherwise it is
    * run as a standalone script and the default engine version is used.
    *
    * @param path specifies what to run
    * @param versionOverride if provided, overrides the default engine version
    *                        that would have been used
    * @param useSystemJVM if set, forces to use the default configured JVM,
    *                     instead of the JVM associated with the engine version
    * @param jvmOpts additional options to pass to the launched JVM
    * @param additionalArguments additional arguments to pass to the runner
    */
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
      .get
    sys.exit(exitCode)
  }

  /**
    * Runs the Language Server.
    *
    * Unless overridden, uses the Enso version associated with the project
    * located at `options.path`.
    *
    * @param options configuration required by the language server
    * @param versionOverride if provided, overrides the default engine version
    *                        that would have been used
    * @param useSystemJVM if set, forces to use the default configured JVM,
    *                     instead of the JVM associated with the engine version
    * @param jvmOpts additional options to pass to the launched JVM
    * @param additionalArguments additional arguments to pass to the runner
    */
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
      .get
    sys.exit(exitCode)
  }

  /**
    * Updates the global configuration.
    *
    * If `value` is an empty string, the `key` is removed from the configuration
    * (if it exists). If `value` is non-empty the key is added or updated in the
    * config. Any updates that set a known key to an invalid value which would
    * prevent from loading the config are cancelled.
    */
  def updateConfig(key: String, value: String): Unit = {
    if (value.isEmpty) {
      configurationManager.removeFromConfig(key)
      Logger.info(
        s"""Key `$key` removed from the global configuration file """ +
        s"(${configurationManager.configLocation.toAbsolutePath})."
      )
    } else {
      configurationManager.updateConfigRaw(key, Json.fromString(value))
      Logger.info(
        s"""Key `$key` set to "$value" in the global configuration file """ +
        s"(${configurationManager.configLocation.toAbsolutePath})."
      )
    }
  }

  /**
    * Prints the value of `key` from the global configuration.
    *
    * If the `key` is not set in the config, sets exit code to 1 and prints a
    * warning.
    */
  def printConfig(key: String): Unit = {
    configurationManager.getConfig.original.apply(key) match {
      case Some(value) =>
        println(value)
        sys.exit()
      case None =>
        Logger.warn(s"Key $key is not set in the global config.")
        sys.exit(1)
    }
  }

  /**
    * Sets the default Enso version.
    */
  def setDefaultVersion(version: DefaultVersion): Unit = {
    configurationManager.updateConfig { config =>
      config.copy(defaultVersion = version)
    }

    version match {
      case DefaultVersion.LatestInstalled =>
        Logger.info(
          s"Default Enso version set to the latest installed version, " +
          s"currently ${configurationManager.defaultVersion}."
        )
      case DefaultVersion.Exact(version) =>
        Logger.info(s"Default Enso version set to $version.")
    }
  }

  /**
    * Prints the default Enso version.
    */
  def printDefaultVersion(): Unit = {
    println(configurationManager.defaultVersion)
  }

  /**
    * Displays the version string of the launcher.
    *
    * @param hideEngineVersion if set, does not look for installed engines to
    *                          display the current version; this can be used to
    *                          avoid making network requests
    */
  def displayVersion(
    hideEngineVersion: Boolean = false
  ): Unit = {
    val useJSON = cliOptions.useJSON
    val runtimeVersionParameter =
      if (hideEngineVersion) None else Some(getEngineVersion(useJSON))

    val versionDescription = VersionDescription.make(
      "Enso Launcher",
      includeRuntimeJVMInfo         = false,
      enableNativeImageOSWorkaround = true,
      additionalParameters          = runtimeVersionParameter.toSeq
    )

    println(versionDescription.asString(useJSON))
  }

  private def getEngineVersion(
    useJSON: Boolean
  ): VersionDescriptionParameter = {
    val (runtimeVersionRunSettings, whichEngine) = runner.version(useJSON).get

    val isEngineInstalled =
      componentsManager.findEngine(runtimeVersionRunSettings.version).isDefined
    val runtimeVersionString = if (isEngineInstalled) {
      val runtimeVersionCommand = runner.createCommand(
        runtimeVersionRunSettings,
        JVMSettings(useSystemJVM = false, jvmOptions = Seq.empty)
      )

      val output = runtimeVersionCommand.captureOutput().get
      if (useJSON) output else "\n" + output.stripTrailing()
    } else {
      if (useJSON) "null"
      else "Not installed."
    }

    VersionDescriptionParameter(
      humanReadableName = whichEngine match {
        case WhichEngine.FromProject(name) =>
          s"Enso engine from project $name"
        case WhichEngine.Default => "Current default Enso engine"
      },
      jsonName = "runtime",
      value    = runtimeVersionString
    )
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
