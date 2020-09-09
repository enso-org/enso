package org.enso.launcher.components

import java.nio.file.{Files, Path, StandardOpenOption}

import nl.gn0s1s.bump.SemVer
import org.enso.cli.CLIOutput
import org.enso.launcher.FileSystem.PathSyntax
import org.enso.launcher.archive.Archive
import org.enso.launcher.cli.GlobalCLIOptions
import org.enso.launcher.installation.DistributionManager
import org.enso.launcher.releases.engine.EngineRelease
import org.enso.launcher.releases.runtime.{
  GraalCEReleaseProvider,
  RuntimeReleaseProvider
}
import org.enso.launcher.releases.{EnsoRepository, ReleaseProvider}
import org.enso.launcher.{FileSystem, Logger}

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try, Using}

/**
  * Manages runtime and engine components.
  *
  * Allows to find, list, install and uninstall components.
  *
  * @param cliOptions options from the CLI setting verbosity of the executed
  *                   actions
  * @param distributionManager the [[DistributionManager]] to use
  * @param engineReleaseProvider the provider of engine releases
  * @param runtimeReleaseProvider the provider of runtime releases
  */
class ComponentsManager(
  cliOptions: GlobalCLIOptions,
  distributionManager: DistributionManager,
  engineReleaseProvider: ReleaseProvider[EngineRelease],
  runtimeReleaseProvider: RuntimeReleaseProvider
) {
  private val showProgress = !cliOptions.hideProgress

  /**
    * Tries to find runtime for the provided engine.
    *
    * Returns None if the runtime is missing.
    */
  def findRuntime(engine: Engine): Option[Runtime] =
    findRuntime(engine.manifest.runtimeVersion)

  /**
    * Finds an installed runtime with the given `version`.
    *
    * Returns None if that version is not installed.
    */
  def findRuntime(version: RuntimeVersion): Option[Runtime] = {
    val name = runtimeNameForVersion(version)
    val path = distributionManager.paths.runtimes / name
    if (Files.exists(path)) {
      // TODO [RW] for now an exception is thrown if the installation is
      //  corrupted, in #1052 offer to repair the broken installation
      loadGraalRuntime(path)
        .map(Some(_))
        .recoverWith {
          case e: Exception =>
            Failure(
              UnrecognizedComponentError(
                s"The runtime $version is already installed, but cannot be " +
                s"loaded due to $e. Until the launcher gets an auto-repair " +
                s"feature, please try reinstalling the runtime by " +
                s"uninstalling all engines that use it and installing them " +
                s"again, or manually removing `$path`.",
                e
              )
            )
        }
        .get
    } else None
  }

  /**
    * Returns the runtime needed for the given engine, trying to install it if
    * it is missing.
    *
    * @param engine the engine for which the runtime is requested
    * @param complain if set and the runtime is missing, prints a warning and
    *                 asks the user to install the missing runtime (unless
    *                 [[cliOptions.autoConfirm]] is set, in which case it
    *                 installs it without asking)
    */
  def findOrInstallRuntime(
    engine: Engine,
    complain: Boolean = true
  ): Runtime =
    findRuntime(engine) match {
      case Some(found) => found
      case None =>
        def complainAndAsk(): Boolean = {
          Logger.warn(
            s"Runtime ${engine.manifest.runtimeVersion} required for $engine " +
            s"is missing."
          )
          cliOptions.autoConfirm || CLIOutput.askConfirmation(
            "Do you want to install the missing runtime?",
            yesDefault = true
          )
        }
        if (!complain || complainAndAsk()) {
          installRuntime(engine.manifest.runtimeVersion)
        } else {
          throw ComponentMissingError(
            s"No runtime for engine $engine. Cannot continue."
          )
        }
    }

  /**
    * Finds an installed engine with the given `version` and reports any errors.
    */
  def getEngine(version: SemVer): Try[Engine] = {
    val name = engineNameForVersion(version)
    val path = distributionManager.paths.engines / name
    if (Files.exists(path)) {
      // TODO [RW] right now we return an exception, in the future (#1052) we
      //  will try recovery
      loadEngine(path)
    } else Failure(ComponentMissingError(s"Engine $version is not installed."))
  }

  /**
    * Finds an engine with the given `version` or returns None if it is not
    * installed.
    *
    * Any other errors regarding loading the engine are thrown.
    * If the engine is marked as broken, a warning is reported.
    */
  def findEngine(version: SemVer): Option[Engine] =
    getEngine(version)
      .map { engine =>
        if (engine.isMarkedBroken) {
          Logger.warn(
            s"Running an engine release ($version) that is marked as broken. " +
            s"Please consider upgrading to a stable release."
          )
        }

        Some(engine)
      }
      .recoverWith {
        case _: ComponentMissingError        => Success(None)
        case e: LauncherUpgradeRequiredError => Failure(e)
        case e: Exception =>
          Failure(
            UnrecognizedComponentError(
              s"The engine $version is already installed, but cannot be " +
              s"loaded due to $e. Until the launcher gets an auto-repair " +
              s"feature, please try running `enso uninstall engine $version` " +
              s"followed by `enso install engine $version`.",
              e
            )
          )
      }
      .get

  /**
    * Returns the engine needed with the given version, trying to install it if
    * it is missing.
    *
    * @param version the requested engine version
    * @param complain if set and the engine is missing, prints a warning and
    *                 asks the user to install the missing engine (unless
    *                 [[cliOptions.autoConfirm]] is set, in which case it
    *                 installs it without asking)
    */
  def findOrInstallEngine(version: SemVer, complain: Boolean = true): Engine =
    findEngine(version) match {
      case Some(found) => found
      case None =>
        def complainAndAsk(): Boolean = {
          Logger.warn(s"Engine $version is missing.")
          cliOptions.autoConfirm || CLIOutput.askConfirmation(
            "Do you want to install the missing engine?",
            yesDefault = true
          )
        }

        if (!complain || complainAndAsk()) {
          installEngine(version)
        } else {
          throw ComponentMissingError(s"No engine $version. Cannot continue.")
        }
    }

  /**
    * Finds installed engines that use the given `runtime`.
    */
  def findEnginesUsingRuntime(runtime: Runtime): Seq[Engine] =
    listInstalledEngines().filter(_.manifest.runtimeVersion == runtime.version)

  /**
    * Lists all installed runtimes.
    */
  def listInstalledRuntimes(): Seq[Runtime] =
    FileSystem
      .listDirectory(distributionManager.paths.runtimes)
      .map(path => (path, loadGraalRuntime(path)))
      .flatMap(handleErrorsAsWarnings[Runtime]("A runtime"))

  /**
    * Lists all installed engines.
    * @return
    */
  def listInstalledEngines(): Seq[Engine] = {
    FileSystem
      .listDirectory(distributionManager.paths.engines)
      .map(path => (path, loadEngine(path)))
      .flatMap(handleErrorsAsWarnings[Engine]("An engine"))
  }

  /**
    * A helper function that is used when listing components.
    *
    * A component error is non-fatal in context of listing, so it is issued as a
    * warning and the component is treated as non-existent in the list.
    */
  private def handleErrorsAsWarnings[A](name: String)(
    result: (Path, Try[A])
  ): Seq[A] =
    result match {
      case (path, Failure(exception)) =>
        Logger.warn(
          s"$name at $path has been skipped due to the following error: " +
          s"$exception"
        )
        Seq()
      case (_, Success(value)) => Seq(value)
    }

  /**
    * Finds the latest released version of the engine, by asking the
    * [[engineReleaseProvider]].
    */
  def fetchLatestEngineVersion(): SemVer =
    engineReleaseProvider.findLatestVersion().get

  /**
    * Uninstalls the engine with the provided `version` (if it was installed).
    */
  def uninstallEngine(version: SemVer): Unit = {
    val engine = getEngine(version).getOrElse {
      Logger.warn(s"Enso Engine $version is not installed.")
      sys.exit(1)
    }

    safelyRemoveComponent(engine.path)
    Logger.info(s"Uninstalled $engine.")
    cleanupRuntimes()
  }

  /**
    * Installs the engine with the provided version.
    *
    * Used internally by [[findOrInstallEngine]]. Does not check if the engine
    * is already installed.
    *
    * The installation tries as much as possible to be robust - the downloaded
    * package is extracted to a temporary directory next to the `engines`
    * directory (to ensure that they are on the same filesystem) and is moved to
    * the actual directory after doing simple sanity checks.
    */
  private def installEngine(version: SemVer): Engine = {
    val engineRelease = engineReleaseProvider.fetchRelease(version).get
    if (!engineRelease.manifest.isUsableWithCurrentVersion) {
      throw LauncherUpgradeRequiredError(
        engineRelease.manifest.minimumLauncherVersion,
        cliOptions
      )
    }
    if (engineRelease.isBroken) {
      if (cliOptions.autoConfirm) {
        Logger.warn(
          s"The engine release $version is marked as broken and it should " +
          s"not be used. Since `auto-confirm` is set, the installation will " +
          s"continue, but you may want to reconsider changing versions to a " +
          s"stable release."
        )
      } else {
        Logger.warn(
          s"The engine release $version is marked as broken and it should " +
          s"not be used."
        )
        val continue = CLIOutput.askConfirmation(
          "Are you sure you still want to continue installing this version " +
          "despite the warning?"
        )
        if (!continue) {
          throw InstallationError(
            "Installation has been cancelled by the user because the " +
            "requested engine release is marked as broken."
          )
        }
      }
    }
    FileSystem.withTemporaryDirectory("enso-install") { directory =>
      Logger.debug(s"Downloading packages to $directory")
      val enginePackage = directory / engineRelease.packageFileName
      Logger.info(s"Downloading ${enginePackage.getFileName}.")
      engineRelease
        .downloadPackage(enginePackage)
        .waitForResult(showProgress)
        .get

      val engineDirectoryName =
        engineDirectoryNameForVersion(engineRelease.version)

      Logger.info(s"Extracting the engine.")
      Archive
        .extractArchive(
          enginePackage,
          distributionManager.paths.temporaryDirectory,
          Some(engineDirectoryName)
        )
        .waitForResult(showProgress)
        .get

      val engineTemporaryPath =
        distributionManager.paths.temporaryDirectory / engineDirectoryName
      def undoTemporaryEngine(): Unit = {
        if (Files.exists(engineTemporaryPath)) {
          FileSystem.removeDirectory(engineTemporaryPath)
        }
      }

      if (engineRelease.isBroken) {
        try {
          Using(
            Files.newBufferedWriter(
              engineTemporaryPath / Manifest.DEFAULT_MANIFEST_NAME,
              StandardOpenOption.WRITE,
              StandardOpenOption.APPEND
            )
          ) { writer =>
            writer.newLine()
            writer.write(s"${Manifest.Fields.brokenMark}: true\n")
          }.get
        } catch {
          case ex: Exception =>
            undoTemporaryEngine()
            throw InstallationError(
              "Cannot add the broken mark to the installed engine's " +
              "manifest. The installation has failed.",
              ex
            )
        }
      }

      val temporaryEngine = loadEngine(engineTemporaryPath).getOrElse {
        undoTemporaryEngine()
        throw InstallationError(
          "Cannot load downloaded engine. Installation reverted."
        )
      }

      try {
        val patchedReleaseManifest =
          engineRelease.manifest.copy(brokenMark = engineRelease.isBroken)
        if (temporaryEngine.manifest != patchedReleaseManifest) {
          undoTemporaryEngine()
          throw InstallationError(
            "Manifest of installed engine does not match the published " +
            "manifest. This may lead to version inconsistencies; the package " +
            "may possibly be corrupted. Reverting installation."
          )
        }

        findOrInstallRuntime(temporaryEngine, complain = false)

        val enginePath = distributionManager.paths.engines / engineDirectoryName
        FileSystem.atomicMove(engineTemporaryPath, enginePath)
        val engine = getEngine(version).getOrElse {
          Logger.error(
            "fatal: Could not load the installed engine." +
            "Reverting the installation."
          )
          FileSystem.removeDirectory(enginePath)
          cleanupRuntimes()
          throw InstallationError(
            "fatal: Could not load the installed engine"
          )
        }

        Logger.info(s"Installed $engine.")
        engine
      } catch {
        case e: Exception =>
          undoTemporaryEngine()
          throw e
      }
    }
  }

  /**
    * Returns name of the directory containing the engine of that version.
    */
  private def engineNameForVersion(version: SemVer): String =
    version.toString

  /**
    * Returns name of the directory containing the runtime of that version.
    */
  private def runtimeNameForVersion(version: RuntimeVersion): String =
    s"graalvm-ce-java${version.java}-${version.graal}"

  /**
    * Loads the GraalVM runtime definition.
    */
  private def loadGraalRuntime(path: Path): Try[Runtime] = {
    val name = path.getFileName.toString
    def verifyRuntime(runtime: Runtime): Try[Unit] =
      if (runtime.isValid) {
        Success(())
      } else {
        Failure(CorruptedComponentError(s"Runtime $runtime is corrupted."))
      }

    for {
      version <- parseGraalRuntimeVersionString(name)
        .toRight(
          UnrecognizedComponentError(s"Invalid runtime component name `$name`.")
        )
        .toTry
      runtime = Runtime(version, path)
      _ <- verifyRuntime(runtime)
    } yield runtime
  }

  /**
    * Gets the runtime version from its name.
    */
  private def parseGraalRuntimeVersionString(
    name: String
  ): Option[RuntimeVersion] = {
    val regex = """graalvm-ce-java(\d+)-(.+)""".r
    name match {
      case regex(javaVersionString, graalVersionString) =>
        SemVer(graalVersionString) match {
          case Some(graalVersion) =>
            Some(RuntimeVersion(graalVersion, javaVersionString))
          case None =>
            Logger.warn(
              s"Invalid runtime version string `$graalVersionString`."
            )
            None
        }
      case _ =>
        Logger.warn(
          s"Unrecognized runtime name `$name`."
        )
        None
    }
  }

  /**
    * Loads the engine definition.
    */
  private def loadEngine(path: Path): Try[Engine] = {
    def verifyEngine(engine: Engine): Try[Unit] =
      if (!engine.isValid) {
        Failure(
          CorruptedComponentError(s"Engine ${engine.version} is corrupted.")
        )
      } else {
        Success(())
      }

    for {
      version  <- parseEngineVersion(path)
      manifest <- loadAndCheckEngineManifest(path)
      engine = Engine(version, path, manifest)
      _ <- verifyEngine(engine)
    } yield engine
  }

  /**
    * Gets the engine version from its path.
    */
  private def parseEngineVersion(path: Path): Try[SemVer] = {
    val name = path.getFileName.toString
    SemVer(name)
      .toRight(
        UnrecognizedComponentError(
          s"Invalid engine component version `$name`."
        )
      )
      .toTry
  }

  /**
    * Loads the engine manifest, checking if that release is compatible with the
    * currently running launcher.
    */
  private def loadAndCheckEngineManifest(path: Path): Try[Manifest] = {
    Manifest.load(path / Manifest.DEFAULT_MANIFEST_NAME).flatMap { manifest =>
      if (!manifest.isUsableWithCurrentVersion) {
        Failure(
          LauncherUpgradeRequiredError(
            manifest.minimumLauncherVersion,
            cliOptions
          )
        )
      } else Success(manifest)
    }
  }

  /**
    * Installs the runtime with the provided version.
    *
    * Used internally by [[findOrInstallRuntime]]. Does not check if the runtime
    * is already installed.
    *
    * The installation tries as much as possible to be robust - the downloaded
    * package is extracted to a temporary directory next to the `runtimes`
    * directory (to ensure that they are on the same filesystem) and is moved to
    * the actual directory after doing simple sanity checks.
    */
  private def installRuntime(runtimeVersion: RuntimeVersion): Runtime =
    FileSystem.withTemporaryDirectory("enso-install-runtime") { directory =>
      val runtimePackage =
        directory / runtimeReleaseProvider.packageFileName(runtimeVersion)
      Logger.info(s"Downloading ${runtimePackage.getFileName}.")
      runtimeReleaseProvider
        .downloadPackage(runtimeVersion, runtimePackage)
        .waitForResult(showProgress)
        .get

      val runtimeDirectoryName = graalDirectoryForVersion(runtimeVersion)

      Logger.info(s"Extracting the runtime.")
      Archive
        .extractArchive(
          runtimePackage,
          distributionManager.paths.temporaryDirectory,
          Some(runtimeDirectoryName)
        )
        .waitForResult(showProgress)
        .get

      val runtimeTemporaryPath =
        distributionManager.paths.temporaryDirectory / runtimeDirectoryName

      def undoTemporaryRuntime(): Unit = {
        if (Files.exists(runtimeTemporaryPath)) {
          FileSystem.removeDirectory(runtimeTemporaryPath)
        }
      }

      try {
        val temporaryRuntime = loadGraalRuntime(runtimeTemporaryPath)
        if (temporaryRuntime.isFailure) {
          throw InstallationError(
            "Cannot load the installed runtime. The package may have been " +
            "corrupted. Reverting installation."
          )
        }

        val runtimePath =
          distributionManager.paths.runtimes / runtimeDirectoryName
        FileSystem.atomicMove(runtimeTemporaryPath, runtimePath)
        val runtime = loadGraalRuntime(runtimePath).getOrElse {
          FileSystem.removeDirectory(runtimePath)
          throw InstallationError(
            "fatal: Cannot load the installed runtime."
          )
        }

        Logger.info(s"Installed $runtime.")
        runtime
      } catch {
        case NonFatal(e) =>
          undoTemporaryRuntime()
          throw e
      }
    }

  private def engineDirectoryNameForVersion(version: SemVer): Path =
    Path.of(version.toString())

  private def graalDirectoryForVersion(version: RuntimeVersion): Path =
    Path.of(s"graalvm-ce-java${version.java}-${version.graal}")

  /**
    * Removes runtimes that are not used by any installed engines.
    */
  def cleanupRuntimes(): Unit = {
    for (runtime <- listInstalledRuntimes()) {
      if (findEnginesUsingRuntime(runtime).isEmpty) {
        Logger.info(
          s"Removing $runtime, because it is not used by any installed Enso " +
          s"versions."
        )
        safelyRemoveComponent(runtime.path)
      }
    }
  }

  /**
    * Tries to remove a component in a safe way.
    *
    * The component is moved (hopefully atomically) to temporary directory next
    * to the actual components directories and only then it is removed from
    * there. As the move should be executed as a single operation, there is no
    * risk that the system may leave the component corrupted if the deletion
    * were abruptly terminated. The component may be corrupted while being
    * removed, but it will already be in the temporary directory, so it will be
    * unreachable. The temporary directory is cleaned when doing
    * installation-related operations.
    */
  private def safelyRemoveComponent(path: Path): Unit = {
    val temporaryPath =
      distributionManager.paths.temporaryDirectory / path.getFileName
    FileSystem.atomicMove(path, temporaryPath)
    FileSystem.removeDirectory(temporaryPath)
  }
}

object ComponentsManager {

  /**
    * Creates a [[ComponentsManager]] using the default [[DistributionManager]]
    * and release providers.
    *
    * @param globalCLIOptions options from the CLI setting verbosity of the
    *                         executed actions
    */
  def makeDefault(globalCLIOptions: GlobalCLIOptions): ComponentsManager =
    new ComponentsManager(
      globalCLIOptions,
      DistributionManager,
      EnsoRepository.defaultEngineReleaseProvider,
      GraalCEReleaseProvider
    )
}
