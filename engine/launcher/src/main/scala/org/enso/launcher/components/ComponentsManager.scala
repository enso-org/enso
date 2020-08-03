package org.enso.launcher.components

import java.nio.file.{Files, Path}

import nl.gn0s1s.bump.SemVer
import org.enso.cli.CLIOutput
import org.enso.launcher.FileSystem.PathSyntax
import org.enso.launcher.archive.Archive
import org.enso.launcher.cli.GlobalCLIOptions
import org.enso.launcher.installation.DistributionManager
import org.enso.launcher.releases.{
  EngineReleaseProvider,
  GraalCEReleaseProvider,
  RuntimeReleaseProvider
}
import org.enso.launcher.{FileSystem, Launcher, Logger}

import scala.util.control.NonFatal

case class Runtime(version: RuntimeVersion, path: Path) {
  override def toString: String =
    s"GraalVM ${version.graal}-java${version.java}"
}
case class Engine(version: SemVer, path: Path, manifest: Manifest) {
  override def toString: String =
    s"Enso Engine $version"
}

class ComponentsManager(
  cliOptions: GlobalCLIOptions,
  distributionManager: DistributionManager,
  engineReleaseProvider: EngineReleaseProvider,
  runtimeReleaseProvider: RuntimeReleaseProvider
) {
  private val showProgress = !cliOptions.hideProgress

  def findRuntime(engine: Engine): Option[Runtime] =
    findRuntime(engine.manifest.runtimeVersion)

  def findRuntime(version: RuntimeVersion): Option[Runtime] = {
    val name = runtimeNameForVersion(version)
    val path = distributionManager.paths.runtimes / name
    if (Files.exists(path)) {
      // TODO [RW] add a sanity check if runtime is in a working state - check
      //  if it at least has the `java` executable, in #976 do the check and
      //  throw an exception on failure, in #1052 offer to repair the broken
      //  installation
      parseGraalRuntime(path)
    } else None
  }

  def findOrInstallRuntime(
    engine: Engine,
    complain: Boolean = true
  ): Runtime =
    findRuntime(engine) match {
      case Some(found) => found
      case None =>
        def complainAndAsk(): Boolean = {
          Logger.error(
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
          throw new RuntimeException(
            s"No runtime for engine $engine. Cannot continue."
          )
        }
    }

  def findEngine(version: SemVer): Option[Engine] = {
    val name = engineNameForVersion(version)
    val path = distributionManager.paths.engines / name
    if (Files.exists(path))
      parseEngine(path)
    else None
  }

  def findOrInstallEngine(version: SemVer, complain: Boolean = true): Engine =
    findEngine(version) match {
      case Some(found) => found
      case None =>
        def complainAndAsk(): Boolean = {
          Logger.error(s"Engine $version is missing.")
          cliOptions.autoConfirm || CLIOutput.askConfirmation(
            "Do you want to install the missing engine?",
            yesDefault = true
          )
        }

        if (!complain || complainAndAsk()) {
          installEngine(version)
        } else {
          throw new RuntimeException(s"No engine $version. Cannot continue.")
        }
    }

  def findEnginesUsingRuntime(runtime: Runtime): Seq[Engine] =
    listInstalledEngines().filter(_.manifest.runtimeVersion == runtime.version)

  def listInstalledRuntimes(): Seq[Runtime] =
    FileSystem
      .listDirectory(distributionManager.paths.runtimes)
      .flatMap(parseGraalRuntime)

  def listInstalledEngines(): Seq[Engine] =
    FileSystem
      .listDirectory(distributionManager.paths.engines)
      .flatMap(parseEngine)

  def fetchLatestEngineVersion(): SemVer =
    engineReleaseProvider.findLatest().get

  private def installEngine(version: SemVer): Engine = {
    val engineRelease = engineReleaseProvider.getRelease(version).get
    FileSystem.withTemporaryDirectory("enso-install") { directory =>
      Logger.debug(s"Downloading packages to $directory")
      val enginePackage = directory / engineRelease.packageFileName
      Logger.info(s"Downloading ${enginePackage.getFileName}")
      engineReleaseProvider
        .downloadPackage(engineRelease, enginePackage)
        .waitForResult(showProgress)
        .get

      val engineDirectoryName =
        engineDirectoryNameForVersion(engineRelease.version)

      Logger.info(s"Extracting engine")
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

      val temporaryEngine = parseEngine(engineTemporaryPath).getOrElse {
        undoTemporaryEngine()
        throw new InstallationError(
          "Cannot load downloaded engine. Installation reverted."
        )
      }

      try {
        if (temporaryEngine.manifest != engineRelease.manifest) {
          undoTemporaryEngine()
          throw new InstallationError(
            "Manifest of installed engine does not match the published " +
            "manifest. This may lead to version inconsistencies; the package " +
            "may possibly be corrupted. Reverting installation."
          )
        }

        findOrInstallRuntime(temporaryEngine, complain = false)

        val enginePath = distributionManager.paths.engines / engineDirectoryName
        Files.move(engineTemporaryPath, enginePath)
        val engine = findEngine(version).getOrElse {
          Logger.error(
            "fatal: Could not load the installed engine." +
            "Reverting the installation."
          )
          FileSystem.removeDirectory(enginePath)
          cleanupRuntimes()
          throw new InstallationError(
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

  private def engineNameForVersion(version: SemVer): String =
    version.toString

  private def runtimeNameForVersion(version: RuntimeVersion): String =
    s"graalvm-ce-java${version.java}-${version.graal}"

  private def parseGraalRuntime(path: Path): Option[Runtime] = {
    val name = path.getFileName.toString
    for {
      version <- parseGraalRuntimeVersionString(name)
    } yield Runtime(version, path)
  }

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

  private def parseEngine(path: Path): Option[Engine] =
    for {
      version  <- parseEngineVersion(path)
      manifest <- loadEngineManifest(path)
    } yield Engine(version, path, manifest)

  private def parseEngineVersion(path: Path): Option[SemVer] = {
    val version = SemVer(path.getFileName.toString)
    if (version.isEmpty) {
      Logger.warn(s"Invalid engine component version `${path.getFileName}`.")
    }
    version
  }

  private def loadEngineManifest(path: Path): Option[Manifest] = {
    val manifest = Manifest.load(path / Manifest.DEFAULT_MANIFEST_NAME)
    manifest match {
      case Some(manifest) =>
        if (manifest.minimumLauncherVersion > Launcher.version) {
          Logger.warn(
            s"Engine `${path.getFileName}` requires launcher version >= " +
            s"${manifest.minimumLauncherVersion}, but the launcher is " +
            s"running ${Launcher.version}. This engine version will not be " +
            s"available until you upgrade the launcher."
          )
          None
        } else Some(manifest)
      case None =>
        Logger.warn(s"Cannot load manifest for `${path.getFileName}`.")
        None
    }
  }

  private def installRuntime(runtimeVersion: RuntimeVersion): Runtime =
    FileSystem.withTemporaryDirectory("enso-install-runtime") { directory =>
      val runtimePackage =
        directory / runtimeReleaseProvider.packageFileName(runtimeVersion)
      Logger.info(s"Downloading ${runtimePackage.getFileName}")
      runtimeReleaseProvider
        .downloadPackage(runtimeVersion, runtimePackage)
        .waitForResult(showProgress)
        .get

      val runtimeDirectoryName = graalDirectoryForVersion(runtimeVersion)

      Logger.info(s"Extracting runtime")
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
        val temporaryRuntime = parseGraalRuntime(runtimeTemporaryPath)
        if (temporaryRuntime.isEmpty) {
          throw new InstallationError(
            "Cannot load the installed runtime. The package may have been " +
            "corrupted. Reverting installation."
          )
        }

        val runtimePath =
          distributionManager.paths.runtimes / runtimeDirectoryName
        Files.move(runtimeTemporaryPath, runtimePath)
        val runtime = parseGraalRuntime(runtimePath).getOrElse {
          FileSystem.removeDirectory(runtimePath)
          throw new InstallationError(
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

  class InstallationError(message: String, cause: Throwable = null)
      extends RuntimeException(message, cause) {
    override def toString: String = s"Installation failed: $message"
  }

  private def cleanupRuntimes(): Unit = {
    // TODO
  }
}

case class DefaultComponentsManager(cliOptions: GlobalCLIOptions)
    extends ComponentsManager(
      cliOptions,
      DistributionManager,
      EngineReleaseProvider,
      GraalCEReleaseProvider
    )
