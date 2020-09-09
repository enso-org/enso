package org.enso.launcher.upgrade

import java.nio.file.{Files, Path}

import nl.gn0s1s.bump.SemVer
import org.enso.cli.CLIOutput
import org.enso.launcher.FileSystem.PathSyntax
import org.enso.launcher.archive.Archive
import org.enso.launcher.cli.{GlobalCLIOptions, InternalOpts}
import org.enso.launcher.components.LauncherUpgradeRequiredError
import org.enso.launcher.installation.DistributionManager
import org.enso.launcher.releases.launcher.LauncherRelease
import org.enso.launcher.releases.{EnsoRepository, ReleaseProvider}
import org.enso.launcher.{CurrentVersion, FileSystem, Logger, OS}

import scala.util.Try
import scala.util.control.NonFatal

class LauncherUpgrader(
  globalCLIOptions: GlobalCLIOptions,
  distributionManager: DistributionManager,
  releaseProvider: ReleaseProvider[LauncherRelease],
  originalExecutablePath: Option[Path]
) {

  /**
    * Queries the release provider for the latest available valid launcher
    * version.
    */
  def latestVersion(): Try[SemVer] = {
    releaseProvider.findLatestVersion()
  }

  /**
    * Performs an upgrade to the `targetVersion`.
    *
    * The upgrade may first temporarily install versions older than the target
    * if the upgrade cannot be performed directly from the current version.
    */
  def upgrade(targetVersion: SemVer): Unit = {
    runCleanup(isStartup = true)
    val release = releaseProvider.fetchRelease(targetVersion).get
    if (release.isMarkedBroken) {
      if (globalCLIOptions.autoConfirm) {
        Logger.warn(
          s"The launcher release $targetVersion is marked as broken and it " +
          s"should not be used. Since `auto-confirm` is set, the upgrade " +
          s"will continue, but you may want to reconsider upgrading to a " +
          s"stable release."
        )
      } else {
        Logger.warn(
          s"The launcher release $targetVersion is marked as broken and it " +
          s"should not be used."
        )
        val continue = CLIOutput.askConfirmation(
          "Are you sure you still want to continue upgrading to this version " +
          "despite the warning?"
        )
        if (!continue) {
          throw UpgradeError(
            "Upgrade has been cancelled by the user because the requested " +
            "version is marked as broken."
          )
        }
      }
    }
    if (release.canPerformUpgradeFromCurrentVersion)
      performUpgradeTo(release)
    else
      performStepByStepUpgrade(release)

    runCleanup()
  }

  /**
    * Cleans up temporary and old launcher executables.
    *
    * Some executables may fail to be cleaned the first time, if other launcher
    * instances are still running. To ensure that old executables are cleaned,
    * this method can be run at launcher startup.
    *
    * @param isStartup specifies if the run is at startup; it will display a
    *                  message informing about the cleanup in this case
    */
  def runCleanup(isStartup: Boolean = false): Unit = {
    val binRoot = originalExecutable.getParent
    val temporaryFiles =
      FileSystem.listDirectory(binRoot).filter(isTemporaryExecutable)
    if (temporaryFiles.nonEmpty && isStartup) {
      Logger.debug("Cleaning temporary files from a previous upgrade.")
    }
    for (file <- temporaryFiles) {
      try {
        Files.delete(file)
        Logger.debug(s"Upgrade cleanup: removed `$file`.")
      } catch {
        case NonFatal(e) =>
          Logger.debug(s"Cannot remove temporary file $file: $e", e)
      }
    }
  }

  /**
    * Continues a multi-step upgrade.
    *
    * Called by [[InternalOpts]] when the upgrade continuation is requested by
    * [[runNextUpgradeStep]].
    */
  def internalContinueUpgrade(targetVersion: SemVer): Unit = {
    val release = releaseProvider.fetchRelease(targetVersion).get
    if (release.canPerformUpgradeFromCurrentVersion)
      performUpgradeTo(release)
    else
      performStepByStepUpgrade(release)
  }

  /**
    * Run the next step of the upgrade using the newly extracted newer launcher
    * version.
    *
    * @param temporaryExecutable path to the new, temporary launcher executable
    * @param targetVersion version to upgrade to
    */
  private def runNextUpgradeStep(
    temporaryExecutable: Path,
    targetVersion: SemVer
  ): Unit = {
    val exitCode = InternalOpts
      .runWithNewLauncher(temporaryExecutable)
      .continueUpgrade(
        targetVersion    = targetVersion,
        originalPath     = originalExecutable,
        globalCLIOptions = globalCLIOptions
      )
    if (exitCode != 0) {
      throw UpgradeError("Next upgrade step has failed. Upgrade cancelled.")
    }
  }

  private def showProgress = !globalCLIOptions.hideProgress

  /**
    * Path to the original launcher executable.
    */
  private val originalExecutable =
    originalExecutablePath.getOrElse(
      distributionManager.env.getPathToRunningExecutable
    )

  /**
    * Performs a step-by-step recursive upgrade.
    *
    * Finds a next version that can be directly upgraded to and is newer enough
    * to allow to upgrade to new versions, extracts it and runs it telling it to
    * continue upgrading to the target version. The extracted version may
    * download additional versions if more steps are needed.
    *
    * @param release release associated with the target version
    */
  private def performStepByStepUpgrade(release: LauncherRelease): Unit = {
    val availableVersions = releaseProvider.fetchAllValidVersions().get
    val nextStepRelease   = nextVersionToUpgradeTo(release, availableVersions)
    Logger.info(
      s"Cannot upgrade to ${release.version} directly, " +
      s"so a multiple-step upgrade will be performed, first upgrading to " +
      s"${nextStepRelease.version}."
    )

    val temporaryExecutable = temporaryExecutablePath(
      "new." + nextStepRelease.version.toString
    )
    FileSystem.withTemporaryDirectory("enso-upgrade-step") { directory =>
      Logger.info(s"Downloading ${nextStepRelease.packageFileName}.")
      val packagePath = directory / nextStepRelease.packageFileName
      nextStepRelease
        .downloadPackage(packagePath)
        .waitForResult(showProgress)
        .get

      Logger.info(
        s"Extracting the executable from ${nextStepRelease.packageFileName}."
      )
      extractExecutable(packagePath, temporaryExecutable)

      Logger.info(
        s"Upgraded to ${nextStepRelease.version}. " +
        s"Proceeding to the next step of the upgrade."
      )
      runNextUpgradeStep(temporaryExecutable, release.version)
    }
  }

  @scala.annotation.tailrec
  private def nextVersionToUpgradeTo(
    release: LauncherRelease,
    availableVersions: Seq[SemVer]
  ): LauncherRelease = {
    val recentEnoughVersions =
      availableVersions.filter(_ >= release.minimumVersionToPerformUpgrade)
    val minimumValidVersion = recentEnoughVersions.sorted.headOption.getOrElse {
      throw UpgradeError(
        s"Upgrade failed: To continue upgrade, a version at least " +
        s"${release.minimumVersionToPerformUpgrade} is required, but no " +
        s"valid version satisfying this requirement could be found."
      )
    }
    val nextRelease = releaseProvider.fetchRelease(minimumValidVersion).get
    Logger.debug(
      s"To upgrade to ${release.version}, " +
      s"the launcher will have to upgrade to ${nextRelease.version} first."
    )
    if (nextRelease.canPerformUpgradeFromCurrentVersion)
      nextRelease
    else nextVersionToUpgradeTo(nextRelease, availableVersions)
  }

  /**
    * Extracts just the launcher executable from the archive.
    *
    * @param archivePath path to the archive
    * @param executablePath path where to put the extracted executable
    */
  private def extractExecutable(
    archivePath: Path,
    executablePath: Path
  ): Unit = {
    var entryFound = false
    Archive
      .iterateArchive(archivePath) { entry =>
        if (
          entry.relativePath.endsWith(
            Path.of("bin") / OS.executableName("enso")
          )
        ) {
          entryFound = true
          entry.extractTo(executablePath)
          false
        } else true
      }
      .waitForResult(showProgress)
      .get
    if (!entryFound) {
      throw UpgradeError(
        s"Launcher executable was not found in `$archivePath`."
      )
    }
  }

  private val temporaryExecutablePrefix = "enso.tmp."

  private def isTemporaryExecutable(path: Path): Boolean =
    path.getFileName.toString.startsWith(temporaryExecutablePrefix)

  private def temporaryExecutablePath(suffix: String): Path = {
    val newName = OS.executableName(temporaryExecutablePrefix + suffix)
    val binRoot = originalExecutable.getParent
    binRoot / newName
  }

  private def copyNonEssentialFiles(
    extractedRoot: Path,
    release: LauncherRelease
  ): Unit =
    try {
      val dataRoot = distributionManager.paths.dataRoot
      for (file <- release.manifest.filesToCopy) {
        FileSystem.copyFile(
          extractedRoot / file,
          dataRoot / file
        )
      }
      for (dir <- release.manifest.directoriesToCopy) {
        val destination = dataRoot / dir
        FileSystem.removeDirectoryIfExists(destination)
        FileSystem.copyDirectory(extractedRoot / dir, destination)
      }
    } catch {
      case NonFatal(e) =>
        Logger.error(
          "An error occurred when copying one of the non-crucial files and " +
          "directories. The upgrade will continue, but the README or " +
          "licences may be out of date.",
          e
        )
    }

  private def performUpgradeTo(release: LauncherRelease): Unit = {
    FileSystem.withTemporaryDirectory("enso-upgrade") { directory =>
      Logger.info(s"Downloading ${release.packageFileName}.")
      val packagePath = directory / release.packageFileName
      release.downloadPackage(packagePath).waitForResult(showProgress).get

      Logger.info("Extracting package.")
      Archive
        .extractArchive(packagePath, directory, None)
        .waitForResult(showProgress)
        .get
      val extractedRoot = directory / "enso"

      val temporaryExecutable = temporaryExecutablePath("new")
      FileSystem.copyFile(
        extractedRoot / "bin" / OS.executableName("enso"),
        temporaryExecutable
      )

      copyNonEssentialFiles(extractedRoot, release)

      Logger.info("Replacing the old launcher executable with the new one.")
      replaceLauncherExecutable(temporaryExecutable)

      val verb =
        if (release.version >= CurrentVersion.version) "upgraded"
        else "downgraded"
      Logger.info(s"Successfully $verb launcher to ${release.version}.")
    }
  }

  /**
    * Replaces the current launcher executable with a new one.
    *
    * On UNIX systems, it just removes the old one and moves the new one in its
    * place.
    *
    * On Windows, the currently running executable cannot be deleted, so instead
    * it is renamed to a different name, so that the new one can be moved in its
    * place. The old executable is removed later when cleanup is run.
    *
    * @param newExecutable path to the new executable that will replace the old
    *                      one
    */
  private def replaceLauncherExecutable(newExecutable: Path): Unit = {
    Logger.debug(s"Replacing $originalExecutable with $newExecutable")
    if (OS.isWindows) {
      val oldName = temporaryExecutablePath(s"old-${CurrentVersion.version}")
      Files.move(originalExecutable, oldName)
      Files.move(newExecutable, originalExecutable)
    } else {
      Files.delete(originalExecutable)
      Files.move(newExecutable, originalExecutable)
    }
  }

}

object LauncherUpgrader {

  /**
    * Creates a [[LauncherUpgrader]] using the default [[DistributionManager]]
    * and release providers.
    *
    * Should be run late enough so that the testing repository override can be
    * applied. It is enough to run it inside of the standard options parsing.
    *
    * @param globalCLIOptions options from the CLI setting verbosity of the
    *                         executed actions
    * @param originalExecutablePath specifies the path of the original launcher
    *                               executable that will be replaced in the last
    *                               step of the upgrade
    */
  def makeDefault(
    globalCLIOptions: GlobalCLIOptions,
    originalExecutablePath: Option[Path] = None
  ): LauncherUpgrader =
    new LauncherUpgrader(
      globalCLIOptions,
      DistributionManager,
      EnsoRepository.defaultLauncherReleaseProvider,
      originalExecutablePath
    )

  def recoverUpgradeRequiredErrors(originalArguments: Array[String])(
    action: => Int
  ): Int = {
    try {
      action
    } catch {
      case e: LauncherUpgradeRequiredError =>
        val autoConfirm = e.globalCLIOptions.autoConfirm
        def shouldProceed: Boolean =
          if (autoConfirm) {
            Logger.warn(
              "A more recent launcher version is required. Since " +
              "`auto-confirm` is set, the launcher upgrade will be peformed " +
              "automatically."
            )
            true
          } else {
            Logger.warn("A more recent launcher version is required.")
            CLIOutput.askConfirmation(
              "Do you want to upgrade the launcher and continue?",
              yesDefault = true
            )
          }

        if (!shouldProceed) {
          throw e
        }

        val upgrader           = makeDefault(e.globalCLIOptions)
        val targetVersion      = upgrader.latestVersion().get
        val launcherExecutable = upgrader.originalExecutable
        upgrader.upgrade(targetVersion)

        Logger.info(
          "Re-running the current command with the upgraded launcher."
        )

        val arguments =
          InternalOpts.removeInternalTestOptions(originalArguments.toIndexedSeq)
        val rerunCommand =
          Seq(launcherExecutable.toAbsolutePath.normalize.toString) ++ arguments
        Logger.debug(s"Running `${rerunCommand.mkString(" ")}`.")
        val processBuilder = new ProcessBuilder(rerunCommand: _*)
        val process        = processBuilder.inheritIO().start()
        process.waitFor()
    }
  }
}
