package org.enso.launcher.upgrade

import java.nio.file.{AccessDeniedException, Files, Path}
import com.typesafe.scalalogging.Logger
import org.enso.semver.SemVer
import org.enso.semver.SemVerOrdering._
import org.enso.cli.{CLIOutput, OS}
import org.enso.distribution.{DistributionManager, FileSystem}
import org.enso.distribution.locking.{
  LockType,
  LockUserInterface,
  Resource,
  ResourceManager
}
import org.enso.runtimeversionmanager.CurrentVersion
import org.enso.distribution.FileSystem.PathSyntax
import org.enso.downloader.archive.Archive
import org.enso.runtimeversionmanager.components.UpgradeRequiredError
import org.enso.launcher.cli.{
  CLIProgressReporter,
  GlobalCLIOptions,
  InternalOpts
}
import org.enso.launcher.releases.launcher.LauncherRelease
import org.enso.runtimeversionmanager.releases.ReleaseProvider
import org.enso.launcher.releases.LauncherRepository
import org.enso.launcher.{Constants, InfoLogger}
import org.enso.launcher.distribution.DefaultManagers
import org.enso.runtimeversionmanager.locking.Resources
import org.slf4j.LoggerFactory

import scala.util.Try
import scala.util.control.NonFatal

class LauncherUpgrader(
  globalCLIOptions: GlobalCLIOptions,
  distributionManager: DistributionManager,
  releaseProvider: ReleaseProvider[LauncherRelease],
  resourceManager: ResourceManager,
  originalExecutablePath: Option[Path]
) {

  private val logger           = Logger[LauncherUpgrader]
  private val progressReporter = CLIProgressReporter(globalCLIOptions)

  /** Queries the release provider for the latest available valid launcher
    * version.
    */
  def latestVersion(): Try[SemVer] = {
    releaseProvider.findLatestVersion()
  }

  /** Performs an upgrade to the `targetVersion`.
    *
    * The upgrade may first temporarily install versions older than the target
    * if the upgrade cannot be performed directly from the current version.
    *
    * If another upgrade is in progress, [[AnotherUpgradeInProgressError]] is
    * thrown.
    */
  def upgrade(targetVersion: SemVer): Unit = {
    val failIfAnotherUpgradeIsRunning = new LockUserInterface {
      override def startWaitingForResource(resource: Resource): Unit =
        throw AnotherUpgradeInProgressError()

      override def finishWaitingForResource(resource: Resource): Unit = ()
    }
    resourceManager.withResource(
      failIfAnotherUpgradeIsRunning,
      Resources.LauncherExecutable,
      LockType.Exclusive
    ) {
      runCleanup(isStartup = true)
      val release = releaseProvider.fetchRelease(targetVersion).get
      if (release.isMarkedBroken) {
        if (globalCLIOptions.autoConfirm) {
          logger.warn(
            s"The launcher release $targetVersion is marked as broken and it " +
            s"should not be used. Since `auto-confirm` is set, the upgrade " +
            s"will continue, but you may want to reconsider upgrading to a " +
            s"stable release."
          )
        } else {
          logger.warn(
            s"The launcher release $targetVersion is marked as broken and it " +
            s"should not be used."
          )
          val continue = CLIOutput.askConfirmation(
            "Are you sure you still want to continue upgrading to this " +
            "version despite the warning?"
          )
          if (!continue) {
            throw UpgradeError(
              "Upgrade has been cancelled by the user because the requested " +
              "version is marked as broken."
            )
          }
        }
      }

      val canPerformDirectUpgrade: Boolean =
        if (release.canPerformUpgradeFromCurrentVersion) true
        else if (CurrentVersion.isDevVersion) {
          logger.warn(
            s"Cannot upgrade to version ${release.version} directly, because " +
            s"it requires at least version " +
            s"${release.minimumVersionToPerformUpgrade}."
          )
          if (globalCLIOptions.autoConfirm) {
            logger.warn(
              s"However, the current version (${CurrentVersion.version}) is " +
              s"a development version, so the minimum version check can be " +
              s"ignored. Since `auto-confirm` is set, the upgrade will " +
              s"continue. But please be warned that it may fail due to " +
              s"incompatibility."
            )
            true
          } else {
            logger.warn(
              s"Since the current version (${CurrentVersion.version}) is " +
              s"a development version, the minimum version check can be " +
              s"ignored. However, please be warned that the upgrade " +
              s"may fail due to incompatibility."
            )
            CLIOutput.askConfirmation(
              "Do you want to continue upgrading to this version " +
              "despite the warning?"
            )
          }
        } else false

      if (canPerformDirectUpgrade)
        performUpgradeTo(release)
      else
        performStepByStepUpgrade(release)

      runCleanup()
      logger.debug("Upgrade completed successfully.")
    }
  }

  /** Cleans up temporary and old launcher executables.
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
      logger.debug(
        s"Cleaning ${temporaryFiles.size} temporary files from a previous upgrade."
      )
    }
    for (file <- temporaryFiles) {
      try {
        tryHardToDelete(file)
        logger.debug(s"Upgrade cleanup: removed `$file`.")
      } catch {
        case NonFatal(e) =>
          logger.debug(s"Cannot remove temporary file $file: $e", e)
      }
    }
  }

  /** On Windows, deleting an executable immediately after it has exited may fail
    * and the process may need to wait a few millisecond. This method detects
    * this kind of failure and retries a few times.
    */
  private def tryHardToDelete(file: Path, attempts: Int = 30): Unit = {
    try {
      Files.delete(file)
    } catch {
      case _: AccessDeniedException if attempts > 0 =>
        logger.trace(s"Failed to delete file `$file`. Retrying.")
        Thread.sleep(100)
        tryHardToDelete(file, attempts - 1)
    }
  }

  /** Continues a multi-step upgrade.
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

  /** Run the next step of the upgrade using the newly extracted newer launcher
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

  /** Path to the original launcher executable.
    */
  private val originalExecutable =
    originalExecutablePath.getOrElse(
      distributionManager.env.getPathToRunningExecutable
    )

  /** Performs a step-by-step recursive upgrade.
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
    InfoLogger.info(
      s"Cannot upgrade to ${release.version} directly, " +
      s"so a multiple-step upgrade will be performed, first upgrading to " +
      s"${nextStepRelease.version}."
    )

    val temporaryExecutable = temporaryExecutablePath(
      "new." + nextStepRelease.version.toString
    )
    FileSystem.withTemporaryDirectory("enso-upgrade-step") { directory =>
      val packagePath  = directory / nextStepRelease.packageFileName
      val downloadTask = nextStepRelease.downloadPackage(packagePath)
      progressReporter.trackProgress(
        s"Downloading ${nextStepRelease.packageFileName}.",
        downloadTask
      )
      downloadTask.force()

      extractExecutable(packagePath, temporaryExecutable)

      InfoLogger.info(
        s"Upgraded to ${nextStepRelease.version}. " +
        s"Proceeding to the next step of the upgrade."
      )
      runNextUpgradeStep(temporaryExecutable, release.version)
    }
  }

  @scala.annotation.tailrec
  private def nextVersionToUpgradeTo(
    currentTargetRelease: LauncherRelease,
    availableVersions: Seq[SemVer]
  ): LauncherRelease = {
    assert(
      currentTargetRelease.minimumVersionToPerformUpgrade.isGreaterThan(
        CurrentVersion.version
      )
    )

    // We look at older versions that are satisfying the minimum version
    // required to upgrade to currentTargetRelease.
    val recentEnoughVersions =
      availableVersions.filter { possibleVersion =>
        val canUpgradeToTarget = possibleVersion.isGreaterThanOrEqual(
          currentTargetRelease.minimumVersionToPerformUpgrade
        )
        val isEarlierThanTarget =
          possibleVersion.isLessThan(currentTargetRelease.version)
        canUpgradeToTarget && isEarlierThanTarget
      }

    // We take the oldest of these, hoping that it will yield the shortest
    // upgrade path (perhaps it will be possible to upgrade directly from
    // current version)
    val minimumValidVersion = recentEnoughVersions.sorted.headOption.getOrElse {
      throw UpgradeError(
        s"Upgrade failed: To continue upgrade, at least version " +
        s"${currentTargetRelease.minimumVersionToPerformUpgrade} is required, " +
        s"but no upgrade path has been found from the current version " +
        s"${CurrentVersion.version}. " +
        s"Please manually download a newer release from " +
        s"${LauncherRepository.websiteUrl}"
      )
    }

    val newTargetRelease = releaseProvider.fetchRelease(minimumValidVersion).get
    assert(newTargetRelease.version != currentTargetRelease.version)
    logger.debug(
      s"To upgrade to ${currentTargetRelease.version}, " +
      s"the launcher will have to upgrade to ${newTargetRelease.version} first."
    )

    // If the current version cannot upgrade directly to the new target version,
    // we continue the search looking for an even earlier version that we could
    // upgrade to.
    if (newTargetRelease.canPerformUpgradeFromCurrentVersion) newTargetRelease
    else nextVersionToUpgradeTo(newTargetRelease, availableVersions)
  }

  /** Extracts just the launcher executable from the archive.
    *
    * @param archivePath path to the archive
    * @param executablePath path where to put the extracted executable
    */
  private def extractExecutable(
    archivePath: Path,
    executablePath: Path
  ): Unit = {
    var entryFound = false
    val extractTask = Archive
      .iterateArchive(archivePath) { entry =>
        if (
          entry.relativePath.endsWith(
            Path.of("bin") / OS.executableName(org.enso.launcher.Constants.name)
          )
        ) {
          entryFound = true
          entry.extractTo(executablePath)
          false
        } else true
      }
    progressReporter.trackProgress(
      s"Extracting the executable from ${archivePath.getFileName}.",
      extractTask
    )
    extractTask.force()
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
        logger.error(
          "An error occurred when copying one of the non-crucial files and " +
          "directories. The upgrade will continue, but the README or " +
          "licences may be out of date.",
          e
        )
    }

  private def performUpgradeTo(release: LauncherRelease): Unit = {
    FileSystem.withTemporaryDirectory("enso-upgrade") { directory =>
      val packagePath  = directory / release.packageFileName
      val downloadTask = release.downloadPackage(packagePath)
      progressReporter.trackProgress(
        s"Downloading ${release.packageFileName}.",
        downloadTask
      )
      downloadTask.force()

      val extractTask = Archive.extractArchive(packagePath, directory, None)
      progressReporter.trackProgress("Extracting package.", extractTask)
      extractTask.force()

      val extractedRoot = directory / "enso"

      val temporaryExecutable = temporaryExecutablePath("new")
      FileSystem.copyFile(
        extractedRoot / "bin" / OS.executableName(Constants.name),
        temporaryExecutable
      )

      copyNonEssentialFiles(extractedRoot, release)

      InfoLogger.info("Replacing the old launcher executable with the new one.")
      replaceLauncherExecutable(temporaryExecutable)

      val verb =
        if (release.version.isGreaterThanOrEqual(CurrentVersion.version))
          "upgraded"
        else "downgraded"
      InfoLogger.info(s"Successfully $verb the launcher to ${release.version}.")
    }
  }

  /** Replaces the current launcher executable with a new one.
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
    logger.debug(s"Replacing $originalExecutable with $newExecutable")
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

  /** Creates a [[LauncherUpgrader]] using the default [[DistributionManager]]
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
  def default(
    globalCLIOptions: GlobalCLIOptions,
    originalExecutablePath: Option[Path] = None
  ): LauncherUpgrader =
    new LauncherUpgrader(
      globalCLIOptions,
      DefaultManagers.distributionManager,
      LauncherRepository.defaultLauncherReleaseProvider,
      DefaultManagers.defaultResourceManager,
      originalExecutablePath
    )

  /** Wraps an action and intercepts the [[UpgradeRequiredError]]
    * offering to upgrade the launcher and re-run the command with the newer
    * version.
    *
    * @param originalArguments original CLI arguments, needed to be able to
    *                          re-run the command
    * @param action action that is executed and may throw the exception; it
    *               should return the desired exit code
    * @return if `action` succeeds, its exit code is returned; otherwise if the
    *         [[UpgradeRequiredError]] is intercepted and an upgrade is
    *         performed, the exit code of the command that has been re-executed
    *         is returned
    */
  def recoverUpgradeRequiredErrors(originalArguments: Array[String])(
    action: => Int
  ): Int = {
    try {
      action
    } catch {
      case upgradeRequiredError: UpgradeRequiredError =>
        askToUpgrade(upgradeRequiredError, originalArguments)
    }
  }

  private var cachedCLIOptions: Option[GlobalCLIOptions] = None
  def setCLIOptions(globalCLIOptions: GlobalCLIOptions): Unit =
    cachedCLIOptions = Some(globalCLIOptions)

  private def askToUpgrade(
    upgradeRequiredError: UpgradeRequiredError,
    originalArguments: Array[String]
  ): Int = {
    val logger = LoggerFactory.getLogger(
      classOf[LauncherUpgrader]
    )
    val globalCLIOptions = cachedCLIOptions.getOrElse(
      throw new IllegalStateException(
        "Upgrade requested but application was not initialized properly."
      )
    )
    def shouldProceed: Boolean =
      if (globalCLIOptions.autoConfirm) {
        logger.warn(
          "A more recent launcher version is required. Since `auto-confirm` " +
          "is set, the launcher upgrade will be peformed automatically."
        )
        true
      } else {
        logger.warn(
          s"A more recent launcher version (at least " +
          s"${upgradeRequiredError.expectedVersion}) is required to " +
          s"continue."
        )
        CLIOutput.askConfirmation(
          "Do you want to upgrade the launcher and continue?",
          yesDefault = true
        )
      }

    if (!shouldProceed) {
      throw upgradeRequiredError
    }

    val upgrader           = default(globalCLIOptions)
    val targetVersion      = upgrader.latestVersion().get
    val launcherExecutable = upgrader.originalExecutable
    try {
      upgrader.upgrade(targetVersion)

      InfoLogger.info(
        "Re-running the current command with the upgraded launcher."
      )

      val arguments =
        InternalOpts.removeInternalTestOptions(originalArguments.toIndexedSeq)
      val rerunCommand =
        Seq(launcherExecutable.toAbsolutePath.normalize.toString) ++ arguments
      logger.debug(s"Running `${rerunCommand.mkString(" ")}`.")
      val processBuilder = new ProcessBuilder(rerunCommand: _*)
      val process        = processBuilder.inheritIO().start()
      process.waitFor()
    } catch {
      case _: AnotherUpgradeInProgressError =>
        logger.error(
          "Another upgrade is in progress." +
          "Please wait for it to finish and manually re-run the requested " +
          "command."
        )
        1
    }
  }
}
