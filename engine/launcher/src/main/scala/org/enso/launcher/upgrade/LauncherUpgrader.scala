package org.enso.launcher.upgrade

import java.nio.file.{Files, Path}

import nl.gn0s1s.bump.SemVer
import org.enso.cli.CLIOutput
import org.enso.launcher.FileSystem.PathSyntax
import org.enso.launcher.archive.Archive
import org.enso.launcher.cli.{GlobalCLIOptions, InternalOpts}
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
  def latestVersion(): Try[SemVer] = {
    releaseProvider.findLatestVersion()
  }

  def upgrade(targetVersion: SemVer): Unit = {
    internalRunCleanup(isStartup = true)
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

    if (!OS.isWindows) {
      internalRunCleanup()
    }
  }

  def continueUpgrade(targetVersion: SemVer): Unit = {
    val release = releaseProvider.fetchRelease(targetVersion).get
    if (release.canPerformUpgradeFromCurrentVersion)
      performUpgradeTo(release)
    else
      performStepByStepUpgrade(release)
  }

  private def runNextUpgradeStep(
    temporaryExecutable: Path,
    targetVersion: SemVer
  ): Unit = {
    val exitCode = InternalOpts
      .runWithNewLauncher(temporaryExecutable)
      .continueUpgrade(
        targetVersion    = targetVersion,
        originalPath     = currentExecutable,
        globalCLIOptions = globalCLIOptions
      )
    if (exitCode != 0) {
      throw UpgradeError("Next upgrade step has failed. Upgrade cancelled.")
    }
  }

  private def showProgress = !globalCLIOptions.hideProgress

  private val currentExecutable =
    originalExecutablePath.getOrElse(
      distributionManager.env.getPathToRunningExecutable
    )

  private def performStepByStepUpgrade(release: LauncherRelease): Unit = {
    val availableVersions = releaseProvider.fetchAllValidVersions().get
    val nextStepRelease   = nextVersionToUpgradeTo(release, availableVersions)
    Logger.info(
      s"Cannot upgrade to ${release.version} directly, " +
      s"multiple-step upgrade will be peformed, first upgrading to " +
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
        s"Upgraded to ${nextStepRelease.version}, " +
        s"proceeding to the next step of the upgrade."
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
        s"Upgrade failed: To continue upgrade, version at least " +
        s"${release.minimumVersionToPerformUpgrade} is required, but no " +
        s"valid version satisfying this requirement could be found."
      )
    }
    val nextRelease = releaseProvider.fetchRelease(minimumValidVersion).get
    Logger.debug(
      s"To upgrade to ${release.version}, " +
      s"will have to upgrade to ${nextRelease.version} first."
    )
    if (nextRelease.canPerformUpgradeFromCurrentVersion)
      nextRelease
    else nextVersionToUpgradeTo(nextRelease, availableVersions)
  }

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

  def internalRunCleanup(isStartup: Boolean = false): Unit = {
    // TODO when this is called ?
    val binRoot = currentExecutable.getParent
    val temporaryFiles =
      FileSystem.listDirectory(binRoot).filter(isTemporaryExecutable)
    if (temporaryFiles.nonEmpty && isStartup) {
      Logger.info("Cleaning temporary files from a previous upgrade.")
    }
    for (file <- temporaryFiles) {
      // TODO this may require retrying to ensure the file is not locked
      try {
        Files.delete(file)
        Logger.debug(s"Upgrade cleanup: removed `$file`.")
      } catch {
        case NonFatal(e) =>
          Logger.error(s"Cannot remove temporary file $file: $e", e)
      }
    }
  }

  private val temporaryExecutablePrefix = "enso.tmp."

  private def isTemporaryExecutable(path: Path): Boolean =
    path.getFileName.toString.startsWith(temporaryExecutablePrefix)

  private def temporaryExecutablePath(suffix: String): Path = {
    val newName = OS.executableName(temporaryExecutablePrefix + suffix)
    val binRoot = currentExecutable.getParent
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

      Logger.info("Replacing old launcher executable with the new one.")
      replaceLauncherExecutable(temporaryExecutable, keepForRollback = false)
      Logger.info(s"Successfully upgraded launcher to ${release.version}.")
    }
  }

  private def rollbackPath = temporaryExecutablePath("rollback")

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
    * @param keepForRollback if true, the old executable is retained for possible rollback
    */
  private def replaceLauncherExecutable(
    newExecutable: Path,
    keepForRollback: Boolean
  ): Unit = {
    Logger.debug(s"Replacing $currentExecutable with $newExecutable")
    if (OS.isWindows || keepForRollback) {
      val oldName =
        if (keepForRollback) rollbackPath
        else temporaryExecutablePath(s"old-${CurrentVersion.version}")
      Files.move(currentExecutable, oldName)
      Files.move(newExecutable, currentExecutable)
    } else {
      Files.delete(currentExecutable)
      Files.move(newExecutable, currentExecutable)
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
}
