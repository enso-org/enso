package org.enso.launcher.upgrade

import java.nio.file.{Files, Path}

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.archive.Archive
import org.enso.launcher.cli.GlobalCLIOptions
import org.enso.launcher.{CurrentVersion, FileSystem, Logger, OS}
import org.enso.launcher.installation.DistributionManager
import org.enso.launcher.releases.ReleaseProvider
import org.enso.launcher.releases.launcher.LauncherRelease

import scala.util.Try
import scala.util.control.NonFatal

class LauncherUpgrader(
  globalCLIOptions: GlobalCLIOptions,
  distributionManager: DistributionManager,
  releaseProvider: ReleaseProvider[LauncherRelease]
) {
  def latestVersion(): Try[SemVer] =
    releaseProvider.findLatestVersion()

  def upgrade(targetVersion: SemVer): Unit = {
    val release = releaseProvider.fetchRelease(targetVersion).get
    if (release.canPerformUpgradeFromCurrentVersion)
      performUpgradeTo(release)
    else performStepByStepUpgrade(release)
  }

  private def performStepByStepUpgrade(release: LauncherRelease): Unit = {
    val availableVersions = releaseProvider.fetchAllValidVersions().get
    val nextVersion       = nextVersionToUpgradeTo(release, availableVersions)
    ??? // TODO
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
    if (nextRelease.canPerformUpgradeFromCurrentVersion)
      nextRelease
    else nextVersionToUpgradeTo(nextRelease, availableVersions)
  }

  def continueUpgrade(targetVersion: SemVer): Unit = {
    ??? // TODO
  }

  def internalRunCleanup(): Unit = {
    // TODO when this is called ?
    val binRoot = distributionManager.env.getPathToRunningExecutable.getParent
    val temporaryFiles =
      FileSystem.listDirectory(binRoot).filter(isTemporaryExecutable)
    for (file <- temporaryFiles) {
      // TODO this may require retrying to ensure the file is not locked
      try {
        Files.delete(file)
        Logger.debug(s"After upgrade cleanup: removed `$file`.")
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
    val binRoot = distributionManager.env.getPathToRunningExecutable.getParent
    binRoot.resolve(newName)
  }

  private def copyNonEssentialFiles(
    extractedRoot: Path,
    release: LauncherRelease
  ): Unit =
    try {
      val dataRoot = distributionManager.paths.dataRoot
      for (file <- release.manifest.filesToCopy) {
        FileSystem.copyFile(
          extractedRoot.resolve(file),
          dataRoot.resolve(file)
        )
      }
      for (dir <- release.manifest.directoriesToCopy) {
        val destination = dataRoot.resolve(dir)
        FileSystem.removeDirectoryIfExists(destination)
        FileSystem.copyDirectory(extractedRoot.resolve(dir), destination)
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
      val packagePath = directory.resolve(release.packageFileName)
      release.downloadPackage(packagePath).waitForResult().get

      val extractedRoot = directory
      Archive.extractArchive(packagePath, extractedRoot, None)

      val temporaryExecutable = temporaryExecutablePath("new")
      FileSystem.copyFile(
        extractedRoot.resolve("bin").resolve(OS.executableName("enso")),
        temporaryExecutable
      )

      copyNonEssentialFiles(extractedRoot, release)

      replaceLauncherExecutable(temporaryExecutable)
      Logger.info(s"Successfully upgraded launcher to ${release.version}.")
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
    */
  private def replaceLauncherExecutable(newExecutable: Path): Unit = {
    val currentExecutable = distributionManager.env.getPathToRunningExecutable
    if (OS.isWindows) {
      val oldName = temporaryExecutablePath(s"old-${CurrentVersion.version}")
      Files.move(currentExecutable, oldName)
      Files.move(newExecutable, currentExecutable)
    } else {
      Files.delete(currentExecutable)
      Files.move(newExecutable, currentExecutable)
    }
  }

}
