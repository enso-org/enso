package org.enso.launcher.installation

import java.nio.file.{Files, Path}

import org.apache.commons.io.FileUtils
import org.enso.cli.CLIOutput
import org.enso.launcher.FileSystem.PathSyntax
import org.enso.launcher.cli.InternalOpts
import org.enso.launcher.{FileSystem, GlobalConfigurationManager, Logger, OS}

/**
  * Allows to [[uninstall]] an installed distribution.
  *
  * @param manager a distribution manager instance which defines locations for
  *                the distribution that will be uninstalled
  * @param autoConfirm if set to true, the uninstaller will use defaults
  *                    instead of asking questions
  */
class DistributionUninstaller(
  manager: DistributionManager,
  autoConfirm: Boolean
) {

  /**
    * Uninstalls a locally installed (non-portable) distribution.
    *
    * Removes the launcher executable and the ENSO_DATA_DIRECTORY and
    * ENSO_CONFIG_DIRECTORY directories (unless they contain unexpected files).
    * If unexpected files are encountered and [[autoConfirm]] is set, the files
    * are preserved and the directories are not removed (but the expected
    * contents are cleaned anyway), otherwise the program asks if the unexpected
    * files should be removed.
    */
  def uninstall(): Unit = {
    checkPortable()
    askConfirmation()
    if (OS.isWindows) uninstallWindows()
    else uninstallUNIX()
  }

  /**
    * Uninstall strategy for OSes that can remove running executables.
    *
    * Simply removes each component, starting with the ones that can potentially
    * be nested (as config and the binary can be inside of the data directory
    * if the user wishes so).
    */
  private def uninstallUNIX(): Unit = {
    uninstallConfig()
    uninstallExecutableUNIX()
    uninstallDataContents(deferDataRootRemoval = false)
    Logger.info("Successfully uninstalled the distribution.")
  }

  /**
    * Uninstall strategy for Windows, where it is not possible to remove a
    * running executable.
    *
    * The executable has to be removed last as the program must terminate to do
    * so. If the executable is inside of the data directory, the directory is
    * cleaned, but its removal is deferred and it will be removed just after the
    * executable at the end.
    */
  private def uninstallWindows(): Unit = {
    val deferRootRemoval = isBinaryInsideData
    uninstallConfig()
    uninstallDataContents(deferRootRemoval)
    Logger.info(
      "Successfully uninstalled the distribution but for the launcher " +
      "executable. It will be removed in a moment after this program " +
      "terminates."
    )
    uninstallExecutableWindows(
      if (deferRootRemoval) Some(manager.paths.dataRoot) else None
    )
  }

  /**
    * Checks if the launcher is running in portable mode and terminates
    * execution if it does.
    *
    * It prints an explanation that uninstall can only be used with non-portable
    * mode and a tip on how the portable distribution can be easily removed
    * manually.
    */
  private def checkPortable(): Unit = {
    if (manager.isRunningPortable) {
      Logger.warn(
        "The Enso distribution you are currently running is in portable " +
        "mode, so it cannot be uninstalled."
      )
      Logger.info(
        s"If you still want to remove it, you can just remove the " +
        s"`${manager.paths.dataRoot}` directory."
      )
      sys.exit(1)
    }
  }

  /**
    * Prints an explanation of what will be uninstalled and which directories
    * will be removed and asks the user if they want to proceed.
    */
  private def askConfirmation(): Unit = {
    Logger.info(
      s"Uninstalling this distribution will remove the launcher located at " +
      s"`${manager.env.getPathToRunningExecutable}`, all engine and runtime " +
      s"components and configuration managed by this distribution."
    )
    Logger.info(
      s"ENSO_DATA_DIRECTORY (${manager.paths.dataRoot}) and " +
      s"ENSO_CONFIG_DIRECTORY (${manager.paths.config}) will be removed " +
      s"unless they contain unexpected files."
    )
    if (!autoConfirm) {
      val proceed =
        CLIOutput.askConfirmation("Do you want to proceed?", yesDefault = true)
      if (!proceed) {
        Logger.warn("Installation has been cancelled on user request.")
        sys.exit(1)
      }
    }
  }

  /**
    * True if the currently running executable is inside of the data root.
    *
    * This is checked, because on Windows this will make removing data root more
    * complicated.
    */
  private def isBinaryInsideData: Boolean = {
    val binaryPath =
      manager.env.getPathToRunningExecutable.toAbsolutePath.normalize
    val dataPath = manager.paths.dataRoot.toAbsolutePath.normalize
    binaryPath.startsWith(dataPath)
  }

  /**
    * Removes the configuration file and the ENSO_CONFIG_DIRECTORY if it does
    * not contain any other files (or if the user agreed to remove them too).
    */
  private def uninstallConfig(): Unit = {
    FileSystem.removeFileIfExists(
      manager.paths.config / GlobalConfigurationManager.globalConfigName
    )
    val remaining =
      FileSystem.listDirectory(manager.paths.config).map(_.getFileName.toString)
    handleRemainingFiles(
      manager.LocallyInstalledDirectories.ENSO_CONFIG_DIRECTORY,
      manager.paths.config,
      remaining
    )
    FileSystem.removeDirectoryIfEmpty(manager.paths.config)
  }

  /**
    * Files that are expected to be inside of the data root.
    */
  private val knownDataFiles = Seq("README.md", "NOTICE")

  /**
    * Directories that are expected to be inside of the data root.
    */
  private val knownDataDirectories = Seq("tmp", "components-licences", "config")

  /**
    * Removes all files contained in the ENSO_DATA_DIRECTORY and possibly the
    * directory itself.
    *
    * If `deferDataRootRemoval` is set, the directory itself is not removed
    * because removing the `bin` directory may block this action. Other files
    * and directories are removed nonetheless,so only the `bin` directory and
    * the root itself are removed at the end.
    */
  private def uninstallDataContents(deferDataRootRemoval: Boolean): Unit = {
    FileSystem.removeDirectory(manager.paths.engines)
    FileSystem.removeDirectory(manager.paths.runtimes)

    val dataRoot = manager.paths.dataRoot
    for (dirName <- knownDataDirectories) {
      FileSystem.removeDirectoryIfExists(dataRoot / dirName)
    }
    for (fileName <- knownDataFiles) {
      FileSystem.removeFileIfExists(dataRoot / fileName)
    }

    if (!deferDataRootRemoval) {
      val nestedBinDirectory = dataRoot / "bin"
      if (Files.exists(nestedBinDirectory))
        FileSystem.removeDirectoryIfEmpty(nestedBinDirectory)
    }

    val ignoredFiles = if (deferDataRootRemoval) Set("bin") else Set()
    val remainingFiles = FileSystem
        .listDirectory(dataRoot)
        .map(_.getFileName.toString)
        .toSet -- ignoredFiles
    if (remainingFiles.nonEmpty) {
      handleRemainingFiles(
        manager.LocallyInstalledDirectories.ENSO_DATA_DIRECTORY,
        dataRoot.toAbsolutePath.normalize,
        remainingFiles.toSeq
      )
    }

    if (!deferDataRootRemoval) {
      FileSystem.removeDirectoryIfEmpty(dataRoot)
    }
  }

  /**
    * Common logic for handling unexpected files in ENSO_DATA_DIRECTORY and
    * ENSO_CONFIG_DIRECTORY.
    *
    * It asks the user if they want to remove these files unless `auto-confirm`
    * is set, in which case it just prints a warning.
    *
    * @param directoryName name of the directory
    * @param path path to the directory
    * @param remainingFiles sequence of filenames that are present in that
    *                       directory but are not expected
    */
  private def handleRemainingFiles(
    directoryName: String,
    path: Path,
    remainingFiles: Seq[String]
  ): Unit =
    if (remainingFiles.nonEmpty) {
      def remainingFilesList =
        remainingFiles.map(fileName => s"`$fileName`").mkString(", ")
      if (autoConfirm) {
        Logger.warn(
          s"$directoryName ($path) contains unexpected files: " +
          s"$remainingFilesList, so it will not be removed."
        )
      } else {
        Logger.warn(
          s"$directoryName ($path) contains unexpected files: " +
          s"$remainingFilesList."
        )
        def confirmation =
          CLIOutput.askConfirmation(
            s"Do you want to remove the $directoryName containing these files?"
          )
        if (confirmation) {
          for (fileName <- remainingFiles) {
            FileUtils.forceDelete((path / fileName).toFile)
          }
        }
      }
    }

  /**
    * Uninstalls the executable on platforms that allow for removing running
    * files.
    *
    * Simply removes the file.
    */
  private def uninstallExecutableUNIX(): Unit = {
    FileSystem.removeFileIfExists(manager.env.getPathToRunningExecutable)
  }

  /**
    * Uninstalls the executable on Windows where it is impossible to remove an
    * executable that is running.
    *
    * Uses a workaround implemented in [[InternalOpts]]. Has to be run at the
    * very end as it has to terminate the current executable.
    */
  private def uninstallExecutableWindows(
    parentToRemove: Option[Path]
  ): Nothing = {
    val temporaryLauncher =
      Files.createTempDirectory("enso-uninstall") / OS.executableName("enso")
    val oldLauncher = manager.env.getPathToRunningExecutable
    Files.copy(oldLauncher, temporaryLauncher)
    InternalOpts
      .runWithNewLauncher(temporaryLauncher)
      .finishUninstall(oldLauncher, parentToRemove)
  }
}
