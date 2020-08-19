package org.enso.launcher.installation

import java.nio.file.Path

import org.apache.commons.io.FileUtils
import org.enso.cli.CLIOutput
import org.enso.launcher.FileSystem.PathSyntax
import org.enso.launcher.{FileSystem, GlobalConfigurationManager, Logger, OS}

class DistributionUninstaller(
  manager: DistributionManager,
  autoConfirm: Boolean
) {
  def uninstall(): Unit = {
    checkPortable()
    askConfirmation()
    val deferDataRootRemoval = isBinaryInsideData && OS.isWindows
    uninstallConfig()
    uninstallDataContents(deferDataRootRemoval)
    val parentRootToRemove =
      if (deferDataRootRemoval) Some(manager.paths.dataRoot) else None
    uninstallBin(parentRootToRemove)
  }

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

  private def isBinaryInsideData: Boolean = {
    val binaryPath =
      manager.env.getPathToRunningExecutable.toAbsolutePath.normalize
    val dataPath = manager.paths.dataRoot.toAbsolutePath.normalize
    binaryPath.startsWith(dataPath)
  }

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

  private val knownDataFiles       = Seq("README.md", "NOTICE")
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

  private def handleRemainingFiles(
    directoryName: String,
    path: Path,
    remainingFiles: Seq[String]
  ): Unit = {
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

  private def uninstallBin(parentToRemove: Option[Path]): Unit = {
    if (OS.isWindows) {
      // TODO workaround
    } else {
      FileSystem.removeFileIfExists(manager.env.getPathToRunningExecutable)
      parentToRemove.foreach { parent =>
        FileSystem.removeDirectoryIfExists(parent / "bin")
        FileSystem.removeDirectoryIfEmpty(parent)
      }
    }
  }
}
