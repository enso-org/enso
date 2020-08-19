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
    val deferDataRootRemoval = isBinaryInsideData && OS.isWindows
    uninstallConfig()
    uninstallDataContents(deferDataRootRemoval)
    val parentRootToRemove =
      if (deferDataRootRemoval) Some(manager.paths.dataRoot) else None
    uninstallBin(parentRootToRemove)
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
    FileSystem.removeDirectoryIfEmpty(manager.paths.config)
  }

  private val knownDataFiles       = Seq("README.md", "NOTICE", ".enso.portable")
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
      def remainingFilesList =
        remainingFiles.toSeq.map(fileName => s"`$fileName`").mkString(", ")
      Logger.warn(
        s"ENSO_DATA_DIRECTORY (${dataRoot.toAbsolutePath.normalize} contains " +
        s"unexpected files: $remainingFilesList."
      )

      def askForConfirmation(): Boolean =
        CLIOutput.askConfirmation(
          "Do you want to remove the ENSO_DATA_DIRECTORY containing these " +
          "files?"
        )

      if (autoConfirm || askForConfirmation()) {
        for (fileName <- remainingFiles) {
          FileUtils.forceDelete((dataRoot / fileName).toFile)
        }
      }
    }

    if (!deferDataRootRemoval) {
      FileSystem.removeDirectoryIfEmpty(dataRoot)
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
