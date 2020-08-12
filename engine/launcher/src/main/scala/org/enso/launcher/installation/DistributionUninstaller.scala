package org.enso.launcher.installation

import java.nio.file.Path

import org.enso.launcher.{FileSystem, OS}
import org.enso.launcher.FileSystem.PathSyntax

class DistributionUninstaller(
  manager: DistributionManager,
  autoConfirm: Boolean
) {
  def uninstall(): Unit = {
    uninstallDataContents()
    uninstallConfig()
//    if (isBinaryInsideData) {} else {
//      removeDataDirectory()
//
//    }
  }

  private def isBinaryInsideData: Boolean = {
    val binaryPath =
      manager.env.getPathToRunningExecutable.toAbsolutePath.normalize
    val dataPath = manager.paths.dataRoot.toAbsolutePath.normalize
    binaryPath.startsWith(dataPath)
  }

  private def uninstallConfig(): Unit = {
    //
  }

  private val knownDataFiles       = Seq("README.md", "NOTICE")
  private val knownDataDirectories = Seq("tmp", "components-licences")
  private def uninstallDataContents(): Unit = {
    FileSystem.removeDirectory(manager.paths.engines)
    FileSystem.removeDirectory(manager.paths.runtimes)
    for (dirName <- knownDataDirectories) {
      FileSystem.removeDirectoryIfExists(manager.paths.dataRoot / dirName)
    }
    for (fileName <- knownDataFiles) {
      FileSystem.removeFileIfExists(manager.paths.dataRoot / fileName)
    }
  }

  private def uninstallBin(parentDataToRemove: Option[Path]): Unit = {
    //
  }

  private def removeDataDirectory(): Unit = {
    //
  }
}
