package org.enso.launcher.installation

import java.io.PrintWriter
import java.nio.file.Files

import org.enso.cli.CLIOutput
import org.enso.launcher.{DistributionManager, Logger}

object ForcePortable {
  def run(force: Boolean): Unit = {
    if (!DistributionManager.isRunningPortable) {
      val installedDistributionExists =
        DistributionManager.LocallyInstalledDirectories.installedDistributionExists

      if (installedDistributionExists && !isThereAnotherLauncherInstalled) {
        if (force) {
          Logger.warn(
            s"There exists an installed distribution at " +
            s"${DistributionManager.paths.dataRoot}, be careful."
          )
        } else {
          CLIOutput.println(
            s"A local launcher installation has been found at " +
            s"${DistributionManager.paths.dataRoot}. It seems that this" +
            s"launcher is used with it. If that is the case, forcing this" +
            s"launcher to become portable, will make it impossible to use " +
            s"that installed distribution."
          )

          if (
            !CLIOutput.askConfirmation(
              "Are you sure you want to switch to the portable mode?"
            )
          ) {
            abandon()
          } else {
            // TODO [RW] or maybe we want to uninstall it?
            CLIOutput.println(
              "If you want to switch back to the installed distribution, " +
              "run `enso install distribution`."
            )
          }
        }
      }

      val executablePath =
        DistributionManager.getPathToRunningBinaryExecutable
      val binDirectory = executablePath.getParent
      if (
        binDirectory.getFileName.toString != DistributionManager.BIN_DIRECTORY
      ) {
        val message =
          s"The launcher binary is stored in a directory that is not called " +
          s"`${DistributionManager.BIN_DIRECTORY}`."
        if (force) {
          Logger.warn(message)
        } else {
          CLIOutput.println(message)
          if (
            !CLIOutput.askConfirmation(
              "Are you sure you want to switch to the portable mode here?"
            )
          ) {
            abandon()
          }
        }
      }

      createPortableMarkFile()
    } else {
      Logger.debug(
        "Already running a portable execution. --force-portable has no effect."
      )
    }
  }

  private def abandon(): Nothing = {
    CLIOutput.println(
      "Portable mode abandoned. Please re-run without the " +
      "--force-portable flag."
    )
    sys.exit(1)
  }

  /**
    * Checks if the local installation location contains a launcher and it is not
    * the one we are currently running.
    */
  private def isThereAnotherLauncherInstalled: Boolean = {
    val installedLauncher =
      DistributionManager.LocallyInstalledDirectories.binaryExecutable
    val currentLauncher =
      DistributionManager.getPathToRunningBinaryExecutable

    Logger.debug(s"Installed = $installedLauncher")

    Files.exists(installedLauncher) && installedLauncher != currentLauncher
  }

  private def createPortableMarkFile(): Unit = {
    val path   = DistributionManager.portableMarkFilePath
    val writer = new PrintWriter(path.toFile)
    try {
      writer.println("ENSO DISTRIBUTION PORTABLE MODE MARK FILE")
    } finally {
      writer.close()
    }
  }
}
