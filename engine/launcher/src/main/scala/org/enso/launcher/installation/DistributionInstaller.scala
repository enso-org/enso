package org.enso.launcher.installation

import java.nio.file.Files

import org.enso.cli.CLIOutput
import org.enso.launcher.internal.installation.DistributionManager
import org.enso.launcher.{FileSystem, Logger}
import org.enso.launcher.FileSystem.PathSyntax

class DistributionInstaller(
  manager: DistributionManager,
  autoConfirm: Boolean,
  bundleAction: Option[DistributionInstaller.BundleAction]
) {
  private val env = manager.env

  def install(): Unit = {
    prepare()
    createDirectories()
    installBinary()
    installBundles()
    Logger.info("Installation succeeded.")
  }

  final private val locally = manager.LocallyInstalledDirectories

  private def prepare(): Unit = {
    if (Files.exists(locally.dataDirectory)) {
      Logger.warn(s"${locally.dataDirectory} already exists.")
    }
    if (Files.exists(locally.configDirectory)) {
      Logger.warn(s"${locally.configDirectory} already exists.")
    }

    val message =
      s"""The installer will create the following directories:
         |data directory = `${locally.dataDirectory}`,
         |config directory = `${locally.configDirectory}`
         |and put the launcher binary in `${locally.binDirectory}`.""".stripMargin

    val binMessage =
      if (isBinOnSystemPath) None
      else
        Some(
          s"${locally.binDirectory} is not on system PATH. You may have to " +
          s"add it to the PATH to be able to launch `enso` from anywhere."
        )

    Logger.info(message)
    binMessage.foreach(msg => Logger.warn(msg))

    if (!autoConfirm) {
      CLIOutput.println(message + binMessage.map("\n" + _).getOrElse(""))
      if (
        !CLIOutput.askConfirmation(
          "Do you want to proceed with the installation?",
          yesDefault = binMessage.isEmpty
        )
      ) {}
    }
  }

  private def isBinOnSystemPath: Boolean = {
    val paths = env.getSystemPath
    paths.contains(locally.binDirectory)
  }

  private def createDirectories(): Unit = {

    Files.createDirectories(locally.dataDirectory)
    Files.createDirectories(
      locally.dataDirectory / DistributionManager.RUNTIMES_DIRECTORY
    )
    Files.createDirectories(
      locally.dataDirectory / DistributionManager.ENGINES_DIRECTORY
    )
    Files.createDirectories(locally.configDirectory)
  }

  private def installBinary(): Unit = {}

  private def installBundles(): Unit = {
    if (DistributionManager.isRunningPortable) {
      val runtimes =
        FileSystem.listDirectory(DistributionManager.paths.runtimes)
      val engines = FileSystem.listDirectory(DistributionManager.paths.engines)
      if (runtimes.length + engines.length > 0) {
        val _ = CLIOutput.askConfirmation(
          "Found bundled engine and/or runtime, do you want to move them to " +
          "the installation directory?"
        )
        // TODO [RW] continue
      }
    } else {
      bundleAction match {
        case Some(value) if value != DistributionInstaller.IgnoreBundles =>
          Logger.warn(
            s"Installer was asked to ${value.description}, but it seems to " +
            s"not be running from a bundle package."
          )
        case None =>
      }
    }
  }
}

object DistributionInstaller {
  trait BundleAction extends CLIOutput.Answer
  case object CopyBundles extends BundleAction {
    override def key: String         = "c"
    override def description: String = "copy bundles"
  }
  case object MoveBundles extends BundleAction {
    override def key: String         = "m"
    override def description: String = "move bundles"
  }
  case object IgnoreBundles extends BundleAction {
    override def key: String         = "i"
    override def description: String = "ignore bundles"
  }
}
