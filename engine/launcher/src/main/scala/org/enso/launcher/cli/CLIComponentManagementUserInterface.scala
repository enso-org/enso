package org.enso.launcher.cli

import com.typesafe.scalalogging.Logger
import nl.gn0s1s.bump.SemVer
import org.enso.cli.{CLIOutput, ProgressBar, TaskProgress}
import org.enso.componentmanager.GlobalCLIOptions
import org.enso.componentmanager.components.{
  ComponentManagementUserInterface,
  RuntimeVersion
}

class CLIComponentManagementUserInterface(cliOptions: GlobalCLIOptions)
    extends ComponentManagementUserInterface {

  /** @inheritdoc */
  override def trackProgress(task: TaskProgress[_]): Unit =
    if (cliOptions.hideProgress) ()
    else ProgressBar.waitWithProgress(task)

  private val logger = Logger[CLIComponentManagementUserInterface]

  /** @inheritdoc */
  override def shouldInstallBrokenEngine(version: SemVer): Boolean =
    if (cliOptions.autoConfirm) { // TODO [UI]
      logger.warn(
        s"The engine release $version is marked as broken and it should " +
        s"not be used. Since `auto-confirm` is set, the installation will " +
        s"continue, but you may want to reconsider changing versions to a " +
        s"stable release."
      )
      true
    } else {
      logger.warn(
        s"The engine release $version is marked as broken and it should " +
        s"not be used."
      )
      CLIOutput.askConfirmation(
        "Are you sure you still want to continue installing this version " +
        "despite the warning?"
      )
    }

  /** @inheritdoc */
  override def shouldInstallMissingEngine(version: SemVer): Boolean = {
    def complainAndAsk(): Boolean = {
      logger.warn(s"Engine $version is missing.")
      cliOptions.autoConfirm || CLIOutput.askConfirmation(
        "Do you want to install the missing engine?",
        yesDefault = true
      )
    }

    complainAndAsk() // TODO [RW] how to detect if we want to complain or not
  }

  /** @inheritdoc */
  override def shouldInstallMissingRuntime(version: RuntimeVersion): Boolean = {
    logger.warn(s"Required runtime $version is missing.")
    cliOptions.autoConfirm || CLIOutput.askConfirmation(
      "Do you want to install the missing runtime?",
      yesDefault = true
    )
  }
}
