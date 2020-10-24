package org.enso.launcher.cli

import com.typesafe.scalalogging.Logger
import nl.gn0s1s.bump.SemVer
import org.enso.cli.{CLIOutput, ProgressBar, TaskProgress}
import org.enso.componentmanager.GlobalCLIOptions
import org.enso.componentmanager.components.ComponentManagementUserInterface

class CLIComponentManagementUserInterface(cliOptions: GlobalCLIOptions)
    extends ComponentManagementUserInterface {
  override def trackProgress(task: TaskProgress[_]): Unit =
    if (cliOptions.hideProgress) ()
    else ProgressBar.waitWithProgress(task)

  private val logger = Logger[CLIComponentManagementUserInterface]

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
}
