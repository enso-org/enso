package org.enso.launcher.cli

import org.enso.cli.{ProgressBar, TaskProgress}
import org.enso.componentmanager.{GlobalCLIOptions, UserInterface}

class CLIUserInterface(globalCLIOptions: GlobalCLIOptions)
    extends UserInterface {
  override def trackProgress(task: TaskProgress[_]): Unit =
    if (globalCLIOptions.hideProgress) ()
    else ProgressBar.waitWithProgress(task)
}
