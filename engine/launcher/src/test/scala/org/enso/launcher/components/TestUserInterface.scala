package org.enso.launcher.components

import org.enso.cli.TaskProgress
import org.enso.componentmanager.UserInterface

class TestUserInterface extends UserInterface {
  override def trackProgress(task: TaskProgress[_]): Unit = ()
}
