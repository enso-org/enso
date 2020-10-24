package org.enso.launcher.components

import nl.gn0s1s.bump.SemVer
import org.enso.cli.TaskProgress
import org.enso.componentmanager.components.ComponentManagementUserInterface

class TestComponentManagementUserInterface
    extends ComponentManagementUserInterface {
  override def trackProgress(task: TaskProgress[_]): Unit = ()

  override def shouldInstallBrokenEngine(version: SemVer): Boolean = false
}
