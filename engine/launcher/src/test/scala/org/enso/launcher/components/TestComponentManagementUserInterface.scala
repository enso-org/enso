package org.enso.launcher.components

import nl.gn0s1s.bump.SemVer
import org.enso.cli.TaskProgress
import org.enso.componentmanager.components.{
  ComponentManagementUserInterface,
  RuntimeVersion
}

class TestComponentManagementUserInterface
    extends ComponentManagementUserInterface {

  /** @inheritdoc */
  override def trackProgress(task: TaskProgress[_]): Unit = ()

  /** @inheritdoc */
  override def shouldInstallBrokenEngine(version: SemVer): Boolean = false

  /** @inheritdoc */
  override def shouldInstallMissingEngine(version: SemVer): Boolean = true

  /** @inheritdoc */
  override def shouldInstallMissingRuntime(version: RuntimeVersion): Boolean =
    true
}
