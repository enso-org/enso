package org.enso.componentmanager.test

import nl.gn0s1s.bump.SemVer
import org.enso.cli.TaskProgress
import org.enso.componentmanager.components.{
  ComponentManagementUserInterface,
  RuntimeVersion
}

class TestComponentManagementUserInterface(installBroken: Boolean)
    extends ComponentManagementUserInterface {

  /** @inheritdoc */
  override def trackProgress(task: TaskProgress[_]): Unit = ()

  /** @inheritdoc */
  override def shouldInstallBrokenEngine(version: SemVer): Boolean = {
    wasAskedForBroken = true
    installBroken
  }

  private var wasAskedForBroken: Boolean = false

  def wasAskedToInstallBroken: Boolean = wasAskedForBroken

  /** @inheritdoc */
  override def shouldInstallMissingEngine(version: SemVer): Boolean = true

  /** @inheritdoc */
  override def shouldInstallMissingRuntime(version: RuntimeVersion): Boolean =
    true

  override def logInfo(message: => String): Unit = ()
}

object TestComponentManagementUserInterface {
  def default: TestComponentManagementUserInterface =
    new TestComponentManagementUserInterface(installBroken = false)
}
