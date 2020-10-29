package org.enso.componentmanager.test

import com.typesafe.scalalogging.Logger
import nl.gn0s1s.bump.SemVer
import org.enso.cli.TaskProgress
import org.enso.componentmanager.components.{
  ComponentManagementUserInterface,
  RuntimeVersion
}

/** [[ComponentManagementUserInterface]] for usage in testing.
  *
  * It ensures that there are no interactive actions and unnecessary logging.
  */
class TestComponentManagementUserInterface(installBroken: Boolean)
    extends ComponentManagementUserInterface {
  private val logger = Logger[TestComponentManagementUserInterface]

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

  /** @inheritdoc */
  override def logInfo(message: => String): Unit = logger.debug(message)
}

object TestComponentManagementUserInterface {

  /** Creates a default [[TestComponentManagementUserInterface]]. */
  def default: TestComponentManagementUserInterface =
    new TestComponentManagementUserInterface(installBroken = false)
}
