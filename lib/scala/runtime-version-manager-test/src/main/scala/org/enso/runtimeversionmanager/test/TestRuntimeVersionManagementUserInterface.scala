package org.enso.runtimeversionmanager.test

import com.typesafe.scalalogging.Logger
import nl.gn0s1s.bump.SemVer
import org.enso.cli.TaskProgress
import org.enso.runtimeversionmanager.components.{
  GraalVMVersion,
  RuntimeVersionManagementUserInterface
}

/** [[RuntimeVersionManagementUserInterface]] for usage in testing.
  *
  * It ensures that there are no interactive actions and unnecessary logging.
  */
class TestRuntimeVersionManagementUserInterface(installBroken: Boolean)
    extends RuntimeVersionManagementUserInterface {
  private val logger = Logger[TestRuntimeVersionManagementUserInterface]

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
  override def shouldInstallMissingRuntime(version: GraalVMVersion): Boolean =
    true

  /** @inheritdoc */
  override def logInfo(message: => String): Unit = logger.debug(message)
}

object TestRuntimeVersionManagementUserInterface {

  /** Creates a default [[TestRuntimeVersionManagementUserInterface]]. */
  def default: TestRuntimeVersionManagementUserInterface =
    new TestRuntimeVersionManagementUserInterface(installBroken = false)
}
