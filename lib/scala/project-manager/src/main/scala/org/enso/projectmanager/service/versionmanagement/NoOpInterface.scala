package org.enso.projectmanager.service.versionmanagement

import nl.gn0s1s.bump.SemVer
import org.enso.cli.task.TaskProgress
import org.enso.runtimeversionmanager.components.{
  GraalVMVersion,
  RuntimeVersionManagementUserInterface
}
import org.enso.runtimeversionmanager.locking.Resource

/** A simple [[RuntimeVersionManagementUserInterface]] that does not allow to
  * install any versions and does not track any progress.
  */
class NoOpInterface extends RuntimeVersionManagementUserInterface {
  override def trackProgress(message: String, task: TaskProgress[_]): Unit =
    ()
  override def shouldInstallMissingEngine(version: SemVer): Boolean = false
  override def shouldInstallMissingRuntime(version: GraalVMVersion): Boolean =
    false
  override def shouldInstallBrokenEngine(version: SemVer): Boolean = false
  override def logInfo(message: => String): Unit                   = ()
  override def startWaitingForResource(resource: Resource): Unit   = ()
  override def finishWaitingForResource(resource: Resource): Unit  = ()
}
