package org.enso.runtimeversionmanager.components

import nl.gn0s1s.bump.SemVer
import org.enso.cli.task.ProgressReporter
import org.enso.distribution.locking.LockUserInterface

/** Encapsulates the communication between [[RuntimeVersionManager]] and its
  * user.
  */
trait RuntimeVersionManagementUserInterface
    extends LockUserInterface
    with ProgressReporter {

  /** Called when an operation requires an engine that is not available.
    *
    * Depending on the return value, the missing engine will be installed or the
    * action will fail.
    */
  def shouldInstallMissingEngine(version: SemVer): Boolean

  /** Called when a runtime required to complete an operation is missing.
    *
    * This should not happen in usual situations as the runtimes are
    * automatically installed with an engine. It may only happen if the runtime
    * has been manually removed.
    *
    * Depending on the return value, the missing runtime will be installed or
    * the action will fail.
    */
  def shouldInstallMissingRuntime(version: GraalVMVersion): Boolean

  /** Called when a broken engine is about to be installed.
    *
    * Depending on the return value, the broken version will be installed or the
    * action will fail.
    */
  def shouldInstallBrokenEngine(version: SemVer): Boolean

  /** Called to allow for special handling of info-level logs. */
  def logInfo(message: => String): Unit
}
