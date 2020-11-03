package org.enso.launcher.distribution

import org.enso.runtimeversionmanager.distribution.{
  PortableDistributionManager,
  TemporaryDirectoryManager
}
import org.enso.runtimeversionmanager.locking.{FileLockManager, ResourceManager}

/** Gathers default managers used in the launcher. */
object DefaultManagers {

  /** Default distribution manager that is capable of detecting portable mode.
    */
  val distributionManager = new PortableDistributionManager(LauncherEnvironment)

  /** Default [[FileLockManager]] storing lock files in a directory defined by
    * the distribution manager.
    */
  val defaultFileLockManager = new FileLockManager(
    distributionManager.paths.locks
  )

  /** Default [[ResourceManager]] using the [[defaultFileLockManager]]. */
  object DefaultResourceManager extends ResourceManager(defaultFileLockManager)

  /** Default [[TemporaryDirectoryManager]]. */
  val temporaryDirectoryManager =
    new TemporaryDirectoryManager(distributionManager, DefaultResourceManager)
}
