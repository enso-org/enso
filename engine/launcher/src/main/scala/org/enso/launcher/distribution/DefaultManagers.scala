package org.enso.launcher.distribution

import java.nio.file.Path

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
  object DefaultFileLockManager extends FileLockManager {

    /** @inheritdoc */
    override def locksRoot: Path = distributionManager.paths.locks
  }

  /** Default [[ResourceManager]] using the [[DefaultFileLockManager]]. */
  object DefaultResourceManager extends ResourceManager(DefaultFileLockManager)

  /** Default [[TemporaryDirectoryManager]]. */
  val temporaryDirectoryManager =
    new TemporaryDirectoryManager(distributionManager, DefaultResourceManager)
}
