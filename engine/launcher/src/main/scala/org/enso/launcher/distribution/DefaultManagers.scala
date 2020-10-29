package org.enso.launcher.distribution

import java.nio.file.Path

import org.enso.componentmanager.distribution.{
  DistributionManager,
  TemporaryDirectoryManager
}
import org.enso.componentmanager.locking.{FileLockManager, ResourceManager}

object DefaultManagers {
  val distributionManager = new DistributionManager(LauncherEnvironment)

  /** Default [[FileLockManager]] storing lock files in a directory defined by the
    * [[DistributionManager]].
    */
  object DefaultFileLockManager extends FileLockManager {

    /** @inheritdoc
      */
    override def locksRoot: Path = distributionManager.paths.locks
  }

  /** Default [[ResourceManager]] using the [[DefaultFileLockManager]].
    */
  object DefaultResourceManager extends ResourceManager(DefaultFileLockManager)

  val temporaryDirectoryManager =
    new TemporaryDirectoryManager(distributionManager, DefaultResourceManager)
}
