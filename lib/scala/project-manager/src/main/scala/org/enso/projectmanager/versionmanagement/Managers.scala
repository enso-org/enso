package org.enso.projectmanager.versionmanagement

import java.nio.file.Path

import org.enso.runtimeversionmanager.distribution.{
  DistributionManager,
  TemporaryDirectoryManager
}
import org.enso.runtimeversionmanager.locking.{FileLockManager, ResourceManager}

object Managers {
  // TODO [RW, AO] should the PM support portable distributions?
  //  If so, where will be the project-manager binary located with respect to
  //  the distribution root?
  val distributionManager = new DistributionManager(DefaultEnvironment)

  object LockManager extends FileLockManager {
    override def locksRoot: Path = distributionManager.paths.locks
  }

  val resourceManager = new ResourceManager(LockManager)

  val temporaryDirectoryManager =
    new TemporaryDirectoryManager(distributionManager, resourceManager)
}
