package org.enso.projectmanager.versionmanagement

import org.enso.runtimeversionmanager.components.{
  RuntimeVersionManagementUserInterface,
  RuntimeVersionManager
}
import org.enso.runtimeversionmanager.distribution.{
  DistributionManager,
  TemporaryDirectoryManager
}
import org.enso.runtimeversionmanager.locking.{FileLockManager, ResourceManager}
import org.enso.runtimeversionmanager.releases.ReleaseProvider
import org.enso.runtimeversionmanager.releases.engine.{
  EngineRelease,
  EngineRepository
}
import org.enso.runtimeversionmanager.releases.graalvm.GraalCEReleaseProvider

object DefaultManagers extends DistributionManagementConfiguration {
  // TODO [RW, AO] should the PM support portable distributions?
  //  If so, where will be the project-manager binary located with respect to
  //  the distribution root?
  val distributionManager = new DistributionManager(DefaultEnvironment)

  lazy val lockManager = new FileLockManager(distributionManager.paths.locks)

  lazy val resourceManager = new ResourceManager(lockManager)

  lazy val temporaryDirectoryManager =
    new TemporaryDirectoryManager(distributionManager, resourceManager)

  lazy val engineReleaseProvider: ReleaseProvider[EngineRelease] =
    EngineRepository.defaultEngineReleaseProvider

  def makeRuntimeVersionManager(
    userInterface: RuntimeVersionManagementUserInterface
  ): RuntimeVersionManager =
    new RuntimeVersionManager(
      userInterface             = userInterface,
      distributionManager       = distributionManager,
      temporaryDirectoryManager = temporaryDirectoryManager,
      resourceManager           = resourceManager,
      engineReleaseProvider     = engineReleaseProvider,
      runtimeReleaseProvider    = GraalCEReleaseProvider
    )
}
