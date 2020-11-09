package org.enso.projectmanager.versionmanagement

import org.enso.runtimeversionmanager.components.{
  RuntimeVersionManagementUserInterface,
  RuntimeVersionManager
}
import org.enso.runtimeversionmanager.distribution.{
  DistributionManager,
  TemporaryDirectoryManager
}
import org.enso.runtimeversionmanager.locking.ResourceManager
import org.enso.runtimeversionmanager.releases.ReleaseProvider
import org.enso.runtimeversionmanager.releases.engine.EngineRelease

trait DistributionManagementConfiguration {
  def distributionManager:       DistributionManager
  def resourceManager:           ResourceManager
  def temporaryDirectoryManager: TemporaryDirectoryManager
  def engineReleaseProvider:     ReleaseProvider[EngineRelease]

  def makeRuntimeVersionManager(
    userInterface: RuntimeVersionManagementUserInterface
  ): RuntimeVersionManager
}
