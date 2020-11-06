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

trait DistributionManagementConfiguration {
  def distributionManager:       DistributionManager
  def resourceManager:           ResourceManager
  def temporaryDirectoryManager: TemporaryDirectoryManager

  def makeRuntimeVersionManager(
    userInterface: RuntimeVersionManagementUserInterface
  ): RuntimeVersionManager
}
