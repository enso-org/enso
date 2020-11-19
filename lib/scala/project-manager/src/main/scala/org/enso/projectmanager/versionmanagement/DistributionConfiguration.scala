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

/** Specifies the configuration of project manager's distribution.
  *
  * By default the project manager will define the set of its managers that use
  * a distribution located in a specific place within the filesystem. It can
  * also be overridden in tests.
  */
trait DistributionConfiguration {

  /** A [[DistributionManager]] instance. */
  def distributionManager: DistributionManager

  /** A [[ResourceManager]] instance. */
  def resourceManager: ResourceManager

  /** A [[TemporaryDirectoryManager]] instance. */
  def temporaryDirectoryManager: TemporaryDirectoryManager

  /** A provider of engine releases. */
  def engineReleaseProvider: ReleaseProvider[EngineRelease]

  /** Creates a [[RuntimeVersionManager]] from the specified
    * [[RuntimeVersionManagementUserInterface]] that will use the other managers
    * defined here.
    */
  def makeRuntimeVersionManager(
    userInterface: RuntimeVersionManagementUserInterface
  ): RuntimeVersionManager
}
