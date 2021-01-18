package org.enso.projectmanager.versionmanagement

import com.typesafe.scalalogging.LazyLogging
import org.enso.runtimeversionmanager.Environment
import org.enso.runtimeversionmanager.components.{
  InstallerKind,
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
import org.enso.runtimeversionmanager.runner.JVMSettings

/** Default distribution configuration to use for the Project Manager in
  * production.
  *
  * The distribution manager and others need to be lazily initialized to ensure
  * that they are initialized at runtime and not at build time if we try
  * building a Native Image.
  */
object DefaultDistributionConfiguration
    extends DistributionConfiguration
    with LazyLogging {

  /** The default [[Environment]] implementation, with no overrides. */
  val environment: Environment = new Environment {}

  /** @inheritdoc */
  lazy val distributionManager = new DistributionManager(environment)

  /** @inheritdoc */
  lazy val lockManager = new FileLockManager(distributionManager.paths.locks)

  /** @inheritdoc */
  lazy val resourceManager = new ResourceManager(lockManager)

  /** @inheritdoc */
  lazy val temporaryDirectoryManager =
    new TemporaryDirectoryManager(distributionManager, resourceManager)

  /** @inheritdoc */
  def engineReleaseProvider: ReleaseProvider[EngineRelease] =
    EngineRepository.defaultEngineReleaseProvider

  /** @inheritdoc */
  def makeRuntimeVersionManager(
    userInterface: RuntimeVersionManagementUserInterface
  ): RuntimeVersionManager =
    new RuntimeVersionManager(
      userInterface             = userInterface,
      distributionManager       = distributionManager,
      temporaryDirectoryManager = temporaryDirectoryManager,
      resourceManager           = resourceManager,
      engineReleaseProvider     = engineReleaseProvider,
      runtimeReleaseProvider    = GraalCEReleaseProvider.default,
      installerKind             = InstallerKind.ProjectManager
    )

  /** @inheritdoc */
  override def defaultJVMSettings: JVMSettings = JVMSettings.default

  /** @inheritdoc */
  override def shouldDiscardChildOutput: Boolean = false
}
