package org.enso.projectmanager.versionmanagement

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
object DefaultDistributionConfiguration extends DistributionConfiguration {

  /** The default [[Environment]] implementation, with no overrides. */
  val environment: Environment = new Environment {}

  // TODO [RW, AO] should the PM support portable distributions?
  //  If so, where will be the project-manager binary located with respect to
  //  the distribution root?
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
  lazy val engineReleaseProvider: ReleaseProvider[EngineRelease] =
    EngineRepository.defaultEngineReleaseProvider

  private def runtimeReleaseProvider = GraalCEReleaseProvider

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
      runtimeReleaseProvider    = runtimeReleaseProvider,
      installerKind             = InstallerKind.ProjectManager
    )

  /** @inheritdoc */
  override def defaultJVMSettings: JVMSettings = JVMSettings.default

  /** @inheritdoc */
  override def shouldDiscardChildOutput: Boolean = false
}
