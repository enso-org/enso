package org.enso.projectmanager

import java.nio.file.Path

import org.enso.projectmanager.versionmanagement.DistributionConfiguration
import org.enso.runtimeversionmanager.components.{
  RuntimeVersionManagementUserInterface,
  RuntimeVersionManager
}
import org.enso.runtimeversionmanager.distribution.{
  DistributionManager,
  TemporaryDirectoryManager
}
import org.enso.runtimeversionmanager.locking.ResourceManager
import org.enso.runtimeversionmanager.releases.engine.{
  EngineRelease,
  EngineReleaseProvider
}
import org.enso.runtimeversionmanager.releases.graalvm.{
  GraalCEReleaseProvider,
  GraalVMRuntimeReleaseProvider
}
import org.enso.runtimeversionmanager.releases.{
  Release,
  ReleaseProvider,
  SimpleReleaseProvider
}
import org.enso.runtimeversionmanager.test.{
  FakeEnvironment,
  HasTestDirectory,
  TestLocalLockManager
}

import scala.util.{Failure, Success, Try}

/** A distribution configuration for use in tests.
  *
  * @param distributionRoot root of the test distribution, should be located
  *                         within some temporary directory
  * @param engineReleaseProvider provider of (fake) engine releases
  * @param runtimeReleaseProvider provider of (fake) Graal releases
  */
class TestDistributionConfiguration(
  distributionRoot: Path,
  override val engineReleaseProvider: ReleaseProvider[EngineRelease],
  runtimeReleaseProvider: GraalVMRuntimeReleaseProvider
) extends DistributionConfiguration
    with FakeEnvironment
    with HasTestDirectory {

  def getTestDirectory: Path = distributionRoot

  lazy val distributionManager = new DistributionManager(
    fakeInstalledEnvironment()
  )

  lazy val lockManager = new TestLocalLockManager

  lazy val resourceManager = new ResourceManager(lockManager)

  lazy val temporaryDirectoryManager =
    new TemporaryDirectoryManager(distributionManager, resourceManager)

  override def makeRuntimeVersionManager(
    userInterface: RuntimeVersionManagementUserInterface
  ): RuntimeVersionManager = new RuntimeVersionManager(
    userInterface             = userInterface,
    distributionManager       = distributionManager,
    temporaryDirectoryManager = temporaryDirectoryManager,
    resourceManager           = resourceManager,
    engineReleaseProvider     = engineReleaseProvider,
    runtimeReleaseProvider    = runtimeReleaseProvider
  )
}

object TestDistributionConfiguration {
  def withoutReleases(distributionRoot: Path): TestDistributionConfiguration = {
    val noReleaseProvider = new SimpleReleaseProvider {
      override def releaseForTag(tag: String): Try[Release] = Failure(
        new IllegalStateException(
          "This provider does not support fetching releases."
        )
      )

      override def listReleases(): Try[Seq[Release]] = Success(Seq())
    }

    new TestDistributionConfiguration(
      distributionRoot       = distributionRoot,
      engineReleaseProvider  = new EngineReleaseProvider(noReleaseProvider),
      runtimeReleaseProvider = new GraalCEReleaseProvider(noReleaseProvider)
    )
  }

  def apply(
    distributionRoot: Path,
    engineReleaseProvider: ReleaseProvider[EngineRelease],
    runtimeReleaseProvider: GraalVMRuntimeReleaseProvider
  ): TestDistributionConfiguration =
    new TestDistributionConfiguration(
      distributionRoot,
      engineReleaseProvider,
      runtimeReleaseProvider
    )

}
