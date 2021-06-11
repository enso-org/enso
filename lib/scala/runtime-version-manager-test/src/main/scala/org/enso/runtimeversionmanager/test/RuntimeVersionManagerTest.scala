package org.enso.runtimeversionmanager.test

import java.nio.file.Path
import nl.gn0s1s.bump.SemVer
import org.enso.distribution.{
  DistributionManager,
  Environment,
  PortableDistributionManager
}
import org.enso.editions.SemVerEnsoVersion
import org.enso.pkg.PackageManager
import org.enso.runtimeversionmanager.components.{
  GraalVMComponentConfiguration,
  InstallerKind,
  RuntimeVersionManagementUserInterface,
  RuntimeVersionManager
}
import org.enso.runtimeversionmanager.distribution.TemporaryDirectoryManager
import org.enso.runtimeversionmanager.releases.engine.EngineReleaseProvider
import org.enso.runtimeversionmanager.releases.graalvm.GraalVMRuntimeReleaseProvider
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Gathers helper methods for testing the [[RuntimeVersionManager]]. */
class RuntimeVersionManagerTest
    extends AnyWordSpec
    with Matchers
    with OptionValues
    with WithTemporaryDirectory
    with FakeEnvironment {

  /** Creates the [[DistributionManager]], [[RuntimeVersionManager]] and an
    * [[Environment]] for use in the tests.
    *
    * Should be called separately for each test case, as the components use
    * temporary directories which are separate for each test case.
    *
    * Additional environment variables may be provided that are added to the
    * [[Environment]] for the created managers.
    */
  def makeManagers(
    environmentOverrides: Map[String, String] = Map.empty,
    userInterface: RuntimeVersionManagementUserInterface =
      TestRuntimeVersionManagementUserInterface.default,
    installerKind: InstallerKind          = InstallerKind.Launcher,
    engineProvider: EngineReleaseProvider = FakeReleases.engineReleaseProvider,
    runtimeProvider: GraalVMRuntimeReleaseProvider =
      FakeReleases.runtimeReleaseProvider
  ): (DistributionManager, RuntimeVersionManager, Environment) = {
    val env                 = fakeInstalledEnvironment(environmentOverrides)
    val distributionManager = new PortableDistributionManager(env)

    val resourceManager = TestLocalResourceManager.create()
    val temporaryDirectoryManager =
      new TemporaryDirectoryManager(distributionManager, resourceManager)
    val componentConfig = new GraalVMComponentConfiguration

    val runtimeVersionManager = new RuntimeVersionManager(
      userInterface,
      distributionManager,
      temporaryDirectoryManager,
      resourceManager,
      engineProvider,
      runtimeProvider,
      componentConfig,
      NoopComponentUpdaterFactory,
      installerKind
    )

    (distributionManager, runtimeVersionManager, env)
  }

  /** Returns just the [[RuntimeVersionManager]].
    *
    * See [[makeManagers]] for details.
    */
  def makeRuntimeVersionManager(): RuntimeVersionManager = makeManagers()._2

  /** Creates a new project using the default package manager.
    */
  def newProject(name: String, path: Path, version: SemVer): Unit = {
    PackageManager.Default.create(
      root        = path.toFile,
      name        = name,
      ensoVersion = SemVerEnsoVersion(version)
    )
  }
}
