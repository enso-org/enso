package org.enso.launcher.components

import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.componentmanager.components.ComponentManager
import org.enso.componentmanager.releases.engine.EngineReleaseProvider
import org.enso.componentmanager.releases.runtime.GraalCEReleaseProvider
import org.enso.componentmanager.releases.testing.FakeReleaseProvider
import org.enso.componentmanager._
import org.enso.componentmanager.test.{DropLogs, WithTemporaryDirectory}
import org.enso.componentmanager.test.{
  FakeEnvironment,
  TestLocalResourceManager
}
import org.enso.pkg.{PackageManager, SemVerEnsoVersion}
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Gathers helper methods for testing the [[ComponentManager]].
  *
  * This class is shared by [[ComponentManagerSpec]] and other,
  * launcher-specific test. It would make most sense to move it to the
  * `component-manager` project, but then it would not be accessible from
  * launcher's tests. To avoid over-complicating this, this class is in the
  * `launcher` subproject, forcing [[ComponentManagerSpec]] to also be within
  * this project instead of `component-manager` where it would fit better.
  * Overall, as we always run all tests, the location of this particular test is
  * not really that important, so for simplicity it is kept here.
  */
class ComponentManagerTest
    extends AnyWordSpec
    with Matchers
    with OptionValues
    with WithTemporaryDirectory
    with FakeEnvironment
    with DropLogs {

  /** Creates the [[DistributionManager]], [[ComponentManager]] and an
    * [[Environment]] for use in the tests.
    *
    * Should be called separately for each test case, as the components use
    * temporary directories which are separate for each test case.
    *
    * Additional environment variables may be provided that are added to the
    * [[Environment]] for the created managers.
    */
  def makeManagers(
    environmentOverrides: Map[String, String] = Map.empty
  ): (DistributionManager, ComponentManager, Environment) = {
    val env = fakeInstalledEnvironment(environmentOverrides)
    val distributionManager =
      new DistributionManager(env, TestLocalResourceManager.create())
    val fakeReleasesRoot =
      Path.of(
        getClass
          .getResource("/org/enso/launcher/components/fake-releases")
          .toURI
      )
    val engineProvider = new EngineReleaseProvider(
      FakeReleaseProvider(
        fakeReleasesRoot.resolve("enso"),
        copyIntoArchiveRoot = Seq("manifest.yaml")
      )
    )
    val runtimeProvider = new GraalCEReleaseProvider(
      FakeReleaseProvider(fakeReleasesRoot.resolve("graalvm"))
    )
    val componentsManager = new ComponentManager(
      new TestUserInterface,
      distributionManager,
      TestLocalResourceManager.create(),
      engineProvider,
      runtimeProvider
    )

    (distributionManager, componentsManager, env)
  }

  /** Returns just the [[ComponentManager]].
    *
    * See [[makeManagers]] for details.
    */
  def makeComponentsManager(): ComponentManager = makeManagers()._2

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
