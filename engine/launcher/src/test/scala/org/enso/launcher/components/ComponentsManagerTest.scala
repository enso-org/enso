package org.enso.launcher.components

import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.cli.GlobalCLIOptions
import org.enso.launcher.installation.DistributionManager
import org.enso.launcher.releases.engine.EngineReleaseProvider
import org.enso.launcher.releases.runtime.GraalCEReleaseProvider
import org.enso.launcher.releases.testing.FakeReleaseProvider
import org.enso.launcher.{Environment, FakeEnvironment, WithTemporaryDirectory}
import org.enso.pkg.{PackageManager, SemVerEnsoVersion}
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ComponentsManagerTest
    extends AnyWordSpec
    with Matchers
    with OptionValues
    with WithTemporaryDirectory
    with FakeEnvironment {

  /**
    * Creates the [[DistributionManager]], [[ComponentsManager]] and an
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
  ): (DistributionManager, ComponentsManager, Environment) = {
    val env                 = fakeInstalledEnvironment(environmentOverrides)
    val distributionManager = new DistributionManager(env)
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
    val componentsManager = new ComponentsManager(
      GlobalCLIOptions(
        autoConfirm  = true,
        hideProgress = true,
        useJSON      = false
      ),
      distributionManager,
      engineProvider,
      runtimeProvider
    )

    (distributionManager, componentsManager, env)
  }

  /**
    * Returns just the [[ComponentsManager]].
    *
    * See [[makeManagers]] for details.
    */
  def makeComponentsManager(): ComponentsManager = makeManagers()._2

  /**
    * Creates a new project using the default package manager.
    */
  def newProject(name: String, path: Path, version: SemVer): Unit = {
    PackageManager.Default.create(
      root        = path.toFile,
      name        = name,
      ensoVersion = SemVerEnsoVersion(version)
    )
  }
}
