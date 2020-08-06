package org.enso.launcher.components

import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.cli.GlobalCLIOptions
import org.enso.launcher.installation.DistributionManager
import org.enso.launcher.releases.{
  EngineReleaseProvider,
  GraalCEReleaseProvider
}
import org.enso.launcher.{FakeEnvironment, WithTemporaryDirectory}
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ComponentsManagerSpec
    extends AnyWordSpec
    with Matchers
    with OptionValues
    with WithTemporaryDirectory
    with FakeEnvironment {

  private def makeComponentsManager(): ComponentsManager = {
    val distributionManager = new DistributionManager(
      fakeInstalledEnvironment()
    )
    val fakeReleasesRoot =
      Path.of(
        getClass
          .getResource("fake-releases")
          .getPath
      )
    val engineProvider = new EngineReleaseProvider(
      FakeReleaseProvider(fakeReleasesRoot.resolve("enso"))
    )
    val runtimeProvider = new GraalCEReleaseProvider(
      FakeReleaseProvider(fakeReleasesRoot.resolve("graalvm"))
    )
    val componentsManager = new ComponentsManager(
      GlobalCLIOptions(autoConfirm = true, hideProgress = true),
      distributionManager,
      engineProvider,
      runtimeProvider
    )

    componentsManager
  }

  "ComponentsManager" should {
    "find the latest engine version in semver ordering" in {
      val componentsManager = makeComponentsManager()
      componentsManager.fetchLatestEngineVersion() shouldEqual SemVer(0, 0, 1)
    }

    "install the engine and a matching runtime for it" in {
      val componentsManager = makeComponentsManager()
      val version           = SemVer(0, 0, 1)
      val engine            = componentsManager.findOrInstallEngine(SemVer(0, 0, 1))
      engine.version shouldEqual version
      val runtime = componentsManager.findRuntime(engine)
      runtime.value.version shouldEqual RuntimeVersion(SemVer(2, 0, 0), "11")
    }

    "list installed engines and runtimes" in {
      // TODO
    }

    "uninstall the runtime iff it is not used by any engines" in {
      // TODO
    }
  }
}
