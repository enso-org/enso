package org.enso.launcher

import org.enso.semver.SemVer
import org.enso.distribution.config.DefaultVersion.{Exact, LatestInstalled}
import org.enso.runtimeversionmanager.components.RuntimeVersionManager
import org.enso.runtimeversionmanager.config.GlobalRunnerConfigurationManager
import org.enso.runtimeversionmanager.test.RuntimeVersionManagerTest

class DefaultVersionSpec extends RuntimeVersionManagerTest {
  def makeConfigAndComponentsManagers()
    : (RuntimeVersionManager, GlobalRunnerConfigurationManager) = {
    val (distributionManager, componentsManager, _) = makeManagers()
    val configurationManager =
      new GlobalRunnerConfigurationManager(
        componentsManager,
        distributionManager
      )
    (componentsManager, configurationManager)
  }

  def makeConfigurationManager(): GlobalRunnerConfigurationManager =
    makeConfigAndComponentsManagers()._2

  private val latestAvailable = SemVer.of(0, 1, 0)

  "enso default" should {
    "fallback to latest available version if none is installed" in {
      makeConfigurationManager().defaultVersion shouldEqual latestAvailable
    }

    "fallback to latest installed version" in {
      val (componentsManager, configManager) =
        makeConfigAndComponentsManagers()
      componentsManager.findOrInstallEngine(SemVer.of(0, 0, 0))
      configManager.defaultVersion shouldEqual SemVer.of(0, 0, 0)
    }

    "set an exact version in the config" in {
      val version              = SemVer.of(0, 1, 0, "test")
      val configurationManager = makeConfigurationManager()
      configurationManager.updateConfig { config =>
        config.copy(defaultVersion = Exact(version))
      }
      configurationManager.defaultVersion shouldEqual version
    }

    "set latest-installed version in the config" in {
      val configurationManager = makeConfigurationManager()
      configurationManager.updateConfig { config =>
        config.copy(defaultVersion = LatestInstalled)
      }
      configurationManager.defaultVersion shouldEqual latestAvailable
    }
  }
}
