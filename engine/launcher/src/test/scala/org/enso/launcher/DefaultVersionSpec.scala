package org.enso.launcher

import com.github.zafarkhaja.semver.Version
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

  private val latestAvailable = Version.of(0, 1, 0)

  "enso default" should {
    "fallback to latest available version if none is installed" in {
      makeConfigurationManager().defaultVersion shouldEqual latestAvailable
    }

    "fallback to latest installed version" in {
      val (componentsManager, configManager) =
        makeConfigAndComponentsManagers()
      componentsManager.findOrInstallEngine(Version.of(0, 0, 0))
      configManager.defaultVersion shouldEqual Version.of(0, 0, 0)
    }

    "set an exact version in the config" in {
      val version              = Version.of(0, 1, 0, "test")
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
