package org.enso.launcher

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.components.{ComponentsManager, ComponentsManagerTest}
import org.enso.launcher.config.DefaultVersion.{Exact, LatestInstalled}
import org.enso.launcher.config.GlobalConfigurationManager

class DefaultVersionSpec extends ComponentsManagerTest {
  def makeConfigAndComponentsManagers()
    : (ComponentsManager, GlobalConfigurationManager) = {
    val (distributionManager, componentsManager, _) = makeManagers()
    val configurationManager =
      new GlobalConfigurationManager(componentsManager, distributionManager)
    (componentsManager, configurationManager)
  }

  def makeConfigurationManager(): GlobalConfigurationManager =
    makeConfigAndComponentsManagers()._2

  private val latestAvailable = SemVer(0, 0, 1)

  "enso default" should {
    "fallback to latest available version if none is installed" in {
      makeConfigurationManager().defaultVersion shouldEqual latestAvailable
    }

    "fallback to latest installed version" in {
      Logger.suppressWarnings {
        val (componentsManager, configManager) =
          makeConfigAndComponentsManagers()
        componentsManager.findOrInstallEngine(SemVer(0, 0, 0))
        configManager.defaultVersion shouldEqual SemVer(0, 0, 0)
      }
    }

    "set an exact version in the config" in {
      val version              = SemVer(0, 1, 0, Some("test"))
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
