package org.enso.launcher.components

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.Logger

class ComponentsManagerSpec extends ComponentsManagerTest {

  "ComponentsManager" should {
    "find the latest engine version in semver ordering" in {
      Logger.suppressWarnings {
        val componentsManager = makeComponentsManager()
        componentsManager.fetchLatestEngineVersion() shouldEqual SemVer(0, 0, 1)
      }
    }

    "install the engine and a matching runtime for it" in {
      Logger.suppressWarnings {
        val (distributionManager, componentsManager, _) = makeManagers()

        val version = SemVer(0, 0, 1)
        val engine  = componentsManager.findOrInstallEngine(SemVer(0, 0, 1))

        engine.version shouldEqual version
        assert(
          engine.path.startsWith(distributionManager.paths.engines),
          "Engine should be installed in the engines directory."
        )

        val runtime = componentsManager.findRuntime(engine)
        runtime.value.version shouldEqual RuntimeVersion(SemVer(2, 0, 0), "11")
        assert(
          runtime.value.path.startsWith(distributionManager.paths.runtimes),
          "Engine should be installed in the engines directory."
        )
      }
    }

    "list installed engines and runtimes" in {
      Logger.suppressWarnings {
        val componentsManager = makeComponentsManager()
        val engineVersions =
          Set(SemVer(0, 0, 0), SemVer(0, 0, 1), SemVer(0, 0, 1, Some("pre")))
        val runtimeVersions =
          Set(
            RuntimeVersion(SemVer(1, 0, 0), "11"),
            RuntimeVersion(SemVer(2, 0, 0), "11")
          )
        engineVersions.map(
          componentsManager.findOrInstallEngine(_, complain = false)
        )

        componentsManager
          .listInstalledEngines()
          .map(_.version)
          .toSet shouldEqual engineVersions
        componentsManager
          .listInstalledRuntimes()
          .map(_.version)
          .toSet shouldEqual runtimeVersions

        val runtime2 =
          componentsManager
            .findRuntime(RuntimeVersion(SemVer(2, 0, 0), "11"))
            .value
        componentsManager.findEnginesUsingRuntime(runtime2) should have length 2
      }
    }

    "uninstall the runtime iff it is not used by any engines" in {
      Logger.suppressWarnings {
        val componentsManager = makeComponentsManager()
        val engineVersions =
          Seq(SemVer(0, 0, 0), SemVer(0, 0, 1), SemVer(0, 0, 1, Some("pre")))
        engineVersions.map(
          componentsManager.findOrInstallEngine(_, complain = false)
        )

        componentsManager.listInstalledEngines() should have length 3
        componentsManager.listInstalledRuntimes() should have length 2

        // remove the engine that shares the runtime with another one
        val version1 = SemVer(0, 0, 1, Some("pre"))
        componentsManager.uninstallEngine(version1)
        val engines1 = componentsManager.listInstalledEngines()
        engines1 should have length 2
        engines1.map(_.version) should not contain version1
        componentsManager.listInstalledRuntimes() should have length 2

        // remove the second engine that shared the runtime
        val version2 = SemVer(0, 0, 1)
        componentsManager.uninstallEngine(version2)
        val engines2 = componentsManager.listInstalledEngines()
        engines2 should have length 1
        engines2.map(_.version) should not contain version2
        val runtimes2 = componentsManager.listInstalledRuntimes()
        runtimes2 should have length 1
        runtimes2.map(_.version).head shouldEqual RuntimeVersion(
          SemVer(1, 0, 0),
          "11"
        )

        // remove the last engine
        componentsManager.uninstallEngine(SemVer(0, 0, 0))
        componentsManager.listInstalledEngines() should have length 0
        componentsManager.listInstalledRuntimes() should have length 0
      }
    }
  }
}
