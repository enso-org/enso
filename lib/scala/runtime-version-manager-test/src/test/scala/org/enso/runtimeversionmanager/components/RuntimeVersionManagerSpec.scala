package org.enso.runtimeversionmanager.components

import nl.gn0s1s.bump.SemVer
import org.enso.runtimeversionmanager.components
import org.enso.runtimeversionmanager.config.GlobalConfigurationManager
import org.enso.runtimeversionmanager.test.{
  RuntimeVersionManagerTest,
  TestRuntimeVersionManagementUserInterface
}

class RuntimeVersionManagerSpec extends RuntimeVersionManagerTest {

  "RuntimeVersionManager" should {
    "find the latest engine version in semver ordering " +
    "(skipping broken releases)" in {
      val componentsManager = makeRuntimeVersionManager()
      componentsManager.fetchLatestEngineVersion() shouldEqual SemVer(0, 0, 1)
    }

    "install the engine and a matching runtime for it" in {
      val (distributionManager, componentsManager, _) = makeManagers()

      val version = SemVer(0, 0, 1)
      val engine  = componentsManager.findOrInstallEngine(SemVer(0, 0, 1))

      engine.version shouldEqual version
      assert(
        engine.path.startsWith(distributionManager.paths.engines),
        "Engine should be installed in the engines directory."
      )

      val runtime = componentsManager.findGraalRuntime(engine)
      runtime.value.version shouldEqual GraalVMVersion(SemVer(2, 0, 0), "11")
      assert(
        runtime.value.path.startsWith(distributionManager.paths.runtimes),
        "Engine should be installed in the engines directory."
      )
    }

    "list installed engines and runtimes" in {
      val componentsManager = makeRuntimeVersionManager()
      val engineVersions =
        Set(SemVer(0, 0, 0), SemVer(0, 0, 1), SemVer(0, 0, 1, Some("pre")))
      val runtimeVersions =
        Set(
          components.GraalVMVersion(SemVer(1, 0, 0), "11"),
          components.GraalVMVersion(SemVer(2, 0, 0), "11")
        )
      engineVersions.map(componentsManager.findOrInstallEngine)

      componentsManager
        .listInstalledEngines()
        .map(_.version)
        .toSet shouldEqual engineVersions
      componentsManager
        .listInstalledGraalRuntimes()
        .map(_.version)
        .toSet shouldEqual runtimeVersions

      val runtime2 =
        componentsManager
          .findGraalRuntime(components.GraalVMVersion(SemVer(2, 0, 0), "11"))
          .value
      componentsManager.findEnginesUsingRuntime(runtime2) should have length 2
    }

    "preserve the broken mark when installing a broken release" in {
      val componentsManager = makeManagers(userInterface =
        new TestRuntimeVersionManagementUserInterface(installBroken = true)
      )._2
      val brokenVersion = SemVer(0, 999, 0, Some("marked-broken"))
      componentsManager.findOrInstallEngine(brokenVersion)

      assert(
        componentsManager.findEngine(brokenVersion).value.isMarkedBroken,
        "The broken release should still be marked as broken after being " +
        "installed and loaded."
      )
    }

    "skip broken releases when finding latest installed version" in {
      val (distributionManager, componentsManager, _) =
        makeManagers(userInterface =
          new TestRuntimeVersionManagementUserInterface(installBroken = true)
        )
      val configurationManager =
        new GlobalConfigurationManager(componentsManager, distributionManager)

      val validVersion          = SemVer(0, 0, 1)
      val newerButBrokenVersion = SemVer(0, 999, 0, Some("marked-broken"))
      componentsManager.findOrInstallEngine(validVersion)
      componentsManager.findOrInstallEngine(newerButBrokenVersion)

      configurationManager.defaultVersion shouldEqual validVersion
    }

    "issue a warning when a broken release is requested" in {
      val userInterface =
        new TestRuntimeVersionManagementUserInterface(installBroken = true)
      val componentsManager = makeManagers(userInterface = userInterface)._2

      val brokenVersion = SemVer(0, 999, 0, Some("marked-broken"))
      componentsManager.findOrInstallEngine(brokenVersion)
      assert(
        userInterface.wasAskedToInstallBroken,
        "User interface should have been queried if broken versions are allowed."
      )
      componentsManager.findEngine(brokenVersion).value
    }

    "uninstall the runtime iff it is not used by any engines" in {
      val componentsManager = makeRuntimeVersionManager()
      val engineVersions =
        Seq(SemVer(0, 0, 0), SemVer(0, 0, 1), SemVer(0, 0, 1, Some("pre")))
      engineVersions.map(componentsManager.findOrInstallEngine)

      componentsManager.listInstalledEngines() should have length 3
      componentsManager.listInstalledGraalRuntimes() should have length 2

      // remove the engine that shares the runtime with another one
      val version1 = SemVer(0, 0, 1, Some("pre"))
      componentsManager.uninstallEngine(version1)
      val engines1 = componentsManager.listInstalledEngines()
      engines1 should have length 2
      engines1.map(_.version) should not contain version1
      componentsManager.listInstalledGraalRuntimes() should have length 2

      // remove the second engine that shared the runtime
      val version2 = SemVer(0, 0, 1)
      componentsManager.uninstallEngine(version2)
      val engines2 = componentsManager.listInstalledEngines()
      engines2 should have length 1
      engines2.map(_.version) should not contain version2
      val runtimes2 = componentsManager.listInstalledGraalRuntimes()
      runtimes2 should have length 1
      runtimes2.map(_.version).head shouldEqual components.GraalVMVersion(
        SemVer(1, 0, 0),
        "11"
      )

      // remove the last engine
      componentsManager.uninstallEngine(SemVer(0, 0, 0))
      componentsManager.listInstalledEngines() should have length 0
      componentsManager.listInstalledGraalRuntimes() should have length 0
    }
  }
}
