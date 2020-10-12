package org.enso.launcher.installation

import java.nio.file.Path

import org.enso.launcher.FileSystem.PathSyntax
import org.enso.launcher.locking.TestLocalResourceManager
import org.enso.launcher.{
  DropLogs,
  Environment,
  FakeEnvironment,
  WithTemporaryDirectory
}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DistributionManagerSpec
    extends AnyWordSpec
    with Matchers
    with WithTemporaryDirectory
    with FakeEnvironment
    with DropLogs {

  "DistributionManager" should {
    "detect portable distribution" in {
      val executable = fakeExecutablePath(portable = true)
      val fakeEnvironment = new Environment {
        override def getPathToRunningExecutable: Path = executable
      }

      val distributionManager =
        new DistributionManager(
          fakeEnvironment,
          TestLocalResourceManager.create()
        )
      distributionManager.isRunningPortable shouldEqual true
      distributionManager.paths.dataRoot shouldEqual getTestDirectory
      distributionManager.paths.config shouldEqual getTestDirectory / "config"
      distributionManager.paths.runtimes shouldEqual
      getTestDirectory / "runtime"
      distributionManager.paths.engines shouldEqual getTestDirectory / "dist"
    }

    "detect installed distribution" in {
      val executable = fakeExecutablePath()
      val fakeEnvironment = new Environment {
        override def getPathToRunningExecutable: Path = executable
      }

      val distributionManager =
        new DistributionManager(
          fakeEnvironment,
          TestLocalResourceManager.create()
        )
      distributionManager.isRunningPortable shouldEqual false
    }

    "respect environment variable overrides " +
    "for installed distribution location" in {
      val dataDir   = getTestDirectory / "test_data"
      val configDir = getTestDirectory / "test_config"
      val binDir    = getTestDirectory / "test_bin"

      val distributionManager =
        new DistributionManager(
          fakeInstalledEnvironment(),
          TestLocalResourceManager.create()
        )
      distributionManager.paths.dataRoot shouldEqual dataDir
      distributionManager.paths.config shouldEqual configDir
      distributionManager.LocallyInstalledDirectories.binDirectory shouldEqual
      binDir
    }
  }
}
