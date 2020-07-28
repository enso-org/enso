package org.enso.launcher.installation

import java.nio.file.{Files, Path}

import org.enso.launcher.{FileSystem, WithTemporaryDirectory}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.enso.launcher.FileSystem.PathSyntax
import org.enso.launcher.internal.Environment
import org.enso.launcher.internal.installation.DistributionManager

class DistributionManagerSpec
    extends AnyWordSpec
    with Matchers
    with WithTemporaryDirectory {

  def fakeExecutablePath(): Path = {
    val fakeBin = getTestDirectory / "bin"
    Files.createDirectories(fakeBin)
    fakeBin / "enso"
  }

  "DistributionManager" should {
    "detect portable distribution" in {
      val executable = fakeExecutablePath()
      val fakeEnvironment = new Environment {
        override def getPathToRunningExecutable: Path = executable
      }
      FileSystem.writeTextFile(getTestDirectory / ".enso.portable", "mark")

      val distributionManager = new DistributionManager(fakeEnvironment)
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

      val distributionManager = new DistributionManager(fakeEnvironment)
      distributionManager.isRunningPortable shouldEqual false
    }

    "respect environment variable overrides " +
    "for installed distribution location" in {
      val executable = fakeExecutablePath()
      val dataDir    = getTestDirectory / "test_data"
      val configDir  = getTestDirectory / "test_config"
      val binDir     = getTestDirectory / "test_bin"
      val fakeEnvironment = new Environment {
        override def getPathToRunningExecutable: Path = executable

        override def getEnvVar(key: String): Option[String] =
          key match {
            case "ENSO_DATA_DIRECTORY"   => Some(dataDir.toString)
            case "ENSO_CONFIG_DIRECTORY" => Some(configDir.toString)
            case "ENSO_BIN_DIRECTORY"    => Some(binDir.toString)
            case _                       => super.getEnvVar(key)
          }
      }

      val distributionManager = new DistributionManager(fakeEnvironment)
      distributionManager.paths.dataRoot shouldEqual dataDir
      distributionManager.paths.config shouldEqual configDir
      distributionManager.LocallyInstalledDirectories.binDirectory shouldEqual
      binDir
    }
  }
}
