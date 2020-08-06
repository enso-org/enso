package org.enso.launcher

import java.nio.file.{Files, Path}
import org.enso.launcher.FileSystem.PathSyntax

trait FakeEnvironment { self: WithTemporaryDirectory =>
  def fakeExecutablePath(): Path = {
    val fakeBin = getTestDirectory / "bin"
    Files.createDirectories(fakeBin)
    fakeBin / "enso"
  }

  def fakeInstalledEnvironment(): Environment = {
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

    fakeEnvironment
  }

}
