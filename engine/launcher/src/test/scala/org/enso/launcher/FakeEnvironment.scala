package org.enso.launcher

import java.nio.file.{Files, Path}
import org.enso.launcher.FileSystem.PathSyntax

/**
  * A test-suite mixin that adds helper functions that create a fake environment
  * which points to an Enso installation inside the temporary directory
  * generated for the test.
  */
trait FakeEnvironment { self: WithTemporaryDirectory =>

  /**
    * Returns a fake path to the Enso executable that is inside the temporary
    * directory for the test.
    *
    * @param portable specifies whether the distribution should be marked as
    *                 portable
    */
  def fakeExecutablePath(portable: Boolean = false): Path = {
    val fakeBin = getTestDirectory / "bin"
    Files.createDirectories(fakeBin)
    if (portable) {
      FileSystem.writeTextFile(getTestDirectory / ".enso.portable", "mark")
    }
    fakeBin / "enso"
  }

  /**
    * Returns an [[Environment]] instance that overrides the `ENSO_*`
    * directories to be inside the temporary directory for the test.
    */
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
