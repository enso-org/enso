package org.enso.launcher.installation

import java.nio.file.{Files, Path}

import org.enso.launcher.FileSystem.PathSyntax
import org.enso.launcher.{FileSystem, NativeTest, OS, WithTemporaryDirectory}

class UninstallerSpec extends NativeTest with WithTemporaryDirectory {
  def installedRoot: Path = getTestDirectory / "installed"

  def prepareInstalledDistribution(
    everythingInsideData: Boolean = false
  ): (Path, Map[String, String]) = {
    val binDirectory =
      if (everythingInsideData) installedRoot / "bin" else getTestDirectory
    val configDirectory =
      if (everythingInsideData) installedRoot / "config"
      else getTestDirectory / "enso-config"
    val dataDirectory    = installedRoot
    val portableLauncher = binDirectory / OS.executableName("enso")
    copyLauncherTo(portableLauncher)
    Files.createDirectories(dataDirectory / "dist")
    Files.createDirectories(configDirectory)
    FileSystem.writeTextFile(
      configDirectory / "global-config.yml",
      "what: ever"
    )

    val env = Map(
      "ENSO_DATA_DIRECTORY"   -> dataDirectory.toAbsolutePath.normalize.toString,
      "ENSO_BIN_DIRECTORY"    -> binDirectory.toAbsolutePath.normalize.toString,
      "ENSO_CONFIG_DIRECTORY" -> configDirectory.toAbsolutePath.normalize.toString
    )
    (portableLauncher, env)
  }

  "enso uninstall distribution" should {
    "uninstall a simple distribution" in {
      val (launcher, env) = prepareInstalledDistribution()

      runLauncherAt(
        launcher,
        Seq("--auto-confirm", "uninstall", "distribution"),
        env
      ) should returnSuccess

      assert(Files.notExists(installedRoot))
      assert(Files.notExists(getTestDirectory / "enso-config"))
      assert(Files.notExists(launcher))
    }

    "uninstall a distribution with config and bin inside of data" in {
      val (launcher, env) =
        prepareInstalledDistribution(everythingInsideData = true)

      val result = runLauncherAt(
        launcher,
        Seq("--auto-confirm", "uninstall", "distribution"),
        env
      )
      result should returnSuccess
      println(result.stdout)
      println(result.stderr)

      assert(Files.notExists(installedRoot))
    }

    "not remove unknown files by default when uninstalling" in {
      val (launcher, env) = prepareInstalledDistribution()
      val configFile      = getTestDirectory / "enso-config" / "unknown-file"
      FileSystem.writeTextFile(configFile, "mark")
      val dataFile = installedRoot / "unknown-file2"
      FileSystem.writeTextFile(dataFile, "mark")

      runLauncherAt(
        launcher,
        Seq("--auto-confirm", "uninstall", "distribution"),
        env
      ) should returnSuccess

      assert(Files.exists(installedRoot))
      assert(Files.exists(getTestDirectory / "enso-config"))
      assert(Files.exists(dataFile), "Should not remove unknown files.")
      assert(Files.exists(configFile), "Should not remove unknown files.")
      assert(
        Files.notExists(getTestDirectory / "enso-config" / "global-config.yml"),
        "But the known ones should be removed."
      )
      assert(
        Files.notExists(installedRoot / "dist"),
        "But the known ones should be removed."
      )
      assert(Files.notExists(launcher))
    }
  }
}
