package org.enso.launcher.installation

import java.nio.file.{Files, Path}

import org.enso.launcher.{FileSystem, NativeTest, WithTemporaryDirectory}
import org.enso.launcher.FileSystem.PathSyntax
import org.enso.launcher.internal.OS

import scala.io.Source

class InstallerSpec extends NativeTest with WithTemporaryDirectory {
  def portableRoot = getTestDirectory / "portable"
  def portableLauncher =
    portableRoot / "bin" / OS.executableName("enso")

  def preparePortableDistribution(): Unit = {
    copyLauncherTo(portableLauncher)
    FileSystem.writeTextFile(portableRoot / ".enso.portable", "mark")
    Files.createDirectories(portableRoot / "config")
    FileSystem.writeTextFile(
      portableRoot / "config" / "global-config.yml",
      "what: ever"
    )
  }

  def installedRoot = getTestDirectory / "installed"
  def env =
    Map(
      "ENSO_DATA_DIRECTORY"   -> (installedRoot / "data").toString,
      "ENSO_CONFIG_DIRECTORY" -> (installedRoot / "config").toString,
      "ENSO_BIN_DIRECTORY"    -> (installedRoot / "bin").toString
    )

  def prepareBundles(): Unit = {
    val engineBundle  = portableRoot / "dist" / "engine1"
    val runtimeBundle = portableRoot / "runtime" / "jvm1"

    Files.createDirectories(engineBundle)
    FileSystem.writeTextFile(engineBundle / "engine.txt", "")

    Files.createDirectories(runtimeBundle)
    FileSystem.writeTextFile(runtimeBundle / "jvm.txt", "")
  }

  def readFileContent(path: Path): String = {
    val source = Source.fromFile(path.toFile)
    try {
      source.getLines().mkString("\n")
    } finally {
      source.close()
    }
  }

  private def notExistsAfterSomeTime(path: Path, retry: Int = 5): Boolean = {
    if (Files.notExists(path)) true
    else if (retry > 0) {
      Thread.sleep(200)
      notExistsAfterSomeTime(path, retry - 1)
    } else false
  }

  "enso install distribution" should {
    "install itself" in {
      preparePortableDistribution()
      runLauncherAt(
        portableLauncher,
        Seq("install", "distribution", "--auto-confirm"),
        env
      )

      (installedRoot / "bin" / OS.executableName("enso")).toFile should exist
      assert(
        Files.isExecutable(installedRoot / "bin" / OS.executableName("enso")),
        "The installed file should be executable."
      )

      val config = installedRoot / "config" / "global-config.yml"
      config.toFile should exist
      readFileContent(config).stripTrailing() shouldEqual "what: ever"

      assert(
        notExistsAfterSomeTime(portableLauncher),
        "The installer should remove itself."
      )
    }

    "move bundles by default" in {
      preparePortableDistribution()
      prepareBundles()
      runLauncherAt(
        portableLauncher,
        Seq("install", "distribution", "--auto-confirm"),
        env
      )

      (installedRoot / "data" / "dist" / "engine1" / "engine.txt").toFile should
      exist
      (installedRoot / "data" / "runtime" / "jvm1" / "jvm.txt").toFile should
      exist

      assert(
        Files.notExists(portableRoot / "dist" / "engine1"),
        "Bundles should have been removed."
      )
      assert(
        Files.notExists(portableRoot / "runtime" / "jvm1"),
        "Bundles should have been removed."
      )
    }

    "copy bundles if told to do so" in {
      preparePortableDistribution()
      prepareBundles()
      runLauncherAt(
        portableLauncher,
        Seq(
          "install",
          "distribution",
          "--auto-confirm",
          "--bundle-install-mode=copy"
        ),
        env
      )

      (installedRoot / "data" / "dist" / "engine1" / "engine.txt").toFile should
      exist
      (installedRoot / "data" / "runtime" / "jvm1" / "jvm.txt").toFile should
      exist

      (portableRoot / "dist" / "engine1").toFile should
      exist
      (portableRoot / "runtime" / "jvm1").toFile should
      exist
    }

    "ignore bundles when told to do so" in {
      preparePortableDistribution()
      prepareBundles()
      runLauncherAt(
        portableLauncher,
        Seq(
          "install",
          "distribution",
          "--auto-confirm",
          "--bundle-install-mode=ignore"
        ),
        env
      )

      assert(
        Files.notExists(installedRoot / "data" / "dist" / "engine1"),
        "Bundles should not have been installed."
      )
      assert(
        Files.notExists(installedRoot / "data" / "runtime" / "jvm1"),
        "Bundles should not have been installed."
      )
    }
  }
}
