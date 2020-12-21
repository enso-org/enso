package org.enso.runtimeversionmanager.components

import java.nio.file.Files

import nl.gn0s1s.bump.SemVer
import org.enso.runtimeversionmanager.FileSystem
import org.enso.runtimeversionmanager.FileSystem.PathSyntax
import org.enso.runtimeversionmanager.releases.EnsoReleaseProvider
import org.enso.runtimeversionmanager.releases.engine.EngineReleaseProvider
import org.enso.runtimeversionmanager.releases.graalvm.GraalCEReleaseProvider
import org.enso.runtimeversionmanager.releases.local.LocalReleaseProvider
import org.enso.runtimeversionmanager.releases.testing.TestArchivePackager
import org.enso.runtimeversionmanager.test.{
  FakeReleases,
  RuntimeVersionManagerTest
}

class LocalReleaseProviderSpec extends RuntimeVersionManagerTest {
  private def localEngines  = getTestDirectory / "offline-engine"
  private def localRuntimes = getTestDirectory / "offline-graal"

  private def makeRuntimeManagerWithLocal(): RuntimeVersionManager = {
    val engineProvider =
      new LocalReleaseProvider(localEngines, FakeReleases.baseEngineProvider)
    val runtimeProvider =
      new LocalReleaseProvider(localRuntimes, FakeReleases.baseRuntimeProvider)
    makeManagers(
      engineProvider  = new EngineReleaseProvider(engineProvider),
      runtimeProvider = new GraalCEReleaseProvider(runtimeProvider)
    )._2
  }

  private def prepareLocalRepository(): Unit = {
    Files.createDirectories(localEngines)
    Files.createDirectories(localRuntimes)

    {
      val engineVersion   = SemVer(1, 2, 3, Some("local"))
      val engineName      = s"enso-$engineVersion"
      val destinationRoot = localEngines / engineName
      Files.createDirectories(destinationRoot)
      val manifest =
        """minimum-launcher-version: 0.0.1
          |minimum-project-manager-version: 0.0.1
          |graal-vm-version: 20.20.20-local
          |graal-java-version: 11
          |""".stripMargin
      val sourceArchive =
        EnsoReleaseProvider.packageNameForComponent("engine", SemVer(0, 0, 0))
      val tmpArchive = getTestDirectory / sourceArchive
      FileSystem.copyDirectory(
        FakeReleases.engineRoot / "enso-0.0.0" / sourceArchive / "enso-0.0.0",
        tmpArchive / engineName
      )
      FileSystem.writeTextFile(
        tmpArchive / engineName / "manifest.yaml",
        manifest
      )
      FileSystem.writeTextFile(destinationRoot / "manifest.yaml", manifest)
      val archive =
        EnsoReleaseProvider.packageNameForComponent("engine", engineVersion)
      TestArchivePackager.packArchive(
        tmpArchive,
        destinationRoot / archive
      )
    }

    {
      val sourceVersion =
        GraalVMVersion(SemVer(2, 0, 0), "11")
      val targetVersion =
        GraalVMVersion(SemVer(20, 20, 20, Some("local")), "11")
      val destinationRoot = localRuntimes / s"vm-${targetVersion.graalVersion}"
      Files.createDirectories(destinationRoot)
      val sourceRoot =
        FakeReleases.runtimeRoot / s"vm-${sourceVersion.graalVersion}"
      TestArchivePackager.packArchive(
        sourceRoot / GraalCEReleaseProvider.packageFileNameForCurrentOS(
          sourceVersion
        ),
        destinationRoot / GraalCEReleaseProvider.packageFileNameForCurrentOS(
          targetVersion
        )
      )
    }
  }

  "LocalReleaseProvider" should {
    "install a release from a local repository" in {
      prepareLocalRepository()
      val runtimeVersionManager = makeRuntimeManagerWithLocal()
      val engineVersion         = SemVer(1, 2, 3, Some("local"))
      val runtimeVersion =
        GraalVMVersion(SemVer(20, 20, 20, Some("local")), "11")
      runtimeVersionManager.findOrInstallEngine(engineVersion)
      runtimeVersionManager
        .listInstalledEngines()
        .map(_.version) shouldEqual Seq(engineVersion)
      runtimeVersionManager
        .listInstalledGraalRuntimes()
        .map(_.version) shouldEqual Seq(runtimeVersion)
    }

    "install a release from the fallback repository" in {
      val runtimeVersionManager = makeRuntimeManagerWithLocal()
      val engineVersion         = SemVer(0, 0, 0)
      runtimeVersionManager.findOrInstallEngine(engineVersion)
      runtimeVersionManager
        .listInstalledEngines()
        .map(_.version) shouldEqual Seq(engineVersion)
    }

    "include releases from both local and fallback" in {
      val localVersion = "enso-1.2.3-local"
      Files.createDirectories(localEngines / localVersion)

      val releaseProvider = new LocalReleaseProvider(
        localEngines,
        FakeReleases.baseEngineProvider
      )

      val tags = releaseProvider.listReleases().get.map(_.tag)
      tags should contain(localVersion)
      tags should contain("enso-0.0.0")
    }
  }
}
