package org.enso.launcher.releases

object Placeholder2

import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.cli.TaskProgress
import org.enso.launcher.OS
import org.enso.launcher.releases.github.GithubReleaseProvider
import org.enso.launcher.components.Manifest

import scala.util.{Failure, Success, Try}

case class EngineRelease(
  version: SemVer,
  manifest: Manifest,
  release: Release
) {
  def packageFileName: String = {
    val os = OS.operatingSystem match {
      case OS.Linux   => "linux"
      case OS.MacOS   => "macos"
      case OS.Windows => "windows"
    }
    val arch = OS.architecture
    val extension = OS.operatingSystem match {
      case OS.Linux   => ".tar.gz"
      case OS.MacOS   => ".tar.gz"
      case OS.Windows => ".zip"
    }
    s"enso-engine-$version-$os-$arch$extension"
  }
}

class EngineReleaseProvider(releaseProvider: ReleaseProvider) {
  private val tagPrefix = "enso-"

  def findLatest(): Try[SemVer] =
    releaseProvider.listReleases().flatMap { releases =>
      val versions =
        releases.map(_.tag.stripPrefix(tagPrefix)).flatMap(SemVer(_))
      versions.sorted.lastOption.map(Success(_)).getOrElse {
        Failure(ReleaseProviderException("No valid engine versions were found"))
      }
    }

  def getRelease(version: SemVer): Try[EngineRelease] = {
    val tag = tagPrefix + version.toString
    for {
      release <- releaseProvider.releaseForVersion(tag)
      manifestAsset <-
        release.assets
          .find(_.fileName == Manifest.DEFAULT_MANIFEST_NAME)
          .toRight(
            ReleaseProviderException(
              s"${Manifest.DEFAULT_MANIFEST_NAME} file is mising from " +
              s"release assets."
            )
          )
          .toTry
      manifestContent <- manifestAsset.fetchAsText().waitForResult()
      manifest <-
        Manifest
          .fromYaml(manifestContent)
          .toRight(
            ReleaseProviderException("Cannot parse attached manifest file.")
          )
          .toTry
    } yield EngineRelease(version, manifest, release)
  }

  def downloadPackage(
    release: EngineRelease,
    destination: Path
  ): TaskProgress[Unit] = {
    val packageName = release.packageFileName
    release.release.assets
      .find(_.fileName == packageName)
      .map(_.downloadTo(destination))
      .getOrElse {
        TaskProgress.immediateFailure(
          ReleaseProviderException(
            s"Cannot find package `$packageName` in the release."
          )
        )
      }
  }
}

object EngineReleaseProvider
    extends EngineReleaseProvider(
      new GithubReleaseProvider(
        "enso-org",
        "enso-staging"
      ) // TODO [RW] move from staging to the main repository */
    )
