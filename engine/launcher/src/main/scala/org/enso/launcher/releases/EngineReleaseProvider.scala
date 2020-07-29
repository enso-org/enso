package org.enso.launcher.releases

object Placeholder2

import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.releases.github.GithubReleaseProvider
import org.enso.launcher.components.Manifest

import scala.util.{Failure, Success, Try}

case class EngineRelease(version: SemVer, manifest: Manifest, release: Release)

class EngineReleaseProvider(releaseProvider: ReleaseProvider) {
  def findLatest(): Try[SemVer] =
    releaseProvider.listReleases().flatMap { releases =>
      val versions = releases.map(_.tag).flatMap(SemVer(_))
      versions.sorted.lastOption.map(Success(_)).getOrElse {
        Failure(new RuntimeException("No valid engine versions were found"))
      }
    }

  def getRelease(version: SemVer): Try[EngineRelease] = {
    val tag = s"enso-$version"
    for {
      release <- releaseProvider.releaseForVersion(tag)
      manifestAsset <-
        release.assets
          .find(_.fileName == Manifest.DEFAULT_MANIFEST_NAME)
          .toRight(
            new RuntimeException(
              s"${Manifest.DEFAULT_MANIFEST_NAME} file is mising from " +
              s"release assets"
            )
          )
          .toTry
      manifestContent <- manifestAsset.fetchAsText().waitForResult()
      manifest <-
        Manifest
          .fromYaml(manifestContent)
          .toRight(new RuntimeException("Cannot parse manifest"))
          .toTry
    } yield EngineRelease(version, manifest, release)
  }

  def downloadPackage(
    release: EngineRelease,
    destination: Path
  ): PendingDownload[Unit] = {
    val version     = release.version
    val packageName = s"enso-engine-$version.zip"
    release.release.assets
      .find(_.fileName == packageName)
      .map(_.downloadTo(destination))
      .getOrElse {
        PendingDownload.immediateFailure(
          new RuntimeException(
            s"Cannot find package `$packageName` in the release."
          )
        )
      }
  }
}

object EngineReleaseProvider
    extends EngineReleaseProvider(
      new GithubReleaseProvider("enso-org", "enso")
    )
