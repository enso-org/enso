package org.enso.launcher.releases

object Placeholder2

import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.releases.github.GithubReleaseProvider
import org.enso.launcher.components.Manifest

import scala.util.{Failure, Success, Try}

case class EngineRelease(
  version: SemVer,
  manifest: Manifest,
  release: Release
) {
  def packageFileName: String = s"enso-engine-$version.zip"
}

class EngineReleaseProvider(releaseProvider: ReleaseProvider) {
  private val tagPrefix = "enso-"

  def findLatest(): Try[SemVer] =
    releaseProvider.listReleases().flatMap { releases =>
      val versions =
        releases.map(_.tag.stripPrefix(tagPrefix)).flatMap(SemVer(_))
      versions.sorted.lastOption.map(Success(_)).getOrElse {
        Failure(new RuntimeException("No valid engine versions were found"))
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
    val packageName = release.packageFileName
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
      new GithubReleaseProvider(
        "enso-org",
        "enso-staging"
      ) // TODO [RW] move from staging to the main repository
    )
