package org.enso.launcher.releases

object Placeholder2

import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.cli.TaskProgress
import org.enso.launcher.OS
import org.enso.launcher.releases.github.GithubReleaseProvider
import org.enso.launcher.components.Manifest

import scala.util.{Failure, Success, Try}

/**
  * Represents an engine release.
  *
  * @param version engine version
  * @param manifest manifest associated with the release
  * @param release a [[Release]] that allows to download assets
  */
case class EngineRelease(
  version: SemVer,
  manifest: Manifest,
  release: Release
) {

  /**
    * Determines the filename of the package that should be downloaded from this
    * release.
    *
    * That filename may be platform specific.
    */
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

/**
  * Wraps a generic [[ReleaseProvider]] to provide engine releases from it.
  */
class EngineReleaseProvider(releaseProvider: ReleaseProvider) {
  private val tagPrefix = "enso-"

  /**
    * Returns the version of the most recent engine release.
    */
  def findLatest(): Try[SemVer] =
    releaseProvider.listReleases().flatMap { releases =>
      val versions =
        releases.map(_.tag.stripPrefix(tagPrefix)).flatMap(SemVer(_))
      versions.sorted.lastOption.map(Success(_)).getOrElse {
        Failure(ReleaseProviderException("No valid engine versions were found"))
      }
    }

  /**
    * Fetches release metadata for a given version.
    */
  def getRelease(version: SemVer): Try[EngineRelease] = {
    val tag = tagPrefix + version.toString
    for {
      release <- releaseProvider.releaseForTag(tag)
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
          .recoverWith(error =>
            Failure(
              ReleaseProviderException(
                "Cannot parse attached manifest file.",
                error
              )
            )
          )
    } yield EngineRelease(version, manifest, release)
  }

  /**
    * Downloads the package associated with the given release into
    * `destination`.
    *
    * @param release the release to download the package from
    * @param destination name of the file that will be created to contain the
    * downloaded package
    */
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

/**
  * Default [[EngineReleaseProvider]] that uses the GitHub Release API.
  */
object EngineReleaseProvider
    extends EngineReleaseProvider(
      new GithubReleaseProvider(
        "enso-org",
        "enso-staging" // TODO [RW] The release provider will be moved from
        // staging to the main repository, when the first official Enso release
        // is released.
      )
    )
