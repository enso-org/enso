package org.enso.launcher.releases

import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.cli.TaskProgress
import org.enso.launcher.OS
import org.enso.launcher.components.Manifest
import org.enso.launcher.releases.github.GithubReleaseProvider

import scala.util.{Failure, Try}

/**
  * Wraps a generic [[SimpleReleaseProvider]] to provide engine releases.
  */
class EngineReleaseProvider(releaseProvider: SimpleReleaseProvider)
    extends EnsoReleaseProvider[EngineRelease](releaseProvider) {

  /**
    * Fetches release metadata for a given version.
    */
  def fetchRelease(version: SemVer): Try[EngineRelease] = {
    val tag = tagPrefix + version.toString
    for {
      release <- releaseProvider.releaseForTag(tag)
      manifestAsset <-
        release.assets
          .find(_.fileName == Manifest.DEFAULT_MANIFEST_NAME)
          .toRight(
            ReleaseProviderException(
              s"${Manifest.DEFAULT_MANIFEST_NAME} file is missing from " +
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
    } yield DefaultEngineRelease(
      version  = version,
      isBroken = release.isMarkedBroken,
      manifest = manifest,
      release  = release
    )
  }

  case class DefaultEngineRelease(
    version: SemVer,
    manifest: Manifest,
    isBroken: Boolean,
    release: Release
  ) extends EngineRelease {

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

    /**
      * Downloads the package associated with the release into `destination`.
      *
      * @param destination name of the file that will be created to contain the
      *                    downloaded package
      */
    def downloadPackage(
      destination: Path
    ): TaskProgress[Unit] = {
      val packageName = packageFileName
      release.assets
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
