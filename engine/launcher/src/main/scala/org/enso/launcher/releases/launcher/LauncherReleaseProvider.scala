package org.enso.launcher.releases.launcher

import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.cli.TaskProgress
import org.enso.launcher.releases.{
  EnsoReleaseProvider,
  Release,
  ReleaseProviderException,
  SimpleReleaseProvider
}

import scala.util.Try

/**
  * Wraps a generic [[SimpleReleaseProvider]] to provide launcher releases.
  */
class LauncherReleaseProvider(releaseProvider: SimpleReleaseProvider)
    extends EnsoReleaseProvider[LauncherRelease](releaseProvider) {
  override def fetchRelease(version: SemVer): Try[LauncherRelease] = {
    val tag = tagPrefix + version.toString
    for {
      release <- releaseProvider.releaseForTag(tag)
      manifestAsset <-
        release.assets
          .find(_.fileName == LauncherManifest.assetName)
          .toRight(
            ReleaseProviderException(
              s"${LauncherManifest.assetName} file is missing from release " +
              s"assets."
            )
          )
          .toTry
      manifestContent <- manifestAsset.fetchAsText().waitForResult()
      manifest        <- LauncherManifest.fromYAML(manifestContent)
    } yield GitHubLauncherRelease(version, manifest, release)
  }

  case class GitHubLauncherRelease(
    version: SemVer,
    manifest: LauncherManifest,
    release: Release
  ) extends LauncherRelease {
    def minimumVersionToPerformUpgrade: SemVer =
      manifest.minimumVersionToUpgrade
    override def downloadPackage(path: Path): TaskProgress[Unit] = ???
  }
}
