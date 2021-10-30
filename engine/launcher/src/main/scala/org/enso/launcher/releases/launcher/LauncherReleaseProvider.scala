package org.enso.launcher.releases.launcher

import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.cli.task.TaskProgress
import org.enso.runtimeversionmanager.releases.{
  EnsoReleaseProvider,
  Release,
  ReleaseProviderException,
  SimpleReleaseProvider
}

import scala.util.Try

/** Wraps a generic [[SimpleReleaseProvider]] to provide launcher releases.
  */
class LauncherReleaseProvider(releaseProvider: SimpleReleaseProvider)
    extends EnsoReleaseProvider[LauncherRelease](releaseProvider) {

  /** @inheritdoc
    */
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
      manifestContent <- TaskProgress.waitForTask(manifestAsset.fetchAsText())
      manifest        <- LauncherManifest.fromYAML(manifestContent)
    } yield GitHubLauncherRelease(version, manifest, release)
  }

  private case class GitHubLauncherRelease(
    version: SemVer,
    manifest: LauncherManifest,
    release: Release
  ) extends LauncherRelease {
    override def packageFileName: String =
      EnsoReleaseProvider.packageNameForComponent("launcher", version)
    override def downloadPackage(path: Path): TaskProgress[Unit] = {
      val packageName = packageFileName
      release.assets
        .find(_.fileName == packageName)
        .map(_.downloadTo(path))
        .getOrElse {
          TaskProgress.immediateFailure(
            ReleaseProviderException(
              s"Cannot find package `$packageName` in the release."
            )
          )
        }
    }

    override def isMarkedBroken: Boolean = release.isMarkedBroken
  }
}
