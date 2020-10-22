package org.enso.launcher.releases.engine

import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.cli.TaskProgress
import org.enso.launcher.components.Manifest
import org.enso.launcher.releases.{
  EnsoReleaseProvider,
  Release,
  ReleaseProviderException,
  SimpleReleaseProvider
}

import scala.util.{Failure, Try}

/** Wraps a generic [[SimpleReleaseProvider]] to provide engine releases.
  */
class EngineReleaseProvider(releaseProvider: SimpleReleaseProvider)
    extends EnsoReleaseProvider[EngineRelease](releaseProvider) {

  /** @inheritdoc
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

  private case class DefaultEngineRelease(
    version: SemVer,
    manifest: Manifest,
    isBroken: Boolean,
    release: Release
  ) extends EngineRelease {
    def packageFileName: String =
      EnsoReleaseProvider.packageNameForComponent("engine", version)

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
