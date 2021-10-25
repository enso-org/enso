package org.enso.runtimeversionmanager.releases.engine

import java.nio.file.Path
import nl.gn0s1s.bump.SemVer
import org.enso.cli.task.TaskProgress
import org.enso.runtimeversionmanager.components.{Engine, Manifest}
import org.enso.runtimeversionmanager.releases.{
  EnsoReleaseProvider,
  Release,
  ReleaseNotFound,
  ReleaseProviderException,
  SimpleReleaseProvider
}

import scala.util.{Failure, Try}

/** Wraps a generic [[SimpleReleaseProvider]] to provide engine releases. */
class EngineReleaseProvider(releaseProvider: SimpleReleaseProvider)
    extends EnsoReleaseProvider[EngineRelease](releaseProvider) {

  /** @inheritdoc */
  def fetchRelease(version: SemVer): Try[EngineRelease] = {
    val tag = tagPrefix + version.toString
    for {
      release <- wrapFetchError(releaseProvider.releaseForTag(tag))
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
      manifestContent <- TaskProgress.waitForTask(manifestAsset.fetchAsText())
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

  /** Intercepts the `ReleaseNotFound` and injects a message regarding nightly
    * releases if applicable.
    */
  private def wrapFetchError[A](result: Try[A]): Try[A] = result.recoverWith {
    case ReleaseNotFound(tag, _, cause) if tag.contains(Engine.nightlyInfix) =>
      // TODO [RW] explain how to upgrade using the upgrade command once its
      //  implemented (#1717)
      Failure(
        ReleaseNotFound(
          tag,
          Some(
            s"Cannot find release `$tag`. Nightly releases expire after some " +
            s"time. Consider upgrading to a stable release or a newer " +
            s"nightly build."
          ),
          cause
        )
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
