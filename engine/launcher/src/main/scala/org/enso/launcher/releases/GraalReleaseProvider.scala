package org.enso.launcher.releases

import java.nio.file.Path

import org.enso.launcher.components.RuntimeVersion
import org.enso.launcher.releases.github.GithubReleaseProvider

import scala.util.{Failure, Success}

class GraalReleaseProvider(releaseProvider: ReleaseProvider) {
  def downloadPackage(
    version: RuntimeVersion,
    path: Path
  ): PendingDownload[Unit] = {
    val tagName     = s"vm-{${version.graal}"
    val packageName = s"${version.graal}-java${version.java} TODO"
    val release     = releaseProvider.releaseForVersion(tagName)
    release match {
      case Failure(exception) =>
        PendingDownload.immediateFailure(exception)
      case Success(release) =>
        release.assets
          .find(_.fileName == packageName)
          .map(_.downloadTo(path))
          .getOrElse {
            PendingDownload.immediateFailure(
              new RuntimeException(
                s"Cannot find package `$packageName` in the release."
              )
            )
          }
    }
  }
}

object GraalReleaseProvider
    extends GraalReleaseProvider(
      new GithubReleaseProvider("graalvm", "graalvm-ce-builds")
    )
