package org.enso.launcher.releases.runtime

import java.nio.file.Path

import org.enso.cli.TaskProgress
import org.enso.launcher.OS
import org.enso.launcher.components.RuntimeVersion
import org.enso.launcher.releases.github.GithubReleaseProvider
import org.enso.launcher.releases.{
  ReleaseProviderException,
  SimpleReleaseProvider
}

import scala.util.{Failure, Success}

/** [[RuntimeReleaseProvider]] implementation providing Graal Community Edition
  * releases from the given [[SimpleReleaseProvider]].
  */
class GraalCEReleaseProvider(releaseProvider: SimpleReleaseProvider)
    extends RuntimeReleaseProvider {

  /** @inheritdoc
    */
  override def packageFileName(version: RuntimeVersion): String = {
    val os = OS.operatingSystem match {
      case OS.Linux   => "linux"
      case OS.MacOS   => "darwin"
      case OS.Windows => "windows"
    }
    val arch = OS.architecture
    val extension = OS.operatingSystem match {
      case OS.Linux   => ".tar.gz"
      case OS.MacOS   => ".tar.gz"
      case OS.Windows => ".zip"
    }
    s"graalvm-ce-java${version.java}-$os-$arch-${version.graal}$extension"
  }

  /** @inheritdoc
    */
  override def downloadPackage(
    version: RuntimeVersion,
    destination: Path
  ): TaskProgress[Unit] = {
    val tagName     = s"vm-${version.graal}"
    val packageName = packageFileName(version)
    val release     = releaseProvider.releaseForTag(tagName)
    release match {
      case Failure(exception) =>
        TaskProgress.immediateFailure(exception)
      case Success(release) =>
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

/** Default [[RuntimeReleaseProvider]] that provides Graal CE releases using the
  * GitHub Release API.
  */
object GraalCEReleaseProvider
    extends GraalCEReleaseProvider(
      new GithubReleaseProvider("graalvm", "graalvm-ce-builds")
    )
