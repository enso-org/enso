package org.enso.launcher.releases

import java.nio.file.Path

import org.enso.cli.TaskProgress
import org.enso.launcher.OS
import org.enso.launcher.components.RuntimeVersion
import org.enso.launcher.releases.github.GithubReleaseProvider

import scala.util.{Failure, Success}

class GraalCEReleaseProvider(releaseProvider: ReleaseProvider)
    extends RuntimeReleaseProvider {
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

  def downloadPackage(
    version: RuntimeVersion,
    path: Path
  ): TaskProgress[Unit] = {
    val tagName     = s"vm-${version.graal}"
    val packageName = packageFileName(version)
    val release     = releaseProvider.releaseForVersion(tagName)
    release match {
      case Failure(exception) =>
        TaskProgress.immediateFailure(exception)
      case Success(release) =>
        release.assets
          .find(_.fileName == packageName)
          .map(_.downloadTo(path))
          .getOrElse {
            TaskProgress.immediateFailure(
              new RuntimeException(
                s"Cannot find package `$packageName` in the release."
              )
            )
          }
    }
  }
}

object GraalCEReleaseProvider
    extends GraalCEReleaseProvider(
      new GithubReleaseProvider("graalvm", "graalvm-ce-builds")
    )
