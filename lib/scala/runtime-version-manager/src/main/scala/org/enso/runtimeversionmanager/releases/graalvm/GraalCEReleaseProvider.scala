package org.enso.runtimeversionmanager.releases.graalvm

import java.nio.file.Path

import org.enso.cli.TaskProgress
import org.enso.runtimeversionmanager.OS
import org.enso.runtimeversionmanager.components.GraalVMVersion
import org.enso.runtimeversionmanager.releases.github.GithubReleaseProvider
import org.enso.runtimeversionmanager.releases.{
  ReleaseProviderException,
  SimpleReleaseProvider
}

import scala.util.{Failure, Success}

/** [[GraalVMRuntimeReleaseProvider]] implementation providing Graal Community
  * Edition releases from the given [[SimpleReleaseProvider]].
  */
class GraalCEReleaseProvider(releaseProvider: SimpleReleaseProvider)
    extends GraalVMRuntimeReleaseProvider {

  /** @inheritdoc */
  override def packageFileName(version: GraalVMVersion): String = {
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
    s"graalvm-ce-java${version.java}-$os-$arch-${version.graalVersion}$extension"
  }

  /** @inheritdoc */
  override def downloadPackage(
    version: GraalVMVersion,
    destination: Path
  ): TaskProgress[Unit] = {
    val tagName     = s"vm-${version.graalVersion}"
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

/** Default [[GraalVMRuntimeReleaseProvider]] that provides Graal CE releases
  * using the GitHub Release API.
  */
object GraalCEReleaseProvider
    extends GraalCEReleaseProvider(
      new GithubReleaseProvider("graalvm", "graalvm-ce-builds")
    )
