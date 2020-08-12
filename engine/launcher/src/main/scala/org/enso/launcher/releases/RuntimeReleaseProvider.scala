package org.enso.launcher.releases

import java.nio.file.Path

import org.enso.cli.TaskProgress
import org.enso.launcher.components.RuntimeVersion

/**
  * Interface for a service providing runtime releases.
  */
trait RuntimeReleaseProvider {

  /**
    * Determines filename of the package that should be downloaded from the
    * release for a given version.
    *
    * The result of this function may be system specific (the package name may
    * include the OS, for example).
    */
  def packageFileName(version: RuntimeVersion): String

  /**
    * Downloads a package for the given version to the provided location.
    * @param version runtime version to download
    * @param destination name of the file that will be created to contain the
    *                    downloaded package
    * @return [[TaskProgress]] allowing to track progress of the download and
    *         wait for its completion
    */
  def downloadPackage(
    version: RuntimeVersion,
    destination: Path
  ): TaskProgress[Unit]
}
