package org.enso.runtimeversionmanager.releases.graalvm

import java.nio.file.Path

import org.enso.cli.task.TaskProgress
import org.enso.runtimeversionmanager.components.GraalVMVersion

/** Interface for a service providing GraalVM runtime releases. */
trait GraalVMRuntimeReleaseProvider {

  /** Determines filename of the package that should be downloaded from the
    * release for a given version.
    *
    * The result of this function may be system specific (the package name may
    * include the OS, for example).
    */
  def packageFileName(version: GraalVMVersion): String

  /** Downloads a package for the given version to the provided location.
    * @param version runtime version to download
    * @param destination name of the file that will be created to contain the
    *                    downloaded package
    * @return [[TaskProgress]] allowing to track progress of the download and
    *         wait for its completion
    */
  def downloadPackage(
    version: GraalVMVersion,
    destination: Path
  ): TaskProgress[Unit]
}
