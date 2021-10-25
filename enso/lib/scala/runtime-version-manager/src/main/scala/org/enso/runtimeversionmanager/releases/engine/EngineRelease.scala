package org.enso.runtimeversionmanager.releases.engine

import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.cli.task.TaskProgress
import org.enso.runtimeversionmanager.components.Manifest

/** Represents an engine release. */
trait EngineRelease {

  /** Engine version. */
  def version: SemVer

  /** Manifest associated with the release. */
  def manifest: Manifest

  /** Specifies whether this release is marked as broken. */
  def isBroken: Boolean

  /** Determines the filename of the package that should be downloaded from this
    * release.
    *
    * That filename may be platform specific.
    */
  def packageFileName: String

  /** Downloads the package associated with the release into `destination`.
    *
    * @param destination name of the file that will be created to contain the
    *                    downloaded package
    */
  def downloadPackage(
    destination: Path
  ): TaskProgress[Unit]
}
