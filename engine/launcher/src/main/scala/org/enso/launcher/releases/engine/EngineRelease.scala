package org.enso.launcher.releases.engine

import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.cli.TaskProgress
import org.enso.launcher.components.Manifest

/**
  * Represents an engine release.
  */
trait EngineRelease {

  /**
    * Engine version.
    */
  def version: SemVer

  /**
    * Manifest associated with the release.
    *
    * @return
    */
  def manifest: Manifest

  /**
    * Specifies whether this release is marked as broken.
    * @return
    */
  def isBroken: Boolean

  /**
    * Determines the filename of the package that should be downloaded from this
    * release.
    *
    * That filename may be platform specific.
    */
  def packageFileName: String

  /**
    * Downloads the package associated with the release into `destination`.
    *
    * @param destination name of the file that will be created to contain the
    *                    downloaded package
    */
  def downloadPackage(
    destination: Path
  ): TaskProgress[Unit]
}
