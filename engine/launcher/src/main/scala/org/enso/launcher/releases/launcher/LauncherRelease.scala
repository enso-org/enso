package org.enso.launcher.releases.launcher

import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.cli.task.TaskProgress
import org.enso.runtimeversionmanager.CurrentVersion

/** Represents a launcher release.
  */
trait LauncherRelease {

  /** Version of the release.
    */
  def version: SemVer

  /** Minimum version of the launcher that is required to upgrade to this
    * release.
    */
  def minimumVersionToPerformUpgrade: SemVer =
    manifest.minimumVersionForUpgrade

  /** Manifest associated with the release.
    */
  def manifest: LauncherManifest

  /** Specifies if the release is marked as broken and should not generally be
    * used unless explicitly asked to.
    */
  def isMarkedBroken: Boolean

  /** Name of the asset containing the launcher package for the current platform
    * inside of this release.
    */
  def packageFileName: String

  /** Downloads the launcher package to the specified destination.
    */
  def downloadPackage(path: Path): TaskProgress[Unit]

  /** Checks if the current launcher version is allowed to upgrade directly to
    * this release.
    *
    * If false, a multi-step upgrade must be performed.
    */
  def canPerformUpgradeFromCurrentVersion: Boolean =
    CurrentVersion.version >= minimumVersionToPerformUpgrade
}
