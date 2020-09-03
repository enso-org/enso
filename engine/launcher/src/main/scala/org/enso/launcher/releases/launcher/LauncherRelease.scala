package org.enso.launcher.releases.launcher

import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.cli.TaskProgress
import org.enso.launcher.CurrentVersion

trait LauncherRelease {
  def version:                        SemVer
  def minimumVersionToPerformUpgrade: SemVer
  def manifest:                       LauncherManifest
  def packageFileName:                String
  def downloadPackage(path: Path):    TaskProgress[Unit]

  def canPerformUpgradeFromCurrentVersion: Boolean =
    CurrentVersion.version >= minimumVersionToPerformUpgrade
}
