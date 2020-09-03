package org.enso.launcher.releases.launcher

import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.cli.TaskProgress

trait LauncherRelease {
  def version:                        SemVer
  def minimumVersionToPerformUpgrade: SemVer
  def downloadPackage(path: Path):    TaskProgress[Unit]
}
