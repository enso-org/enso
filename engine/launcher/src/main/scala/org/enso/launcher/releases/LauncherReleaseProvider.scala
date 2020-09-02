package org.enso.launcher.releases

import nl.gn0s1s.bump.SemVer

import scala.util.Try

trait LauncherReleaseProvider {
  def findLatest():                Try[LauncherRelease]
  def getRelease(version: SemVer): Try[LauncherRelease]
}
