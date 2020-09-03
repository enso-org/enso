package org.enso.launcher.releases
import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.cli.TaskProgress

import scala.util.Try

/**
  * Wraps a generic [[SimpleReleaseProvider]] to provide launcher releases.
  */
class LauncherReleaseProvider(releaseProvider: SimpleReleaseProvider)
    extends EnsoReleaseProvider[LauncherRelease](releaseProvider) {
  override def fetchRelease(version: SemVer): Try[LauncherRelease] = ???

  case class GitHubLauncherRelease(release: Release) extends LauncherRelease {
    override def version: SemVer                                 = ???
    override def minimumVersionToPerformUpgrade: SemVer          = ???
    override def downloadPackage(path: Path): TaskProgress[Unit] = ???
  }
}
