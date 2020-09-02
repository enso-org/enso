package org.enso.launcher.releases
import java.nio.file.Path

import nl.gn0s1s.bump.SemVer
import org.enso.cli.TaskProgress

import scala.util.Try

class GithubLauncherReleaseProvider(releaseProvider: ReleaseProvider)
    extends LauncherReleaseProvider {
  case class GitHubLauncherRelease(release: Release) extends LauncherRelease {
    override def version: SemVer                                 = ???
    override def minimumVersionToPerformUpgrade: SemVer          = ???
    override def downloadPackage(path: Path): TaskProgress[Unit] = ???
  }

  override def findLatest(): Try[LauncherRelease] = ???

  override def getRelease(version: SemVer): Try[LauncherRelease] = ???
}
