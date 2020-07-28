package org.enso.launcher.releases.github

import org.enso.launcher.releases.{Release, ReleaseProvider}

import scala.annotation.unused

class GithubReleaseProvider(
  @unused organisation: String,
  @unused projectName: String
) extends ReleaseProvider {
  override def releaseForVersion(tag: String): Option[Release] =
    ???

  // TODO possibly change to `listReleases`
  override def latestRelease(): Option[Release] = ???
}
