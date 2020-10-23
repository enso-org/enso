package org.enso.componentmanager.releases.github

import org.enso.componentmanager.releases.{Asset, Release}

case class GithubRelease(release: GithubAPI.Release) extends Release {

  /** @inheritdoc
    */
  override def tag: String = release.tag

  /** @inheritdoc
    */
  override def assets: Seq[Asset] = release.assets.map(GithubAsset)
}
