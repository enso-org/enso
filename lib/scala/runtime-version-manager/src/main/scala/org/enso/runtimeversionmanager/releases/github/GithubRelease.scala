package org.enso.runtimeversionmanager.releases.github

import org.enso.runtimeversionmanager.releases.{Asset, Release}

case class GithubRelease(release: GithubAPI.Release) extends Release {

  /** @inheritdoc
    */
  override def tag: String = release.tag

  /** @inheritdoc
    */
  override def assets: Seq[Asset] = release.assets.map(GithubAsset)
}
