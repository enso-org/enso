package org.enso.runtimeversionmanager.releases.github

import org.enso.runtimeversionmanager.releases.{Asset, Release, ReleaseProvider}

case class GithubRelease(release: GithubAPI.Release) extends Release {

  /** @inheritdoc */
  override def tag: String = ReleaseProvider.TagPrefix + release.tag

  /** @inheritdoc */
  override def assets: Seq[Asset] = release.assets.map(GithubAsset)
}
