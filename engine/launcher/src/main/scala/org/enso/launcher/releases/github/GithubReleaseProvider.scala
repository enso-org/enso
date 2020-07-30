package org.enso.launcher.releases.github

import java.nio.file.Path

import org.enso.cli.TaskProgress
import org.enso.launcher.releases.{Asset, Release, ReleaseProvider}

import scala.util.Try

class GithubReleaseProvider(
  organisation: String,
  projectName: String
) extends ReleaseProvider {
  private val repo = GithubAPI.Repository(organisation, projectName)

  override def releaseForVersion(tag: String): Try[Release] =
    GithubAPI.getRelease(repo, tag).waitForResult().map(wrapRelease)

  override def listReleases(): Try[Seq[Release]] =
    GithubAPI.listReleases(repo).map(_.map(wrapRelease))

  private def wrapRelease(release: GithubAPI.Release): Release =
    new Release {
      override def tag: String        = release.tag
      override def assets: Seq[Asset] = release.assets.map(wrapAsset)
    }

  private def wrapAsset(asset: GithubAPI.Asset): Asset =
    new Asset {
      override def fileName: String = asset.name
      override def downloadTo(path: Path): TaskProgress[Unit] =
        GithubAPI.downloadAsset(asset, path)
      override def fetchAsText(): TaskProgress[String] =
        GithubAPI.fetchTextAsset(asset)
    }
}
