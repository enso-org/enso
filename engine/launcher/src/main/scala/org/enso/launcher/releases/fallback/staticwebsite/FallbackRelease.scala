package org.enso.launcher.releases.fallback.staticwebsite

import org.enso.launcher.http.URIBuilder
import org.enso.launcher.releases.{Asset, Release}

case class FallbackRelease(
  tag: String,
  assetNames: Seq[String],
  root: URIBuilder
) extends Release {

  /**
    * @inheritdoc
    */
  override def assets: Seq[Asset] =
    assetNames.map(name => FallbackAsset(name, root))
}

object FallbackRelease {

  /**
    * Creates a [[FallbackRelease]] from a [[ReleaseDescription]] and a
    * [[URIBuilder]] representing the root of the releases directory.
    */
  def fromDescription(
    description: ReleaseDescription,
    releasesRoot: URIBuilder
  ): FallbackRelease = {
    val tag = description.tag
    FallbackRelease(
      tag        = tag,
      assetNames = description.assetNames,
      root       = releasesRoot / tag
    )
  }
}
