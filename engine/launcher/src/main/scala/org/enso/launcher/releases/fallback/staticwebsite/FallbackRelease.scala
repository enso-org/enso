package org.enso.launcher.releases.fallback.staticwebsite

import org.enso.launcher.releases.{Asset, Release}

/**
  * A [[Release]] provided by the [[FileStorageFallbackReleaseProvider]].
  *
  * @param tag tag identifying the release
  * @param assetNames filenames of assets included in the release
  * @param storage reference to the underlying [[FileStorage]] that provides the
  *                assets
  * @param root path to the release directory in the [[storage]]
  */
case class FallbackRelease(
  tag: String,
  assetNames: Seq[String],
  storage: FileStorage,
  root: Seq[String]
) extends Release {

  /**
    * @inheritdoc
    */
  override def assets: Seq[Asset] =
    assetNames.map(name => FallbackAsset(name, storage, root))
}

object FallbackRelease {

  /**
    * Creates a [[FallbackRelease]] from a [[ReleaseDescription]], a
    * [[FileStorage]] and a path representing the root of the releases
    * directory in that storage.
    */
  def fromDescription(
    description: ReleaseDescription,
    storage: FileStorage,
    releasesRoot: Seq[String]
  ): FallbackRelease = {
    val tag = description.tag
    FallbackRelease(
      tag        = tag,
      assetNames = description.assetNames,
      storage    = storage,
      root       = releasesRoot ++ Seq(tag)
    )
  }
}
