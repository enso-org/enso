package org.enso.launcher.releases.fallback.staticwebsite

import java.nio.file.Path

import org.enso.cli.TaskProgress
import org.enso.launcher.releases.Asset

/**
  * An [[Asset]] provided by [[FallbackRelease]].
  *
  * @param fileName filename of the asset
  * @param storage reference to the [[FileStorage]] associated with the
  *                [[FileStorageFallbackReleaseProvider]] that this asset comes
  *                from, used to access its data
  * @param releaseRoot path to the directory containing assets for the release
  *                    this asset belongs to
  */
case class FallbackAsset(
  fileName: String,
  storage: FileStorage,
  releaseRoot: Seq[String]
) extends Asset {

  /**
    * @inheritdoc
    */
  override def downloadTo(path: Path): TaskProgress[Unit] =
    storage.download(storagePath, destination = path)

  /**
    * @inheritdoc
    */
  override def fetchAsText(): TaskProgress[String] =
    storage.fetchString(storagePath)

  private def storagePath = releaseRoot ++ Seq(fileName)
}
