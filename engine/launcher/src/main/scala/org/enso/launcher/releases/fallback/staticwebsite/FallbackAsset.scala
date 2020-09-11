package org.enso.launcher.releases.fallback.staticwebsite

import java.nio.file.Path

import org.enso.cli.TaskProgress
import org.enso.launcher.releases.Asset

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
