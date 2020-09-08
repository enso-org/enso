package org.enso.launcher.releases

import java.nio.file.Path

import org.enso.cli.TaskProgress

/**
  * Represents a downloadable release asset.
  */
trait Asset {

  /**
    * Asset's filename.
    */
  def fileName: String

  /**
    * Downloads the asset to the provided path.
    *
    * The path should include a filename for the asset (not just a parent
    * directory).
    *
    * Returns a [[TaskProgress]] instance that is completed when the download
    * finishes.
    */
  def downloadTo(path: Path): TaskProgress[Unit]

  /**
    * Fetches the asset treating it as text data.
    *
    * Returns a [[TaskProgress]] instance that will contain a [[String]]
    * containing the fetched text.
    */
  def fetchAsText(): TaskProgress[String]
}
