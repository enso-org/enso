package org.enso.launcher.releases.fallback.staticwebsite

import java.nio.file.Path

import org.enso.cli.TaskProgress

/**
  * A generic file storage that allows to download files or read them as
  * [[String]].
  *
  * Can be backed by a local filesystem or a web service.
  *
  * Files are identified by paths which are sequences of strings.
  */
trait FileStorage {

  /**
    * Downloads the file from storage `path` and saves it into `destination` on
    * the local filesystem.
    */
  def download(path: Seq[String], destination: Path): TaskProgress[Unit]

  /**
    * Fetches the file from storage `path` as a [[String]].
    */
  def fetchString(path: Seq[String]): TaskProgress[String]
}
