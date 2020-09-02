package org.enso.launcher.releases

import java.nio.file.Path

import org.enso.cli.TaskProgress

/**
  * Represents a downloadable release asset.
  */
trait Asset {
  def fileName:               String
  def downloadTo(path: Path): TaskProgress[Unit]
  def fetchAsText():          TaskProgress[String]
}
