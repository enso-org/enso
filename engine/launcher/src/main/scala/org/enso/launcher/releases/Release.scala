package org.enso.launcher.releases

import java.nio.file.Path

trait Asset {
  def fileName:               String
  def downloadTo(path: Path): Unit
  def fetchAsText():          String
}

trait Release {
  def tag:    String
  def assets: Seq[String]
}

trait ReleaseProvider {
  def releaseForVersion(tag: String): Option[Release]
  def latestRelease():                Option[Release]
}
