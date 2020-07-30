package org.enso.launcher.releases

import java.nio.file.Path

import org.enso.cli.TaskProgress

import scala.util.Try

trait Asset {
  def fileName:               String
  def downloadTo(path: Path): TaskProgress[Unit]
  def fetchAsText():          TaskProgress[String]
}

trait Release {
  def tag:    String
  def assets: Seq[Asset]
}

trait ReleaseProvider {
  def releaseForVersion(tag: String): Try[Release]
  def listReleases():                 Try[Seq[Release]]
}
