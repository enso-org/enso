package org.enso.launcher.releases

import java.nio.file.Path

import org.enso.cli.TaskProgress

import scala.util.Try

/**
  * Represents a downloadable release asset.
  */
trait Asset {
  def fileName:               String
  def downloadTo(path: Path): TaskProgress[Unit]
  def fetchAsText():          TaskProgress[String]
}

/**
  * Wraps a generic release returned by [[ReleaseProvider]].
  */
trait Release {

  /**
    * The tag identifying this release.
    */
  def tag: String

  /**
    * The sequence of assets available in this release.
    */
  def assets: Seq[Asset]
}

/**
  * A generic release provider that allows to list and download releases.
  */
trait ReleaseProvider {

  /**
    * Finds a release for the given tag.
    */
  def releaseForTag(tag: String): Try[Release]

  /**
    * Fetches a list of all releases.
    */
  def listReleases(): Try[Seq[Release]]
}

case class ReleaseProviderException(message: String, cause: Throwable = null)
    extends RuntimeException(message, cause) {

  /**
    * @inheritdoc
    */
  override def toString: String =
    s"A problem occurred when trying to find the release: $message"
}
