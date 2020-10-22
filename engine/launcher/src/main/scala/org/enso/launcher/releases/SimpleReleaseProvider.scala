package org.enso.launcher.releases

import scala.util.Try

/** A generic release provider that allows to list and download releases.
  */
trait SimpleReleaseProvider {

  /** Finds a release for the given tag.
    */
  def releaseForTag(tag: String): Try[Release]

  /** Fetches a list of all releases.
    */
  def listReleases(): Try[Seq[Release]]
}
