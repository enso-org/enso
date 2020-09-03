package org.enso.launcher.releases

/**
  * Wraps a generic release identified by a tag and containing a sequence of
  * assets.
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

  /**
    * Checks if the given release is marked as broken.
    */
  def isMarkedBroken: Boolean = assets.exists(_.fileName == "broken")
}
