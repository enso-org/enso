package org.enso.runtimeversionmanager.releases

import com.github.zafarkhaja.semver

import scala.util.Try

/** A high-level release provider that includes release metadata.
  * @tparam ReleaseType type of a specific component's release, containing any
  *                     necessary metadata
  */
trait ReleaseProvider[ReleaseType] {

  /** Returns the version of the most recent release.
    *
    * It ignores releases marked as broken, so the latest non-broken release is
    * returned.
    */
  def findLatestVersion(): Try[semver.Version]

  /** Returns sequence of all available versions (including ones marked as
    * broken).
    *
    * The sequence does not have to be sorted.
    */
  def fetchAllVersions(): Try[Seq[ReleaseProvider.Version]]

  /** Returns sequence of available non-broken versions.
    *
    * The sequence does not have to be sorted.
    */
  def fetchAllValidVersions(): Try[Seq[semver.Version]]

  /** Fetch release metadata for the given version.
    */
  def fetchRelease(version: semver.Version): Try[ReleaseType]
}

object ReleaseProvider {

  val TagPrefix = "enso-"

  case class Version(version: semver.Version, markedAsBroken: Boolean)
}
