package org.enso.runtimeversionmanager

import buildinfo.Info
import com.typesafe.scalalogging.Logger
import com.github.zafarkhaja.semver.Version

import scala.util.Try

/** Helper object that allows to get the current application version.
  *
  * In development-mode it allows to override the returned version for testing
  * purposes.
  */
object CurrentVersion {

  private var currentVersion: Version =
    Try(Version.parse(Info.ensoVersion)).getOrElse {
      throw new IllegalStateException("Cannot parse the built-in version.")
    }

  private val defaultDevEnsoVersion: Version =
    Try(Version.parse(Info.defaultDevEnsoVersion)).getOrElse {
      throw new IllegalStateException("Cannot parse the built-in dev version.")
    }

  /** Version of the component. */
  def version: Version = currentVersion

  /** Check if the current version is the development one. */
  def isDevVersion: Boolean =
    currentVersion.preReleaseVersion() == defaultDevEnsoVersion
      .preReleaseVersion()

  /** Override launcher version with the provided one.
    *
    * Internal helper method used for testing. It should be called before any
    * calls to [[version]].
    */
  def internalOverrideVersion(newVersion: Version): Unit =
    if (Info.isRelease)
      throw new IllegalStateException(
        "Internal testing function internalOverrideVersion used in a " +
        "release build."
      )
    else {
      Logger[CurrentVersion.type]
        .debug(s"Overriding version to [{}].", newVersion)
      currentVersion = newVersion
    }

}
