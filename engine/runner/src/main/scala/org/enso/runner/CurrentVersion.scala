package org.enso.runner

import org.enso.semver.SemVer

/** A helper object that allows to access current version of the runner.
  *
  * The current version is parsed from [[buildinfo]], but in development mode it
  * can be overridden by setting `enso.version.override` property. This is used
  * in project-manager tests to override the version of projects created using
  * the runner.
  */
object CurrentVersion {

  /** The version that the application should report. */
  lazy val version: SemVer = computeVersion()

  private def computeVersion(): SemVer = {
    val buildVersion =
      SemVer.parse(buildinfo.Info.ensoVersion).getOrElse {
        throw new IllegalStateException(
          "Fatal error: Enso version included in buildinfo is not a valid " +
          "semver string, this should never happen."
        )
      }
    if (buildinfo.Info.isRelease) buildVersion
    else
      sys.props
        .get("enso.version.override")
        .flatMap(v => SemVer.parse(v).toOption)
        .getOrElse(buildVersion)
  }
}
