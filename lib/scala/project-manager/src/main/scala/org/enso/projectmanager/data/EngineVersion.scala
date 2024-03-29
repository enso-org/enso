package org.enso.projectmanager.data

import org.enso.semver.SemVer

/** Represents an engine version that is installed or available to be installed.
  *
  * @param version semver version
  * @param markedAsBroken specifies if that version is marked as broken
  */
case class EngineVersion(version: SemVer, markedAsBroken: Boolean)
