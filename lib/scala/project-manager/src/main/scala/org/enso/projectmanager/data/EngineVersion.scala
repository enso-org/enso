package org.enso.projectmanager.data

import com.github.zafarkhaja.semver.Version

/** Represents an engine version that is installed or available to be installed.
  *
  * @param version semver version
  * @param markedAsBroken specifies if that version is marked as broken
  */
case class EngineVersion(version: Version, markedAsBroken: Boolean)
