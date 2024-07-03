package org.enso.editions

import org.enso.semver.SemVer

/** Represents the engine version that is associated with a project.
  */
sealed trait EnsoVersion

/** Represents `default` Enso version.
  *
  * If `enso-version` is set to `default`, the locally default Enso engine
  * version is used for the project.
  */
case object DefaultEnsoVersion extends EnsoVersion {
  private val defaultEnsoVersion = "default"

  /** @inheritdoc
    */
  override def toString: String = defaultEnsoVersion
}

/** An exact semantic versioning string.
  */
case class SemVerEnsoVersion(version: SemVer) extends EnsoVersion {

  /** @inheritdoc
    */
  override def toString: String = version.toString
}
