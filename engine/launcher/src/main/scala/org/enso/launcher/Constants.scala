package org.enso.launcher

import com.github.zafarkhaja.semver.Version

object Constants {

  /** The engine version in which the uploads command has been introduced.
    *
    * It is used to check by the launcher if the engine can handle this command
    * and provide better error messages if it cannot.
    */
  val uploadIntroducedVersion: Version =
    Version.of(0, 2, 17, "SNAPSHOT")

  /** The engine version in which the dependency preinstall command has been
    * introduced.
    *
    * It is used to check by the launcher if the engine can handle this command
    * and provide better error messages if it cannot.
    */
  val preinstallDependenciesIntroducedVersion: Version =
    Version.of(0, 2, 28, "SNAPSHOT")

  /** The upload URL associated with the main Enso library repository. */
  val defaultUploadUrl = "https://publish.libraries.release.enso.org/"
}
