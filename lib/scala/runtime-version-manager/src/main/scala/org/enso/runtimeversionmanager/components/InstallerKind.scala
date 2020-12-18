package org.enso.runtimeversionmanager.components

/** Describes the kind of installer that can use the `runtime-version-manager`.
  *
  * Currently there are two installers: the launcher and the project manager.
  * This [[InstallerKind]] is used to distinguish them as a given version of the
  * `runtime-version-manager` may be related to different versions of these
  * installers.
  */
sealed trait InstallerKind
object InstallerKind {

  /** The [[InstallerKind]] representing the launcher. */
  case object Launcher extends InstallerKind

  /** The [[InstallerKind]] representing the project manager. */
  case object ProjectManager extends InstallerKind
}
