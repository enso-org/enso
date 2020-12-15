package org.enso.runtimeversionmanager.components

sealed trait InstallerKind
object InstallerKind {
  case object Launcher       extends InstallerKind
  case object ProjectManager extends InstallerKind
}
