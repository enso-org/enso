package org.enso.projectmanager.data

/** Specifies how to handle missing components. */
sealed trait MissingComponentAction
object MissingComponentAction {
  case object Fail               extends MissingComponentAction
  case object Install            extends MissingComponentAction
  case object ForceInstallBroken extends MissingComponentAction
}
