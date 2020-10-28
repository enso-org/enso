package org.enso.projectmanager.data

/** Specifies how to handle missing components. */
sealed trait MissingComponentAction
object MissingComponentAction {

  /** Specifies that an action requiring a missing component should fail. */
  case object Fail extends MissingComponentAction

  /** Specifies that an action requiring a missing component should install it,
    * unless it is broken.
    */
  case object Install extends MissingComponentAction

  /** Specifies that an action requiring a missing component should forcibly
    * install it, even if it is broken.
    */
  case object ForceInstallBroken extends MissingComponentAction
}
