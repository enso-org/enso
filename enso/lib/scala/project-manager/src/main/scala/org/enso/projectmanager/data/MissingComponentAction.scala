package org.enso.projectmanager.data

import enumeratum._

/** Specifies how to handle missing components. */
sealed trait MissingComponentAction extends EnumEntry
object MissingComponentAction
    extends Enum[MissingComponentAction]
    with CirceEnum[MissingComponentAction] {

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

  override val values = findValues
}
