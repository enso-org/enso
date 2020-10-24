package org.enso.projectmanager.model

import enumeratum._

/** Enum that distinguishes between different kinds of projects.
  */
sealed trait ProjectKind extends EnumEntry

object ProjectKind extends Enum[ProjectKind] with CirceEnum[ProjectKind] {

  /** Enum value for user projects.
    */
  case object UserProject extends ProjectKind

  override def values = findValues

}
