package org.enso.launcher.locking

sealed trait LockType
object LockType {
  case object Exclusive extends LockType
  case object Shared    extends LockType
}
