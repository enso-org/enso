package org.enso.runtimeversionmanager.locking

/** Defines the lock type.
  */
sealed trait LockType
object LockType {

  /** An exclusive lock can be held by only one user at a time.
    */
  case object Exclusive extends LockType

  /** A shared lock may be held by multiple users in a given moment.
    */
  case object Shared extends LockType
}
