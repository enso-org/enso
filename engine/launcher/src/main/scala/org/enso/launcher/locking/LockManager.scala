package org.enso.launcher.locking

/**
  * TODO [RW]
  */
trait LockManager {

  /**
    * TODO [RW]
    */
  def acquireLock(resourceName: String, lockType: LockType): Lock

  /**
    * TODO [RW]
    */
  def tryAcquireLock(resourceName: String, lockType: LockType): Option[Lock]

  /**
    * TODO [RW]
    */
  def acquireLockWithWaitingAction(
    resourceName: String,
    lockType: LockType,
    waiting: () => Unit
  ): Lock = {
    val immediate = tryAcquireLock(resourceName, lockType)
    immediate match {
      case Some(lock) => lock
      case None =>
        waiting()
        acquireLock(resourceName, lockType)
    }
  }
}
