package org.enso.launcher.locking

/**
  * TODO [RW]
  */
trait LockManager {

  /**
    * TODO [RW]
    */
  def acquireLock(resourceName: String, shared: Boolean): Lock

  /**
    * TODO [RW]
    */
  def tryAcquireLock(resourceName: String, shared: Boolean): Option[Lock]

  /**
    * TODO [RW]
    */
  def acquireLockWithWaitingAction(
    resourceName: String,
    shared: Boolean,
    waiting: () => Unit
  ): Lock = {
    val immediate = tryAcquireLock(resourceName, shared)
    immediate match {
      case Some(lock) => lock
      case None =>
        waiting()
        acquireLock(resourceName, shared)
    }
  }
}
