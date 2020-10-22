package org.enso.launcher.locking

/** Manages locks that can be used to synchronize different launcher processes
  * running in parallel to avoid components corruption caused by simultaneous
  * modification of components.
  */
trait LockManager {

  /** Acquires the lock with the given name and type.
    *
    * Will wait if the lock cannot be acquired immediately.
    */
  def acquireLock(resourceName: String, lockType: LockType): Lock

  /** Tries to immediately acquire the lock.
    *
    * Returns immediately with None if the lock cannot be acquired without
    * waiting.
    */
  def tryAcquireLock(resourceName: String, lockType: LockType): Option[Lock]

  /** Acquires the lock with the given name and type.
    *
    * If the lock cannot be acquired immediately, it will execute the
    * `waitingAction` and start waiting to acquire the lock. The waiting action
    * may be used to notify the user that the program will have to wait and that
    * some programs must be closed to continue.
    */
  def acquireLockWithWaitingAction(
    resourceName: String,
    lockType: LockType,
    waitingAction: () => Unit
  ): Lock = {
    val immediate = tryAcquireLock(resourceName, lockType)
    immediate match {
      case Some(lock) => lock
      case None =>
        waitingAction()
        acquireLock(resourceName, lockType)
    }
  }
}
