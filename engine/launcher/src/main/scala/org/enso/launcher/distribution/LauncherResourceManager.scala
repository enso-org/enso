package org.enso.launcher.distribution

import org.enso.distribution.locking._

/** Adds additional capabilities to the [[ResourceManager]], focused on
  * synchronizing launcher instances.
  */
class LauncherResourceManager(lockManager: LockManager)
    extends ResourceManager(lockManager) {
  private var mainLock: Option[Lock] = None

  /** Initializes the [[MainLock]]. */
  def initializeMainLock(): Unit = {
    val lock =
      lockManager
        .tryAcquireLock(MainLock.name, LockType.Shared)
        .getOrElse {
          throw DistributionIsModifiedError(MainLock.waitMessage)
        }
    mainLock = Some(lock)
  }

  /** Exception that is thrown when the main lock is held exclusively.
    *
    * This situation means that the current distribution is being installed or
    * uninstalled, so it should not be used in the meantime and the application
    * has to terminate immediately.
    */
  case class DistributionIsModifiedError(message: String)
      extends RuntimeException(message)

  /** Acquires an exclusive main lock (first releasing the shared lock),
    * ensuring that no other processes using this distribution can be running in
    * parallel.
    *
    * @param waitAction function that is executed if the lock cannot be acquired
    *                   immediately
    */
  def acquireExclusiveMainLock(waitAction: () => Unit): Unit = {
    mainLock match {
      case Some(oldLock) =>
        oldLock.release()
        mainLock = None
      case None =>
    }

    val lock = lockManager.acquireLockWithWaitingAction(
      MainLock.name,
      LockType.Exclusive,
      waitAction
    )
    mainLock = Some(lock)
  }

  /** Releases the main lock.
    *
    * Should be called just before the program terminates. It is not an error to
    * skip it, as the operating system should unlock all resources after the
    * program terminates, but on some platforms this automatic 'garbage
    * collection for locks' may take some time, so it is better to release it
    * manually.
    */
  def releaseMainLock(): Unit =
    mainLock match {
      case Some(lock) =>
        lock.release()
        mainLock = None
      case None =>
    }

  /** The main lock that is held by all launcher processes.
    *
    * It is used to ensure that no other processes are running when the
    * distribution is being installed or uninstalled.
    */
  private case object MainLock extends Resource {
    override def name: String = "launcher-main"
    override def waitMessage: String =
      "Another process is installing or uninstalling the current " +
      "distribution. Please wait until that finishes."
  }
}
