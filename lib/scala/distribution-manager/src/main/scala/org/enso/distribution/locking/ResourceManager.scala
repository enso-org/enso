package org.enso.distribution.locking

import com.typesafe.scalalogging.Logger

import scala.util.Using

/** Uses a [[LockManager]] implementation to synchronize access to [[Resource]].
  */
class ResourceManager(lockManager: LockManager) {

  private val logger = Logger[ResourceManager]

  /** Runs the `action` while holding a lock (of `lockType`) for the `resource`.
    *
    * If the lock cannot be acquired immediately, the `waitingInterface` is used
    * to notify the user.
    */
  def withResource[R](
    waitingInterface: LockUserInterface,
    resource: Resource,
    lockType: LockType
  )(action: => R): R = {
    Using(acquireResource(waitingInterface, resource, lockType)) { _ =>
      action
    }.get
  }

  /** Runs the `action` while holding multiple locks for a sequence of
    * resources.
    */
  def withResources[R](
    waitingInterface: LockUserInterface,
    resources: (Resource, LockType)*
  )(action: => R): R =
    resources match {
      case Seq((head, exclusive), tail @ _*) =>
        withResource(waitingInterface, head, exclusive) {
          withResources(waitingInterface, tail: _*)(action)
        }
      case Seq() =>
        action
    }

  /** Acquires a resource, handling possible waiting, and returns its [[Lock]]
    * instance that can be used to unlock it.
    */
  private def acquireResource(
    waitingInterface: LockUserInterface,
    resource: Resource,
    lockType: LockType
  ): Lock = {
    var waited = false
    val lock = lockManager.acquireLockWithWaitingAction(
      resource.name,
      lockType = lockType,
      () => {
        waited = true
        waitingInterface.startWaitingForResource(resource)
      }
    )
    if (waited) waitingInterface.finishWaitingForResource(resource)
    lock
  }

  /** Runs the provided `action` if an exclusive lock can be immediately
    * acquired for the temporary directory, i.e. the directory is not used by
    * anyone.
    *
    * If the lock cannot be acquired immediately, the action is not executed and
    * None is returned.
    */
  def tryWithExclusiveTemporaryDirectory[R](action: => R): Option[R] = {
    lockManager.tryAcquireLock(
      TemporaryDirectory.name,
      LockType.Exclusive
    ) match {
      case Some(lock) =>
        try Some(action)
        finally lock.release()
      case None => None
    }
  }

  private var temporaryDirectoryLock: Option[Lock] = None

  /** Marks the temporary directory as in use.
    *
    * This lock does not have to be released as it will be automatically
    * released when the program terminates.
    */
  def startUsingTemporaryDirectory(): Unit = {
    if (temporaryDirectoryLock.isDefined) {
      logger.trace("The temporary directory was already in-use.")
      return
    }

    val lock = lockManager.acquireLockWithWaitingAction(
      TemporaryDirectory.name,
      LockType.Shared,
      () => logger.warn(TemporaryDirectory.waitMessage)
    )
    temporaryDirectoryLock = Some(lock)
  }

  /** Releases the lock for the temporary directory.
    *
    * Used by the uninstaller as part of releasing all locks to ensure that the
    * locks directory can be removed.
    */
  def unlockTemporaryDirectory(): Unit =
    temporaryDirectoryLock match {
      case Some(lock) =>
        lock.release()
        temporaryDirectoryLock = None
      case None =>
    }

  /** This resource is acquired whenever the temporary directory is first
    * accessed.
    *
    * It ensures that installations running in parallel will not remove each
    * other's files. The temporary directory is only cleaned when no other
    * processes use it.
    */
  private case object TemporaryDirectory extends Resource {
    override def name: String = "temporary-files"
    override def waitMessage: String =
      "Another process is cleaning temporary files, " +
      "the installation has to wait until that is complete to avoid " +
      "conflicts. It should not take a long time."
  }
}
