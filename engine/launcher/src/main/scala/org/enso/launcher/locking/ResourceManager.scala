package org.enso.launcher.locking

import org.enso.launcher.Logger

import scala.util.Using

class ResourceManager(lockManager: LockManager) {
  def withResource[R](resource: Resource, lockType: LockType)(
    action: => R
  ): R =
    Using {
      lockManager.acquireLockWithWaitingAction(
        resource.name,
        lockType = lockType,
        () => Logger.warn(resource.waitMessage)
      )
    } { _ => action }.get

  def withResources[R](resources: (Resource, LockType)*)(action: => R): R =
    resources match {
      case Seq((head, exclusive), tail @ _*) =>
        withResource(head, exclusive) { withResources(tail: _*)(action) }
      case Seq() =>
        action
    }

  private case object MainResource extends Resource {
    override def name: String = "launcher-main"
    override def waitMessage: String =
      "Another process is installing or uninstalling the current " +
      "distribution. Please wait until that finishes."
  }

  private var mainLock: Option[Lock] = None

  def initializeMainLock(): Unit = {
    val lock =
      lockManager.tryAcquireLock(MainResource.name, LockType.Shared).getOrElse {
        Logger.error(MainResource.waitMessage)
        sys.exit(1)
      }
    mainLock = Some(lock)
  }

  def acquireExclusiveMainLock(waitAction: () => Unit): Unit = {
    mainLock match {
      case Some(lock) =>
        lock.release()
        mainLock = None
      case None =>
    }

    val lock = lockManager.acquireLockWithWaitingAction(
      MainResource.name,
      LockType.Exclusive,
      waitAction
    )
    mainLock = Some(lock)
  }

  def releaseMainLock(): Unit =
    mainLock match {
      case Some(lock) =>
        lock.release()
        mainLock = None
      case None =>
    }
}
