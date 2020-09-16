package org.enso.launcher.locking

import org.enso.launcher.Logger

import scala.util.Using

class ResourceManager(lockManager: LockManager) {
  def withResource[R](
    resource: Resource,
    lockType: LockType,
    waitingAction: Option[Resource => Unit] = None
  )(
    action: => R
  ): R =
    Using {
      lockManager.acquireLockWithWaitingAction(
        resource.name,
        lockType = lockType,
        () =>
          waitingAction
            .map(_.apply(resource))
            .getOrElse(Logger.warn(resource.waitMessage))
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

  var mainLock: Option[Lock] = None

  def initializeMainLock(): Unit = {
    val lock =
      lockManager.tryAcquireLock(MainResource.name, LockType.Shared).getOrElse {
        throw new RuntimeException(MainResource.waitMessage) // TODO custom exc
      }
    mainLock = Some(lock)
  }

  def acquireExclusiveMainLock(waitAction: () => Unit): Unit = {
    mainLock match {
      case Some(oldLock) =>
        oldLock.release()
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

  private case object TemporaryDirectory extends Resource {
    override def name: String = "temporary-files"
    override def waitMessage: String =
      "Another process is cleaning temporary files, " +
      "the installation has to wait until that is complete to avoid " +
      "conflicts. It should not take a long time."
  }

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

  def startUsingTemporaryDirectory(): Unit = {
    lockManager.acquireLockWithWaitingAction(
      TemporaryDirectory.name,
      LockType.Shared,
      () => Logger.warn(TemporaryDirectory.waitMessage)
    )
  }
}
