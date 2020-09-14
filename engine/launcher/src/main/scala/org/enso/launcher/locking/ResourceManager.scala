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

//  def lock(resource: Resource, exclusive: Boolean): Lock =
//    lockManager.acquireLockWithWaitingAction(
//      resource.name,
//      shared = !exclusive,
//      () => Logger.warn(resource.waitMessage)
//    )
//
//  def tryLock(resource: Resource, exclusive: Boolean): Option[Lock] =
//    lockManager.tryAcquireLock(resource.name, shared = !exclusive)
}
