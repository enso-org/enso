package org.enso.launcher.locking

import org.enso.launcher.Logger

import scala.util.Using

class ResourceManager(lockManager: LockManager) {
  def withResource[R](resource: Resource, exclusive: Boolean)(
    action: => R
  ): R =
    Using {
      lockManager.acquireLockWithWaitingAction(
        resource.name,
        shared = !exclusive,
        () => Logger.warn(resource.waitMessage)
      )
    } { _ => action }.get
}
