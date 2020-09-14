package org.enso.launcher.locking

import scala.util.Using.Releasable

trait Lock {
  def release(): Unit
}

object Lock {
  implicit val releasable: Releasable[Lock] = (resource: Lock) =>
    resource.release()
}
