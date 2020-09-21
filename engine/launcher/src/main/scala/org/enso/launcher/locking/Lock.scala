package org.enso.launcher.locking

import scala.util.Using.Releasable

/**
  * A lock synchronizing access to some resource.
  */
trait Lock {

  /**
    * Releases the lock and any resources (file channels etc.) that are
    * associated with it.
    */
  def release(): Unit
}

object Lock {

  /**
    * [[Releasable]] instance for [[Lock]] making it usable with the `Using`
    * construct.
    */
  implicit val releasable: Releasable[Lock] = (resource: Lock) =>
    resource.release()
}
