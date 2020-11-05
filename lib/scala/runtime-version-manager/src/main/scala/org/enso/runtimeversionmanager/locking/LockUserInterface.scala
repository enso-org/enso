package org.enso.runtimeversionmanager.locking

trait LockUserInterface {

  /** Method that is called whenever a resource cannot be acquired immediately
    * and will require a blocking wait.
    */
  def startWaitingForResource(resource: Resource): Unit

  /** Method that is called if [[startWaitingForResource]] was called, after the
    * resource in question has been acquired.
    */
  def finishWaitingForResource(resource: Resource): Unit
}
