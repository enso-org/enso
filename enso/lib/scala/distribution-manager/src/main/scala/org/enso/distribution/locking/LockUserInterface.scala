package org.enso.distribution.locking

/** Defines callbacks that can be called when a requested resource is locked and
  * the application has to wait for other processes.
  *
  * These callbacks can be used to explain to the user why the current action is
  * stalling and, for example, ask the user to close other Enso programs to
  * continue.
  */
trait LockUserInterface {

  /** Method that is called whenever a resource cannot be acquired immediately
    * and will require a blocking wait.
    */
  def startWaitingForResource(resource: Resource): Unit

  /** Method that is called if [[startWaitingForResource]] was called, after the
    * resource in question has been acquired.
    *
    * Every call to [[finishWaitingForResource]] corresponds to a call to an
    * earlier [[startWaitingForResource]].
    */
  def finishWaitingForResource(resource: Resource): Unit
}
